box::use(
  dplyr[
    group_by,
    ungroup,
    summarise,
    mutate,
    bind_rows,
    filter,
    arrange,
    select,
    rename
  ],
  tidyr[unnest, nest],
  tibble[tibble],
  stringr[str_split],
  GA[ga],
  mirai[mirai]
)

#' @export
calculate_ga <- function(...) {
  mirai(
    {
      if (nrow(cursos) == 0) return(data.frame())
      candidatos_por_curso <- cursos |>
        group_by(codigo, seccion) |>
        nest() |>
        ungroup() |>
        group_by(codigo) |>
        summarise(
          candidates = list(tibble(seccion = seccion, data = data)),
          .groups = "drop"
        )

      n_courses <- nrow(candidatos_por_curso)
      if (n_courses == 0) return(NULL)
      lower_bounds <- rep(1, n_courses)
      upper_bounds <- sapply(candidatos_por_curso$candidates, nrow)

      fitness_function <- function(x) {
        # Convertir cada componente a un índice entero (dentro de los límites permitidos)
        sel <- round(x)
        sel <- pmax(1, pmin(sel, upper_bounds))

        # Reconstruir el horario combinando los segmentos elegidos para cada curso
        schedule_list <- vector("list", n_courses)
        for (i in 1:n_courses) {
          cand <- candidatos_por_curso$candidates[[i]]
          chosen_idx <- sel[i]
          chosen_candidate <- cand[chosen_idx, ]
          if (nrow(chosen_candidate$data[[1]]) == 0) next
          seg <- chosen_candidate$data[[1]] %>%
            mutate(
              codigo = candidatos_por_curso$codigo[i],
              seccion = chosen_candidate$seccion
            )
          schedule_list[[i]] <- seg
        }
        schedule <- bind_rows(schedule_list)

        # Penalización muy fuerte si no se seleccionaron todos los cursos
        if (nrow(schedule) == 0) return(-1e6)
        if (length(unique(schedule$codigo)) < n_courses) return(-1e6)

        ## 1. Penalización por conflictos entre cursos
        conflicts <- 0
        n_seg <- nrow(schedule)
        for (i in 1:(n_seg - 1)) {
          for (j in (i + 1):n_seg) {
            if (schedule$codigo[i] == schedule$codigo[j]) next
            dias_i <- unlist(str_split(schedule$dias[i], ",\\s*"))
            dias_j <- unlist(str_split(schedule$dias[j], ",\\s*"))
            common_days <- intersect(dias_i, dias_j)
            if (length(common_days) > 0) {
              if (
                !(schedule$fin[i] <= schedule$inicio[j] ||
                  schedule$inicio[i] >= schedule$fin[j])
              ) {
                conflicts <- conflicts + 1
              }
            }
          }
        }

        ## 2. Expandir el horario para tener cada ocurrencia por día
        schedule_expanded <- schedule %>%
          mutate(dia_list = str_split(dias, ",\\s*")) %>%
          unnest(dia_list) %>%
          rename(dia = dia_list)

        # Mapear los días a números (Lunes=1, Martes=2, Miércoles=3, Jueves=4, Viernes=5)
        day_map <- c(
          "Lunes" = 1,
          "Martes" = 2,
          "Miércoles" = 3,
          "Jueves" = 4,
          "Viernes" = 5
        )
        schedule_expanded <- schedule_expanded %>%
          mutate(day_num = day_map[dia])

        ## 3. Calcular el bloque total de horas en campus (por día)
        city_hours_by_day <- schedule_expanded %>%
          group_by(day_num) %>%
          summarise(day_hours = max(fin) - min(inicio))
        total_city_hours <- sum(city_hours_by_day$day_hours)

        ## 4. Penalización por tiempo ocioso (idle time) dentro de cada día
        idle_penalty_total <- 0
        dias <- unique(schedule_expanded$day_num)
        for (day in dias) {
          day_schedule <- schedule_expanded %>%
            filter(day_num == day) %>%
            arrange(inicio)
          if (nrow(day_schedule) > 1) {
            gaps <- diff(day_schedule$inicio) -
              (day_schedule$fin[-nrow(day_schedule)] -
                day_schedule$inicio[-nrow(day_schedule)])
            idle_penalty_total <- idle_penalty_total + sum(pmax(0, gaps))
          }
        }

        ## 5. Penalización por cantidad de días utilizados y días no consecutivos
        unique_days <- sort(unique(schedule_expanded$day_num))
        n_days <- length(unique_days)
        gap_penalty <- (max(unique_days) - min(unique_days) + 1 - n_days)

        ## 6. Aplicar penalizaciones con pesos reforzados para concentrar los días
        penalty_conflicts <- conflicts * 2500
        penalty_hours <- total_city_hours * 200
        penalty_days <- n_days * 5000 # Penalización alta por cada día utilizado
        penalty_gap <- gap_penalty * 1000 # Penalización fuerte si los días no son consecutivos
        penalty_idle <- idle_penalty_total * 150

        fitness_value <- n_courses *
          1000 -
          penalty_conflicts -
          penalty_hours -
          penalty_days -
          penalty_gap -
          penalty_idle

        return(fitness_value)
      }

      ga_result <- ga(
        type = "real-valued",
        fitness = fitness_function,
        lower = lower_bounds,
        upper = upper_bounds,
        popSize = 200, # Tamaño de población
        maxiter = 50, # Máximo número de iteraciones
        run = 5, # Criterio de parada: 5 iteraciones sin mejora
        pmutation = 0.1, # Tasa de mutación
        pcrossover = 0.8, # Probabilidad de cruce
        elitism = 30, # Número de individuos elitistas
        monitor = FALSE
      )
      best_solution <- round(ga_result@solution[1, ])
      schedule_list <- vector("list", n_courses)
      for (i in 1:n_courses) {
        cand <- candidatos_por_curso$candidates[[i]]
        chosen_idx <- best_solution[i]
        chosen_candidate <- cand[chosen_idx, ]
        seg <- chosen_candidate$data[[1]] %>%
          mutate(
            codigo = candidatos_por_curso$codigo[i],
            seccion = chosen_candidate$seccion
          )
        schedule_list[[i]] <- seg
      }
      optimal_schedule <- bind_rows(schedule_list)

      return(optimal_schedule)
    },
    ...
  )
}
