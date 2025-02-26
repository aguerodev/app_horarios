box::use(
  stringr[str_extract, str_trim, str_replace, str_split],
  dplyr[filter, collect, mutate, select],
  tidyr[separate_longer_delim, unnest, fill],
  purrr[map_dbl]
)

redondear_hora <- function(hora_minuto) {
  if (is.na(hora_minuto)) return(NA_real_)

  # Capturar errores en la función completa
  tryCatch(
    {
      # Extraer el indicador AM/PM (si lo hay) y convertirlo a mayúsculas
      period <- toupper(str_extract(hora_minuto, "(AM|PM)"))

      # Eliminar el indicador de la cadena para obtener solo "HH:MM"
      hora_minuto_clean <- str_trim(str_replace(
        hora_minuto,
        "(?i)\\s*(AM|PM)",
        ""
      ))

      partes <- strsplit(hora_minuto_clean, ":", fixed = TRUE)[[1]]
      if (length(partes) < 2) return(NA_real_)

      hora <- as.numeric(partes[1])
      minutos <- as.numeric(substr(partes[2], 1, 2))

      # Conversión a formato 24 horas si se detecta AM/PM
      if (!is.na(period)) {
        if (period == "PM" && hora < 12) hora <- hora + 12
        if (period == "AM" && hora == 12) hora <- 0
      }

      # Redondear la hora: si los minutos son 50 o más se suma 1 a la hora
      if (minutos >= 50) hora <- hora + 1

      # Manejo de medianoche
      if (hora >= 24) hora <- 0

      return(hora)
    },
    error = function(e) NA_real_
  )
}

#' @export
preprocess_data <- function(con, values) {
  # Usar directamente dplyr::tbl
  dplyr::tbl(con, "temp_horario") |>
    filter(uid %in% values) |>
    collect() |>
    # Separar la cadena de horarios en segmentos usando "-" como separador
    separate_longer_delim(horario, delim = "-") |>
    # Procesamiento más eficiente de datos
    mutate(
      segments = str_split(horario, ","),
      horario = NULL # Eliminar columna ya no necesaria
    ) |>
    unnest(segments) |>
    mutate(
      segments = str_trim(segments),
      # Extraer datos en un solo paso de mutate
      dias = str_extract(segments, "^[\\p{L},]+"),
      inicio_str = str_extract(segments, "\\d{1,2}:\\d{2} \\w{2}"),
      fin_str = str_extract(segments, "(?<=A )\\d{1,2}:\\d{2} \\w{2}"),
      inicio = map_dbl(inicio_str, redondear_hora),
      fin = map_dbl(fin_str, redondear_hora),
      # Eliminar columnas intermedias
      segments = NULL,
      inicio_str = NULL,
      fin_str = NULL
    ) |>
    select(codigo, seccion = grupo, dias, inicio, fin) |>
    fill(inicio, fin, .direction = "up")
}
