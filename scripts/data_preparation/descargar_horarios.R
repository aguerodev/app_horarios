library(pdftools)
library(stringr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(DBI)
library(duckdb)

# Función auxiliar para procesar el bloque de horarios (se esperan 2 líneas por grupo)
process_horario <- function(horario_block) {
  if (is.na(horario_block) || horario_block == "") {
    return(list(horario = NA, aula = NA))
  }

  # Separar líneas (compatible con Windows y Unix)
  lines <- str_trim(unlist(str_split(horario_block, "\\r?\\n")))
  lines <- lines[lines != ""]

  # Extraer de cada línea: día, horario y aula.
  parsed <- lapply(lines, function(line) {
    m <- str_match(line, "^\\s*([A-Z,]+)\\s+(\\d{1,2}:\\d{2}\\s*a\\s*\\d{1,2}:\\d{2})\\s+(.+)$")
    if (is.na(m[1])) return(NULL)
    list(days = m[2], time = m[3], aula = m[4])
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (length(parsed) == 0) return(list(horario = NA, aula = NA))

  # Si se detectan exactamente 2 líneas se unen separadas por "; "
  if (length(parsed) == 2) {
    horario_out <- paste(sapply(parsed, function(x) paste0(x$days, " ", x$time)), collapse = "; ")
    aulas <- sapply(parsed, function(x) x$aula)
    aula_out <- if (length(unique(aulas)) == 1) aulas[1] else paste(aulas, collapse = "; ")
    return(list(horario = horario_out, aula = aula_out))
  }

  # Fallback: unir todas las líneas encontradas
  horario_out <- paste(sapply(parsed, function(x) paste0(x$days, " ", x$time)), collapse = "; ")
  aulas <- sapply(parsed, function(x) x$aula)
  aula_out <- if (length(unique(aulas)) == 1) aulas[1] else paste(aulas, collapse = "; ")
  return(list(horario = horario_out, aula = aula_out))
}

# Función principal para extraer la información de los cursos
extraer_info_cursos <- function(texto) {
  # Dividir el texto en bloques de curso (cada uno empieza con "Recinto:")
  bloques <- unlist(str_split(texto, "(?=\\nRecinto:)"))

  res <- lapply(bloques, function(bloque) {
    # Datos generales del curso
    recinto   <- str_match(bloque, "Recinto:\\s*([^\\n]+)")[,2]
    area      <- str_match(bloque, "Área:\\s*([^\\n]+)\\.")[,2]
    facultad  <- str_match(bloque, "Facultad:\\s*([^\\n]+)\\.")[,2]
    escuela   <- str_match(bloque, "Escuela:\\s*([^\\n]+)")[,2]

    sigla_match  <- str_match(bloque, "Sigla:\\s*(\\w+)\\s*-\\s*([^\\n]+)")
    requisitos   <- str_match(bloque, "Requisitos:\\s*([^\\n]+)")[,2]
    correquisitos <- str_match(bloque, "Correquisitos:\\s*([^\\n]+)")[,2]
    tipo_curso   <- str_match(bloque, "Tipo curso:\\s*([^\\n]+)")[,2]
    creditos     <- str_match(bloque, "Créditos:\\s*([^\\n]+)")[,2]

    # Patrón para capturar cada grupo individualmente.
    patron_grupo <- "(?s)Grupo:\\s*(\\d+)\\.\\s*Modalidad:\\s*([^\\n]+)\\s*Profesor:\\s*([^\\n]*?)\\s*D[ií]a y Hora\\s*Aula y Edificio\\s*(.*?)(?=\\n\\s*Grupo:|\\n\\s*Recinto:|$)"

    grupos_matches <- str_match_all(bloque, patron_grupo)[[1]]
    if(nrow(grupos_matches) == 0) return(NULL)

    grupos_df <- map_df(1:nrow(grupos_matches), function(i) {
      grupo_num     <- str_trim(grupos_matches[i, 2])
      modalidad     <- str_trim(grupos_matches[i, 3])
      profesor      <- str_trim(grupos_matches[i, 4])
      horario_block <- str_trim(grupos_matches[i, 5])

      sch <- process_horario(horario_block)

      data.frame(
        escuela      = escuela,
        # Se limpia la cadena y se extrae el código y nombre del curso
        codigo       = str_replace(sigla_match[1, 1], "Sigla:\\s*", "") |> str_trim(),
        sigla        = sigla_match[1, 1],
        nombre_curso = str_trim(sigla_match[1, 2]),
        requisitos   = requisitos,
        correquisitos = correquisitos,
        tipo_curso   = tipo_curso,
        creditos     = creditos,
        grupo        = grupo_num,
        modalidad    = modalidad,
        profesor     = profesor,
        horario      = sch$horario,
        aula         = sch$aula,
        stringsAsFactors = FALSE
      )
    })
    grupos_df
  })

  bind_rows(res)
}

# Procesar todas las páginas del PDF y combinar resultados
resultado_final <- pdftools::pdf_text("scripts/horarios.pdf") |>
  paste0(collapse = "") |>
  lapply(extraer_info_cursos) |>
  bind_rows() |>
  mutate(
    nombre_curso = str_remove(nombre_curso, "\\s*\\([^)]*\\)"),
    tipo_curso = str_remove(tipo_curso, "\\s*\\([^)]*\\)"),
    escuela = str_remove(escuela, ":.*")
  ) |>
  fill(escuela, codigo, nombre_curso, requisitos, correquisitos, tipo_curso, creditos)

df <- resultado_final

# Mapeo de abreviaturas a nombres completos de días
mapeo_dias <- c(
  "L" = "Lunes",
  "K" = "Martes",
  "M" = "Miércoles",
  "J" = "Jueves",
  "V" = "Viernes",
  "S" = "Sábado",
  "D" = "Domingo"
)

# Función para redondear la hora (manejo de errores incluido)
redondear_hora <- function(hora_minuto) {
  tryCatch({
    if (is.na(hora_minuto)) return(NA)
    partes <- strsplit(hora_minuto, ":", fixed = TRUE)[[1]]
    if (length(partes) < 2) return(NA)
    hora <- as.numeric(partes[1])
    minutos <- as.numeric(substr(partes[2], 1, 2))
    if (minutos >= 50) hora <- hora + 1
    if (hora >= 24) hora <- 0  # Manejo de medianoche
    hora
  }, error = function(e) NA)
}

df_procesado2 <- df |>
  # Separar la cadena de horarios en segmentos usando ";" como separador
  mutate(segments = str_split(horario, ";")) |>
  unnest(segments) |>
  mutate(
    segments = str_trim(segments),
    # Extraer la abreviatura del día (asumido al inicio del segmento)
    dia_abrev = str_extract(segments, "^[A-Za-z,]+"),
    # Convertir las abreviaturas a nombres completos
    dias = map_chr(dia_abrev, ~ {
      if (is.na(.x)) return(NA)
      dias_abr <- str_split(.x, ",\\s*")[[1]]
      dias_completos <- recode(dias_abr, !!!mapeo_dias)
      paste(na.omit(dias_completos), collapse = ", ")
    }),
    # Extraer la hora de inicio y fin
    inicio_str = str_extract(segments, "\\d{1,2}:\\d{2}"),
    fin_str = str_extract(segments, "(?<=a )\\d{1,2}:\\d{2}"),
    inicio = map_dbl(inicio_str, redondear_hora),
    fin = map_dbl(fin_str, redondear_hora)
  ) |>
  select(codigo, grupo, requisitos, correquisitos, creditos, dias, inicio, fin) |>
  filter(!is.na(inicio) & !is.na(fin)) |>
  # Separar la variable 'codigo' en dos columnas: 'codigo' y 'curso'
  separate(col = codigo, into = c("codigo", "curso"), sep = " - ", extra = "merge", fill = "right") |>
  separate_longer_delim(cols = dias, delim = ",") |>
  mutate(across(where(is.character), str_trim))

# Función vectorizada para formatear la hora de 24h a 12h con formato "h:00 AM/PM"
format_hour <- function(h) {
  sapply(h, function(x) {
    if (x == 0) {
      hour12 <- 12
      suffix <- "AM"
    } else if (x < 12) {
      hour12 <- x
      suffix <- "AM"
    } else if (x == 12) {
      hour12 <- 12
      suffix <- "PM"
    } else {
      hour12 <- x - 12
      suffix <- "PM"
    }
    sprintf("%d:00 %s", hour12, suffix)
  })
}

df_resumen <- df_procesado2 %>%
  group_by(codigo, curso, grupo, requisitos, correquisitos, creditos) %>%
  summarise(
    horario = if (n_distinct(inicio) == 1 & n_distinct(fin) == 1) {
      paste0(
        paste(dias, collapse = ", "),
        " ",
        format_hour(first(inicio)),
        " A ",
        format_hour(first(fin))
      )
    } else {
      paste0(dias, " ", format_hour(inicio), " A ", format_hour(fin)) %>%
        paste(collapse = " - ")
    },
    .groups = "drop"
  )

# Guardar el resultado en una base de datos DuckDB
con <- dbConnect(duckdb::duckdb(), dbdir = "horarios.duckdb", read_only = FALSE)
dbWriteTable(con, "tabla_horario", df_resumen, overwrite = TRUE)
dbDisconnect(con, shutdown = TRUE)
