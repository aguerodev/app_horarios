box::use(
  shiny[
    NS,
    tagList,
    selectizeInput,
    updateSelectizeInput,
    moduleServer,
    reactive,
    isolate
  ],
  dplyr[distinct, collect],
  stringi[stri_trans_general]
)

#' @export
selectUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectizeInput(
      inputId = ns("siglas"),
      label = "Busque los cursos por nombre o siglas.",
      choices = NULL,
      selected = character(0),
      multiple = TRUE,
      options = list(
        placeholder = "Seleccione los cursos"
      )
    )
  )
}

#' @export
selectServer <- function(id, con, logger) {
  moduleServer(id, function(input, output, session) {
    # Consulta optimizada
    getCourseData <- function() {
      dplyr::tbl(con, "temp_horario") |>
        distinct(codigo, curso) |>
        collect()
    }

    # Inicializar datos al inicio
    df <- getCourseData()
    values <- df$codigo
    .names <- paste0(df$codigo, " - ", df$curso)
    .names <- stri_trans_general(.names, "Latin-ASCII")
    names(values) <- .names

    # Actualizar selectizeInput
    updateSelectizeInput(
      session = session,
      inputId = "siglas",
      choices = values,
      server = TRUE
    )

    # Devolver reactive con logging
    siglas <- reactive({
      x <- input$siglas
      if (length(x) > 0) {
        logger$debug(
          "Se seleccionaron los cursos {paste0(x, collapse = ', ')}"
        )
      }
      return(x)
    })

    return(siglas)
  })
}
