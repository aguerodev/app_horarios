options(box.path = ".")

box::use(
  dplyr[distinct, select, left_join, collect],
  shiny[
    NS,
    tagList,
    moduleServer,
    reactive,
    observeEvent,
    observe,
    req,
    showNotification,
    showModal,
    modalDialog,
    modalButton,
    renderTable,
    ExtendedTask,
    div,
    tags,
    HTML
  ],
  bslib[input_task_button, bind_task_button],
  shinyscreenshot[screenshotButton]
)

box::use(
  modules / fn_prepare_data[preprocess_data],
  modules / fn_ga[calculate_ga]
)

#' @export
GAUI <- function(id) {
  ns <- NS(id)
  tagList(
    input_task_button(
      id = ns("btn"),
      label = "Calcular Horario",
      label_busy = "Calculando...",
      type = "warning"
    )
  )
}

#' @export
GAServer <- function(id, con, selected_rows) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # Crear la consulta base una sola vez
      table_query <- dplyr::tbl(con, "temp_horario")

      # Procesar datos reactivamente
      df <- reactive({
        req(selected_rows())
        preprocess_data(con, selected_rows())
      })

      # Configurar tarea extendida para cálculo GA
      fun_ga <- ExtendedTask$new(calculate_ga) |>
        bind_task_button("btn")

      # Manejar el evento de clic en el botón
      observeEvent(input$btn, {
        req(df())

        if (nrow(df()) == 0) {
          showNotification(
            "Seleccione mínimo 1 curso antes de calcular el horario.",
            type = "error"
          )
          return()
        }

        showNotification(
          "Vamos a calcular su horario, por favor espere... (puede durar unos minutos)",
          type = "message"
        )

        fun_ga$invoke(cursos = df())
      })

      # Observar y mostrar resultados
      observe({
        result <- fun_ga$result()
        req(result)

        # Preparar datos de resultado
        courses <- result |>
          distinct(codigo, seccion) |>
          select(codigo, grupo = seccion)

        # Obtener datos completos de la base de datos
        df_data <- collect(table_query)

        # Unir datos de resultados con información completa
        schedule <- left_join(
          courses,
          df_data,
          by = c("codigo" = "codigo", "grupo" = "grupo")
        ) |>
          select(codigo, curso, grupo, horario)

        # ID único para el contenedor del botón
        ss_button_id <- ns("ss_button_wrapper")

        # Mostrar modal con resultados
        showModal(
          modalDialog(
            title = "Horario Óptimo",
            size = "l",
            renderTable(schedule, striped = TRUE, hover = TRUE),
            div(
              id = ss_button_id,
              screenshotButton(
                label = "Descargar Horario",
                selector = ".modal-body",
                scale = 1.5,
                filename = "horario",
                download = TRUE,
                server_dir = tempdir()
              )
            ),
            tags$script(HTML(sprintf(
              "$(document).on('click', '#%s button', function() { $('#%s').hide(); });",
              ss_button_id,
              ss_button_id
            ))),
            easyClose = TRUE,
            footer = modalButton("Cerrar")
          )
        )
      })
    }
  )
}
