box::use(
  shiny[
    NS,
    tagList,
    h4,
    moduleServer,
    observeEvent,
    reactiveVal,
    req
  ],
  DT[renderDataTable, datatable, DTOutput],
  dplyr[filter, collect, pull],
  DBI[dbExecute]
)

#' @export
tableUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Marque solo cursos disponibles o sin cupos"),
    DTOutput(ns("tabla"))
  )
}

#' @export
tableServer <- function(id, con, values) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      selected_rows <- reactiveVal(character(0))

      # Crear la consulta una sola vez
      table_query <- dplyr::tbl(con, "temp_horario")

      # Función para generar HTML de botones
      createButtonHTML <- function(uid, cupo) {
        btn_class <- if (cupo == 1) "btn-warning" else "btn-secondary"
        btn_text <- if (cupo == 1) "Marcar sin cupos" else "Marcar con cupos"

        sprintf(
          "<button class='btn %s btn-sm estadoBtn' data-uid='%s' onclick=\"Shiny.setInputValue('%s', %s, {priority: 'event'});\">%s</button>",
          btn_class,
          uid,
          ns("toggle_sin_cupos"),
          uid,
          btn_text
        )
      }

      # Renderizar tabla
      output$tabla <- renderDataTable({
        req(values())
        # Obtener los valores actuales (esto evalúa el reactiveVal)
        current_values <- values()

        df <- table_query |>
          filter(codigo %in% !!current_values) |> # Forzar evaluación con !!
          collect()

        if (nrow(df) == 0) return(data.frame())

        # Vectorizar la creación de botones
        df$Estado <- vapply(
          seq_len(nrow(df)),
          function(i) createButtonHTML(df$uid[i], df$cupo[i]),
          character(1)
        )

        datatable(
          df,
          escape = FALSE,
          selection = "none",
          options = list(
            dom = 't',
            paging = TRUE,
            scrollY = "400px",
            scrollCollapse = TRUE,
            deferRender = TRUE,
            pageLength = -1,
            columnDefs = list(list(visible = FALSE, targets = c(0, 8)))
          ),
          rownames = FALSE
        )
      })

      # Actualizar selected_rows cuando cambian los valores
      shiny::observe({
        req(values())
        # Obtener los valores actuales
        current_values <- values()

        # Forzar evaluación antes de la consulta SQL
        selected <- table_query |>
          filter(codigo %in% !!current_values, cupo == 1) |>
          collect() |>
          pull(uid)

        selected_rows(selected)
      })

      # Manejar eventos de botón
      observeEvent(input$toggle_sin_cupos, {
        uid <- as.numeric(input$toggle_sin_cupos)
        current_selected <- selected_rows()
        currently_selected <- uid %in% current_selected
        new_cupo_value <- if (currently_selected) 0 else 1

        # Actualizar la base de datos
        dbExecute(
          con,
          "UPDATE temp_horario SET cupo = ? WHERE uid = ?",
          params = list(new_cupo_value, uid)
        )

        # Actualizar selected_rows
        if (currently_selected) {
          selected_rows(setdiff(current_selected, uid))
        } else {
          selected_rows(c(current_selected, uid))
        }

        # Crear y enviar el nuevo HTML del botón
        newButton <- createButtonHTML(uid, new_cupo_value)
        session$sendCustomMessage(
          "updateButton",
          list(uid = uid, html = newButton)
        )
      })

      return(selected_rows)
    }
  )
}
