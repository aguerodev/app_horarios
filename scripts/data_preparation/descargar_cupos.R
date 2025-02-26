descargar_cupos <- function() {
  box::use(
    selenider[
      selenider_session,
      chromote_options,
      open_url,
      s,
      elem_set_value,
      elem_click,
      get_page_source
    ],
    rvest[html_table],
    purrr[keep_at, list_rbind],
    dplyr[mutate, across, where, na_if, select],
    tidyr[fill, drop_na],
    stats[setNames]
  )
  session <- selenider_session(
    "chromote",
    timeout = 10,
    options = chromote_options(headless = TRUE)
  )

  open_url("https://ematricula.ucr.ac.cr/ematricula/")
  s("input[name='carne']") |>
    elem_set_value(text = "B90080")
  s("input[name='pin']") |>
    elem_set_value(text = "Lk@9#TmQ5!rW2")
  s("input[name='crudMethod']") |>
    elem_click()

  open_url(
    "https://ematricula.ucr.ac.cr/ematricula/showInformeCuposDisponibles.do"
  )
  x <- get_page_source() |>
    html_table() |>
    keep_at(2) |>
    list_rbind() |>
    mutate(
      across(where(is.character), \(x) na_if(x, ""))
    ) |>
    fill(1:6) |>
    drop_na() |>
    setNames(c("codigo", "curso", "creditos", "grupo", "horario", "cupos")) |>
    select(codigo, grupo, cupos) |>
    mutate(
      grupo = sprintf("%03d", grupo)
    )

  open_url("https://ematricula.ucr.ac.cr//ematricula/logout.do")

  return(x)
}
