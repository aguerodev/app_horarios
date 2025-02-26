ui <- function(request) {
  tagList(
    # Header con título y botón de logout
    tags$div(
      style = "width: 100%; display: flex; justify-content: space-between; align-items: center; padding: 10px 20px; border-bottom: 1px solid #ddd;",
      tags$h1("Experimento Horarios", style = "margin: 0; font-size: 24px;"),
      logoutButton(label = "Cerrar Sesión")
    ),
    # Resto del layout de la aplicación
    page_sidebar(
      sidebar = sidebar(
        mod_select$selectUI("codes"),
        mod_ga$GAUI("ga")
      ),
      mod_table$tableUI("main_table"),
      tagList(
        tags$head(
          tags$script(
            "setTimeout(function(){
               var url = new URL(window.location.href);
               url.search = '';
               history.replaceState(null, '', url.toString());
             }, 2000);"
          ),
          tags$script(
            HTML(
              "
              Shiny.addCustomMessageHandler('updateButton', function(message) {
                var uid = message.uid;
                var newHtml = message.html;
                var btn = document.querySelector('button.estadoBtn[data-uid=\"' + uid + '\"]');
                if (btn) {
                  btn.outerHTML = newHtml;
                }
              });
            "
            )
          )
        )
      )
    )
  )
}
