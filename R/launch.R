#' @title Launch app
#' @description Just run ABTestDeception::launch(). That's it.
#' @export
launch <- function() {
  ggplot2::theme_set(theme_minimal(base_size=32))
  thematic::thematic_shiny(font='auto')
  shinyApp(ui, server)
}
