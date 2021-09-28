#' @title Launch app
#' @description Runs the ABTestDeception app
#' @export
launch <- function() {

  shiny::runApp(system.file("app", package='ABTestDeception'))

}
