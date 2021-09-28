#' @title Launch app
#' @description Runs the ABTestDeception app
#' @export
launch <- function() {

  shiny::runApp(system.file("inst/app", package='ABTestDeception'))

}
