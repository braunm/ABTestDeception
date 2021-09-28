#' @title Launch app
#' @description Runs the ABTestDeception app
#' @export
launch <- function(...) {

  ## Compile Sass into css file (this just seems to work better than bslib's functions.)
##  bs_path <- stringr::str_c(.libPaths(), "/bslib/lib/bs5/scss/") ## but use bslib-provided Bootstrap
  ## sass::sass(input=sass::sass_file(system.file("inst/app/sass/base.scss", package='ABTestDeception', mustWork=TRUE)),
  ##            output=system.file("inst/app/www/custom.css", package='ABTestDeception', mustWork=TRUE),
  ##            options=sass::sass_options(include_path = bs_path),
  ##            cache=FALSE)

 ## sass::sass(input=sass::sass_file("inst/app/sass/base.scss"),
 ##             output="inst/app/www/custom.css",
 ##             options=sass::sass_options(include_path = bs_path),
 ##             cache=FALSE)




  shinyApp(ui, server, ...)


}
