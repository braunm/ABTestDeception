.onLoad <- function(libname, pkgname) {

  rules_file <- system.file("www/rules.scss", package="ABTestDeception")
  assign('rules_file', rules_file, envir=topenv())

  ggplot2::theme_set(plot_theme())





  }
