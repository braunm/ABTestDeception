
## #' @rdname themes
## #' @name theme_custom
## #' @title Theme for plots
## #' @export
## theme_custom <- function(font_size=11, font_root='sans') {
##   theme_minimal(
##     base_size = font_size,
##     base_family=font_root) +
##     theme(panel.grid.minor=element_blank(),
##           strip.background=element_blank(),
##           strip.text=element_text(color='black'),
##           axis.title=element_text(size=rel(.9), color='black'),
##           axis.text=element_text(size=rel(.8), color='black')
##           )
## }

#' @name plot_theme
#' @title Theme for Plots
#' @return A theme
#' @export
plot_theme <- function() {
  theme(panel.grid.minor = element_blank(),
        panel.grid.major=element_line(size=.5),
        text=element_text(size=12),
          plot.margin=margin())
}

## #' @name get rules file
## #' @title sass file


#' @name app_theme
#' @title the theme of the app
#' @return Sass theme for the app
#' @export
app_theme <-  function() {

   rules_file <- function() {
    system.file("www/rules.scss", package="ABTestDeception")
  }

  bslib::bs_theme(version="5",
                  primary='#354c97',
                  secondary = '#212529',
                  success='rgba(89,195,195,1)',
                  warning='orange',
                  danger=smu_red,
                  light='white',
                  base_font=font_google("Roboto Condensed"),
                  heading_font=font_google("Roboto Condensed"),
                  font_scale=.8) %>%
    bslib::bs_add_variables(
      "offcanvas-horizontal-width"="300px",
      "offcanvas-vertical-height"="25vh") %>%
    bslib::bs_add_rules(sass::sass_file(rules_file()))
}
