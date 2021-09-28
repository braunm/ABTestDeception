

#' @name theme_custom
#' @title Theme for plots
#' @export
theme_custom <- function(font_size=11, font_root='sans') {
  theme_minimal(
    base_size = font_size,
    base_family=font_root) +
    theme(panel.grid.minor=element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(color='black'),
          axis.title=element_text(size=rel(.9), color='black'),
          axis.text=element_text(size=rel(.8), color='black')
          )
}
