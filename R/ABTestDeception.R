#' @name ABTestDeception-package
#' @aliases ABTestDeception-package
#' @docType package
#' @title A Shiny App Demonstrating the Effects of Divergent Delivery and Response Heterogenity on Bias in
#' @description See Braun and Schwartz (2021)
#' @details Details go here
#'
#' @references
#' Braun, Michael and Eric Schwartz. 2021. The A/B Test Deception: Divergent Delivery, Ad Response Heterogeneity, and Erroneous Inferences in Online Advertising Field Experiments. https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3896024
#'
#' @keywords package
#' @importFrom MASS fractions
#' @importFrom scales label_number
#' @importFrom sass sass
#' @importFrom shinyWidgets sliderTextInput
#' @importFrom grDevices rgb
#' @importFrom kableExtra kable_styling row_spec add_header_above
#' @import knitr
#' @import shiny
#' @import stringr
#' @import forcats
#' @import thematic
#' @import ggrepel
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @import showtext
#' @import sass
#' @import methods
globalVariables(c('X', 'Z', 'post', 'lift', 'lift_targ', 'XZ', 'PrX', 'PrZ',
                  'post_Q', 'mixQ', 'EY_XZ', 'x0', 'y0', 'w', 'h', 'angle', 'axislab',
                  'value', 'pt', 'Mt', 'Xname', 'lab', 'lift_P', 'lift_Q', 'siglab',
                  'prior_Q', 'scope', 'A', 'B', 'diffAB', 'Targ', 'Aud', 'y', 'err_label',
                  'xlabs', 'color', 'xbars', 'sig', 'PrTargXZ', 'EY', 'lift_ATE',
                  'label', 'R', 'err_color', 'x', 'mix_color', 'lift_All'))
