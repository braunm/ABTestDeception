#' Labels for sliders.  They are very long for ui.R, so define them here.



sldr <- list(
  My="$$\\mathbf{E}[Y^{(1)}_{Aud}]~$$Aggregate among exposed users",
  EY0P="$$\\mathbf{E}[Y^{(0)}_P]~$$Baseline among unexposed Poets",
  EY0Q="$$\\mathbf{E}[Y^{(0)}_Q]~$$Baseline among unexposed Quants",
  at="$$\\alpha_\\tau~$$Ad A to Ad B",
  pt="$$\\pi_\\tau~$$Poets to Quants",
  rt="$$\\rho_\\tau~$$Divergent delivery",
  ay="$$\\alpha_Y~$$Ad A to Ad B",
  py="$$\\pi_Y~$$Poets to Quants",
  ry="$$\\rho_Y~$$Response interaction",
  gamma="$$\\gamma_P~$$Proportion of Poets in audience",
  zeta="$$\\zeta_A~$$Proportion of audience randomly assigned to Ad A",
  Phi="$$\\tilde{\\Phi}~$$Aggregate targeting probability"
)


#' @title this_sliderBox
#' @description constructs a box for a ui slider
#' @param name name
#' @param label  string for label (possibly MathJax
#' @param grid use the grid on the slider?
#' @param ... other parameters passed to sliderTextinput
#' @export
this_sliderBox <- function(name, label, grid=TRUE, ...) {

 ## steps_var <- rlang::sym(str_c("steps$", name))
  ##  choices <- eval(steps_var)
  choices <- steps(name)
  res <- shinyWidgets::sliderTextInput(name, label, choices=choices,
                  grid=grid, ...)
  return(res)

}


## My=


  ## pt="$$\\pi_\\tau=\\frac{\\Phi_P^{\\phantom{(1)}}}{\\Phi_Q^{\\phantom{(1)}}}$$",
  ## rt="$$\\rho_\\tau=\\frac{\\Phi_{PA}^{\\phantom{(1)}}}{\\Phi_{QA}^{\\phantom{(1)}}}\\frac{\\Phi_{QB}^{\\phantom{(1)}}}{\\Phi_{PB}^{\\phantom{(1)}}}$$",
## ay="$$\\alpha_Y=\\frac{\\Theta^{(1)}_A}{\\Theta^{(1)}_B}$$",
## py="$$\\pi_Y=\\frac{\\Theta^{(1)}_P}{\\Theta^{(1)}_Q}$$",
## ry="$$\\rho_Y=\\frac{\\Theta^{(1)}_{PA}}{\\Theta^{(1)}_{QA}}\\frac{\\Theta^{(1)}_{QB}}{\\Theta^{(1)}_{PB}}$$",
