#' @name steps
#' @title Steps for Sliders
#' @param v variable associated with the slider
#' @return a vector of franctions indicating discrete steps on the slider
#' @export
steps <- function(v) {
  switch(v,
         at=MASS::fractions(c(1 / 2, 4 / 7, 2 / 3, 4 / 5, 1, 5 / 4, 3 / 2, 7 / 4, 2)),
         ay=MASS::fractions(c(1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3, 1,
                              3 / 2, 2, 5 / 2, 3, 7 / 2, 4)),
         pt=MASS::fractions(c(1 / 8, 2 / 15, 1 / 7, 2 / 13, 1 / 6, 2 / 11,
                              1 / 5, 2 / 9, 1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3,
                              1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4, 9 / 2, 5, 11 / 2,
                              6, 13 / 2, 7, 15 / 2, 8)),
         py=MASS::fractions(c(1 / 8, 2 / 15, 1 / 7, 2 / 13, 1 / 6, 2 / 11, 1 / 5,
                              2 / 9, 1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3,
                              1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4, 9 / 2, 5, 11 / 2,
                              6, 13 / 2, 7, 15 / 2, 8)),
         rt=MASS::fractions(c(1 / 8, 1 / 7, 1 / 6, 1 / 5, 1 / 4, 1 / 3, 1 / 2, 1:8)),
         ry=MASS::fractions(c(1 / 8, 1 / 7, 1 / 6, 1 / 5, 1 / 4, 1 / 3, 1 / 2, 1:8)),
         gamma=seq(.3, .7, by=.05),
         zeta=seq(.3, .7, by=.05),
         Mt=seq(.05, .25, by=.05),
         My=seq(.05, .25, by=.05),
         EY0P=seq(0, .05, by=.005),
         EY0Q=seq(0, .05, by=.005))
}
