

#' @rdname oval
#' @param PZ, QZ Phi values for ad Z
#' @param w width of the oval
#' @return half-height of the oval
oval_yrad <- function(PZ, QZ, w, g, PrZ) {

  res <- PrZ * 400 * (g * PZ +  (1 - g) * QZ) / ( pi * w)
  return(res)

}

#' @rdname oval
#' @param fudge fudge factor b/c shift is not proportional
oval_ypos <- function(PZ, QZ, yrad, g, fudge=.9) {

  gx <- 10 * (2 * (1 - g) - 1)
  marg <- g * PZ + (1 - g) * QZ
  sp <- g * PZ / marg

  res <- fudge * yrad * ( 2 * sp - 1) + gx
  return(res)
}


#' @name oval
#' @title Dimensions of ovals
#' @return a tibble of oval dimensions
oval_dims <- function(P, Q, Mt, Z, PrZ, g, fudge=.9) {

  w <- 2
  yrad <- oval_yrad(P, Q, w, g, PrZ)
  ypos <- oval_ypos(P, Q, yrad, g, fudge)


  while (ypos + yrad >= 9.9 | ypos - yrad <= -9.9) {

 ## while (yrad > min(19.5 * g, 19.5 * (1 - g))) {
    w <- w * 1.01
    yrad <- oval_yrad(P, Q, w, g, PrZ)
    ypos <- oval_ypos(P, Q, yrad, g, fudge)
  }

  ## A
  if (Z == 'A') {
    q <- 10 * (2 * PrZ - 1)
    xpos <- (q - 10) / 2
  } else {
    q <- 10 * (1 - 2 * PrZ)
    xpos <- (q + 10) / 2
  }
  res <- tibble(Z=Z, w=w, h=yrad, y0=ypos, x0=xpos)
  return(res)
}
