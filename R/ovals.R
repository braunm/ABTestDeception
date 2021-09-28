#' @rdname oval
#' @title oval_width
#' @param PZ, QZ data for P and Q
#' @param Mt, PrZ, g other parameters
oval_width <- function(PZ, QZ, Mt, PrZ, g) {

  ## if (PrZ <= .5) {
  ##   res <- 6 * PrZ
  ## } else {
  ##   res <- 1.5
  ## }

  res <- 2
  return(res)
}

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


#' @rdname oval
oval_dims <- function(P, Q, Mt, Z, PrZ, g, fudge=.9) {

  w <- oval_width(P, Q, Mt, PrZ, g)
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


theme_egg <- function() {
  theme_minimal(
    base_size = font_size,
    base_family=font_family) +
    theme(panel.grid=element_blank(),
          strip.background=element_blank(),
          strip.text=element_text(size=12, color='black'),
          axis.title=element_text(size=12),
          axis.title.x.top=element_text(hjust= .2),
          axis.ticks=element_blank(),
          axis.text.x.top=element_text(size=12, vjust= 0, color='black'),
          axis.text.y=element_text(color='black', angle=90, hjust=.5,
                                   margin=margin(0, 0, 0, 0),
                                   vjust= 1,
                                   size=12),
          panel.spacing=unit(0, 'pt'),
          legend.position='bottom',
          legend.text=element_text(size=16),
          legend.box.margin=margin(0, 0, 0, 0, unit='pt')
          )
}
