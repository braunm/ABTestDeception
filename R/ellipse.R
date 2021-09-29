


## #' @title compute ellipse coordinates
## #' @param x x coordinate
## #' @param x0,y0 center
## #' @param h,w  height/2 and width/2, assuming no rotation
## #' @param th angle or rotation, in radians
## #' @return a tibble with x and the two y values (may include NaN)
## #' @export
## compute_ellipse <- function(x, h, w, x0, y0, th) {

##   A <-2 * h^2 * y0 * cos(th)^2
##   B <- sqrt(h^2 + w^2 - 2 * (x - x0)^2 + (h - w) * (h + w) * cos(2 * th))
##   C <- 2 * w^2 * y0 * sin(th)^2
##   D <- (h - w) * (h + w) * (x - x0) * sin(2 * th)
##   E <- 2 * (h^2 * cos(th)^2 + w^2 * sin(th)^2)

##   yL <- (A - sqrt(2) * h * w * B + C + D) / E
##   yU <- (A + sqrt(2) * h * w * B + C + D) / E

##   res <- tibble(x=x, yU=yU, yL=yL)
##   return(res)
## }

#' @rdname ellipse
#' @title compute ellipse coordinates assumming no rotation
#' @param x x coordinate
#' @param x0,y0 center
#' @param h,w  height/2 and width/2, assuming no rotation
#' @return a tibble with x and the two y values (may include NaN)
#' @export
compute_ellipse2 <- function(x, h, w, x0, y0) {

  res <- tibble(x=x, R= 1 - (x - x0)^2 / w^2) %>%
    filter(R >= 0) %>%
    mutate(yL=y0 - h * sqrt(R),
           yU=y0 + h * sqrt(R))


  return(res)

  }

#' @rdname ellipse
#' @title wrapper around compute_ellipse for tibble
#' @param D tibble with w, h, x0, y0, and th
#' @param nx number of samples (some of these may be dropped if the ellipse is rotated).
#' @return tibble with x, yL, yU, and non-ellipse paramters from D (e.g., Z, col)
#' @importFrom rlang .data
#' @export
func_ellipse <- function(D, nx=200) {

  xb <- get_x_bounds(D$h, D$w, D$angle)
  x <- D$x0 + seq(xb[1], xb[2], length=nx)

#  x <- D$x0 + seq(-pmax(D$h, D$w), pmax(D$h, D$w), length=nx)
  E <- compute_ellipse2(x, D$h, D$w, D$x0, D$y0)
  D2 <- D %>% select(-x0, -y0, -w, -h, -angle)
  res <- E %>%
    filter(is.finite(.data$yU), is.finite(.data$yL)) %>%
    mutate(!!!D2)


  return(res)
}


#' @rdname ellipse
#' @title bounding box on x axis
#' @param w,h semi-axes
#' @param rotation angle
#' @return vector with xmin and xmax
#' @export
get_x_bounds <- function(w, h, th) {
## for unshifted, rotated ellipse

  q1 <- w^2 * sin(th)^2 + h^2 * cos(th)^2
  num <- -q1 * h^2 * w^2
  den1 <- cos(th)^2 * sin(th)^2 * (w^2 - h^2)^2
  den2 <- q1 * (w^2 * cos(th)^2 + h^2 * sin(th)^2)

  R <- sqrt(num / (den1 - den2))

  res <- c(-R, R)
  return(res)



}

## #' @rdname ellipse
## #' @param D tibble with ellipse parameters
## #' @param n number of draws per class
## #' @param pctR1 pct in treatment group
## #' @param padding factor to keep points away from the border
## #' @export
## sample_ellipse <- function(D, n=100, pctR1=.5, padding=.001) {

##   u <- runif(n)
##   xu <- pmax(D$h, D$w)
##   xl <- -xu
##   x <- xl + (xu - xl) * u + D$x0

##   Ybounds <- func_ellipse2_(x, D$h, D$w, D$x0, D$y0, D$angle) %>%
##     filter(is.finite(yL), is.finite(yU))
##   n <- NROW(Ybounds)

##   res <- Ybounds %>%
##     mutate(ysamp=runif(n, yL, yU),
##            arm=rbernoulli(n, pctR1),
##            rep=row_number(),
##            Z=D$Z) %>%
##     mutate(arm=factor(str_c("R", as.integer(arm))))

##   return(res)
## }
