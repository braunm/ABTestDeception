#' @name analytic_posteriors
#' @title Analytic functions for Prob(X=P|targeted, Z)
#' @param rt,at,pt rho, alpha, and pi for targeting
#' @param h Prob(Z=A)
#' @param g Prob(X=P)
#' @return sigma_PB
#' @export
Fspb <- function(rt, at, pt,  h, g) {
  d <- (h - 1) * (g * (pt - 1) + 1) * (rt - 1)
  n1 <- h * (at * rt - 1)
  n2 <- g * (pt * (at + rt - 2) * h - at * rt * h + h + 2 * pt - pt * rt - 1)
  t1 <- (n1 + n2 + 1)/ d

  w1 <- (at * (pt - rt))^2 + (pt * rt - 1)^2
  w2 <- -2 * at * (rt * pt^2 + rt^2 * pt - 4 * rt * pt + pt + rt)
  q1 <- (w1 + w2) * h^2
  q2 <- 2 * (at * (rt * pt^2 + rt^2 * pt - 4 * rt * pt + pt + rt) - (pt * rt - 1)^2) * h
  q3 <- (pt * rt - 1)^2

  x1 <- (q1 + q2 + q3) * g^2

  v1 <- ((pt - rt) * rt * at^2 + 2 * rt * at + pt * (rt^2 - 4 * rt + 1) * at + pt * rt - 1) * h^2
  v2 <- -(2 * at * rt + 2 * pt * rt + at * pt * (rt^2 - 4 * rt + 1) - 2) * h
  v3 <- pt * rt - 1

  x2 <- 2 * (v1 + v2 + v3) * g
  x3 <- (h * (at * rt - 1) + 1)^2

  sq <- sqrt((x1 + x2 + x3) / d^2)

  res <- ((rt < 1) * (t1 - sq) + (rt >= 1) * (t1 + sq)) / 2


  return(res)
}

#' @rdname analytic_posteriors
#' @param rt rho_t
#' @param spb sigma_PB
#' @export
Fspa <- function(rt, spb) {
  w <- rt * spb / (1 - spb)
  return(w / (1 + w))
}

## IMPORTANT.  These functions are really for Theta1.  They are lifts only to the extent that Theta0=0 by default.

#' @name lambda_XZ
#' @title lambda_XZ functions
#' @param ay,py,ry alpha, pi, and rho for EY
#' @param h allocation probability Prob(Z=A)
#' @param g Prob(X=P) for audience
#' @param MY average potential lift Y1 for audience
#' @param theta0 baseline EY0 if unexposed
#' @return lambda_XZ
#' @export
FLpa <- function(ay, py, ry, h, g, My) {

  s <- FLsqrt(ay, py, ry, h, g)
  n1 <- g * h * (ay * (py + ry - 2 * py * ry) + py * ry - 1)
  n2 <- g + h - 1 - ay * h * ry - g * py * ry
  num <- My * (s + n1 + n2)
  den <- 2 * g * h * (ry - 1) * ((ay-1) * h + 1) * (g * (py - 1) + 1)
  EY1 <- -num / den
  return(EY1)
}

#' @rdname lambda_XZ
#' @export
FLqa <- function(ay, py, ry, h, g, My) {

  s <- FLsqrt(ay, py, ry, h, g)
  d1 <- h * (ay * (g * (py + ry - 2) - ry + 2) - g * py * ry + g - 1)
  d2 <- g * py * ry - g + 1
  EY1 <- 2 * ay * My / (s + d1 + d2)
  return(EY1)
}

#' @rdname lambda_XZ
#' @export
FLpb <- function(ay, py, ry, h, g, My) {

  s <- FLsqrt(ay, py, ry, h, g)
  d1 <- g * (h * py * (ay + ry - 2) - ay * h * ry + h - py * (ry - 2) - 1)
  d2 <- ay * h * ry - h + 1
  EY1 <- 2 * My * py / (s + d1 + d2)
  return(EY1)
}

#' @rdname lambda_XZ
#' @export
FLqb <- function(ay, py, ry, h, g, My) {

  s <- FLsqrt(ay, py, ry, h, g)
  d1 <- g * h * (ay * py - 1 - ry * (ay + py - 2))
  d2 <- (ay - 2) * h * ry + g * (py - 2) * ry + g + h + 2 * ry - 1
  EY1 <- 2 * My * ry / (s + d1 + d2)
  return(EY1)
}

#' @rdname lambda_XZ
FLsqrt <- function(ay, py, ry, h, g) {
## square root term for other lambda_XZ functions
  q1 <- (g + h - 1 + g * h * (ay * py - 1))^2
  q3 <- (ay * (g - 1) * h - g * (h - 1) * py)^2 * ry^2

  v1 <- g * (g - 1) * (h - 1)^2 * py
  v2 <- ay^2 * (g - 1) * g * h^2 * py
  v3 <- ay * (h - 1) * h * (1 + g * (g - 2 + 4 * py + g * (py - 4) * py))
  q2 <- -2 * ry * (v1 + v2 + v3)

  res <- sqrt(q1 + q2 + q3)


  return(res)
}


#' @name F_all
#' @title F_all
#' @param at,pt,rt alpha, pi, and rho for targeting
#' @param ay,py,ry alpha, pi, and rho for EY
#' @param MY average potential lift Y1 for audience
#' @param h allocation probability Prob(Z=A)
#' @param g Prob(X=P) for audience
#' @param EY0P, EY0Q baseline response rates
#' @return list of all computed values
#' @export
F_all<- function(at, pt, rt, Mt, ay, py, ry, My, g, h, EY0P=0, EY0Q=0) {

  spb <- Fspb(rt, at, pt, h, g)
  spa <- Fspa(rt, spb)

  EYpa <- FLpa(ay, py, ry, h, g, My)
  EYpb <- FLpb(ay, py, ry, h, g, My)
  EYqa <- FLqa(ay, py, ry, h, g, My)
  EYqb <- FLqb(ay, py, ry, h, g, My)

  Phi_pa <- FLpa(at, pt, rt, h, g, Mt)
  Phi_pb <- FLpb(at, pt, rt, h, g, Mt)
  Phi_qa <- FLqa(at, pt, rt, h, g, Mt)
  Phi_qb <- FLqb(at, pt, rt, h, g, Mt)

  Phi_A <- g * Phi_pa + (1 - g) * Phi_qa
  Phi_B <- g * Phi_pb + (1 - g) * Phi_qb

  EY0 <- g * EY0P + (1 - g) * EY0Q

  EYa_ATE <- g * EYpa + (1 - g) * EYqa
  EYb_ATE <- g * EYpb + (1 - g) * EYqb
  EYa_targ <- spa * EYpa + (1 - spa) * EYqa
  EYb_targ <- spb * EYpb + (1 - spb) * EYqb

  Lpa <- EYpa - EY0P
  Lpb <- EYpb - EY0P
  Lqa <- EYqa - EY0Q
  Lqb <- EYqb - EY0Q

  La_ATE <- g * Lpa + (1 - g) * Lqa
  Lb_ATE <- g * Lpb + (1 - g) * Lqb
  La_targ <- spa * Lpa + (1 - spa) * Lqa
  Lb_targ <- spb * Lpb + (1 - spb) * Lqb

  bias <- (spa - g) * (Lpa - Lqa)- (spb - g) * (Lpb - Lqb)

  Delta_ATE <-  La_ATE - Lb_ATE
  Delta_targ <- La_targ - Lb_targ

  res_XZ <- tribble(
    ~X, ~Z, ~PrTargXZ, ~post, ~EY, ~lift,
    'P', 'A', Phi_pa, spa, EYpa, EYpa - EY0P,
    'P', 'B', Phi_pb , spb, EYpb, EYpb - EY0P,
    'Q', 'A', Phi_qa , 1 - spa, EYqa, EYqa - EY0Q,
    'Q', 'B', Phi_qb , 1 - spb, EYqb, EYqb - EY0Q,
    'All', 'A', Phi_A, 1, EYa_ATE, EYa_ATE - EY0,
    'All', 'B', Phi_B, 1, EYb_ATE, EYb_ATE - EY0
  )

  res_all <- tribble(
    ~Z, ~PrTarg, ~EY_ATE, ~EY_targ, ~lift_ATE, ~lift_targ,
    'A', Phi_A, EYa_ATE, EYa_targ,La_ATE, La_targ,
    'B', Phi_B, EYb_ATE, EYb_targ,Lb_ATE, Lb_targ
  )

  res <- list(XZ=res_XZ, all=res_all,
              Delta_ATE=Delta_ATE, Delta_targ=Delta_targ,
              bias=bias)

  return(res)
}


#' @name bias
#' @title bias
#' @param at,pt,rt alpha, pi, and rho for targeting
#' @param ay,py,ry alpha, pi, and rho for EY
#' @param MY average potential lift Y1 for audience
#' @param zeta allocation probability Prob(Z=A)
#' @param g Prob(X=P) for audience
#' @param EY0P, EY0Q baseline response rates
#' @return list of all computed values
#' @details vectorized, to support plotting
#' @export
F_bias<- function(at, pt, rt, Mt, ay, py, ry, My, g, zeta, EY0P=0, EY0Q=0) {

  spb <- Fspb(rt, at, pt, zeta, g)
  spa <- Fspa(rt, spb)

  EYpa <- FLpa(ay, py, ry, zeta, g, My)
  EYpb <- FLpb(ay, py, ry, zeta, g, My)
  EYqa <- FLqa(ay, py, ry, zeta, g, My)
  EYqb <- FLqb(ay, py, ry, zeta, g, My)

  EY0 <- g * EY0P + (1 - g) * EY0Q

  Lpa <- EYpa - EY0P
  Lpb <- EYpb - EY0P
  Lqa <- EYqa - EY0Q
  Lqb <- EYqb - EY0Q

  La_ATE <- g * Lpa + (1 - g) * Lqa
  Lb_ATE <- g * Lpb + (1 - g) * Lqb
  La_Targ <- spa * Lpa + (1 - spa) * Lqa
  Lb_Targ <- spb * Lpb + (1 - spb) * Lqb


  Err_A <- La_Targ - La_ATE
  Err_B <- Lb_Targ - Lb_ATE
  bias <- (spa - g) * (Lpa - Lqa)- (spb - g) * (Lpb - Lqb)

  Delta_ATE <-  La_ATE - Lb_ATE
  Delta_Targ <- La_Targ - Lb_Targ

  res <- tibble(pt=pt,
                Err_A=Err_A,
                Err_B=Err_B,
                bias=bias)

  return(res)
}
