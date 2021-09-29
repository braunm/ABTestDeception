
#' @name get_PrTarg
#' @title Get targeting probabilities
#' @param PrTargXZ Tibble with columns X, Z, and PrTarg
#' @param PrXZ tibble with columns X, Z, and mix (mixing probs)
#' @return a list
#' @details Also computes marginals
#' @export
get_PrTarg <- function(PrTargXZ, PrXZ) {

  x_lev <- c('P', 'Q', 'All')
  z_lev <- c('A', 'B', 'All')
  xz_lev <- c('P_A', 'P_B', 'Q_A', 'Q_B',
              'P_All', 'Q_All', 'All_A', 'All_B',
              'All_All')

  W1 <- PrTargXZ %>%
    left_join(PrXZ, by=c('X', 'Z'))
  WZ <- W1 %>%
    group_by(X) %>%
    summarize(PrTarg=sum(PrTargXZ * PrZ)) %>%
    ungroup() %>%
    mutate(Z='All')
  WX <- W1 %>%
    group_by(Z) %>%
    summarize(PrTarg=sum(PrTargXZ * PrX)) %>%
    ungroup() %>%
    mutate(X='All')
  WAll <- W1 %>%
    summarize(PrTarg=sum(PrTargXZ * PrX * PrZ)) %>%
    mutate(X='All', Z='All')

  res <- W1 %>%
    select(X, Z, PrTarg=PrTargXZ) %>%
    bind_rows(WZ, WX, WAll) %>%
    unite('XZ', X, Z, remove=FALSE, sep='_') %>%
    mutate(X=factor(X, levels=x_lev),
           Z=factor(Z, levels=z_lev),
           XZ=factor(XZ, levels=xz_lev)
           )


  return(res)
}

#' @name get_postXZ
#' @title Get posterior prob of X, given targ and Z
#' @param PrTargXZ tibble with columns X, Z, PrTargXZ
#' @param PrXZ tibble with columns X, Z, PrX, PrZ
#' @details Pr(X|targ,Z)=Pr(targ|X,Z)*Pr(X|Z)/Pr(targ|Z)
#' @export
get_postXZ <- function(PrTargXZ, PrXZ) {

  PrTarg <- get_PrTarg(PrTargXZ, PrXZ) # Pr(targ|X,Z)

  PrTargZ <- PrTarg %>%
    filter(X == 'All', Z != 'All') %>%
    rename(PrTargZ=PrTarg) %>%
    select(-X, -XZ)

  PrTargAll <- PrTarg %>%
    filter(X == 'All', Z == 'All') %>%
    pull(PrTarg)

  res <- PrTargXZ  %>%
    left_join(PrXZ, by=c('X', 'Z')) %>%
    left_join(PrTargZ, by='Z') %>%
    mutate(PrXtargZ=PrTargXZ * PrX / PrTargZ) %>%
    mutate(PrXZtarg=PrTargXZ * PrX * PrZ / PrTargAll)


  return(res)
}
