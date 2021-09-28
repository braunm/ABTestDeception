
#' @name get_EY
#' @title Get conversion probabilities if exposed
#' @param EYXZ Tibble with X, Z, and EY1
#' @param PrXZ Tibble with X, Z, PrX, and PrZ
#' @param EY0 Tibble with X and EY0
#' @return a list
#' @details conversion probabilities, conditional on being targeted
#' @export
get_EY <- function(EYXZ, PrXZ, EY0) {

  x_lev <- c('P', 'Q', 'All')
  z_lev <- c('A', 'B', 'All')
  xz_lev <- c('P_A', 'P_B', 'Q_A', 'Q_B',
              'P_All', 'Q_All', 'All_A', 'All_B',
              'All_All')


  W1 <- EYXZ %>%
    left_join(PrXZ, by=c('X', 'Z'))
  WZ <- W1 %>%
    group_by(X) %>%
    summarize(EY1=sum(EY_XZ * PrZ)) %>%
    ungroup() %>%
    mutate(Z='All')
  WX <- W1 %>%
    group_by(Z) %>%
    summarize(EY1=sum(EY_XZ * PrX)) %>%
    ungroup() %>%
    mutate(X='All')
  WAll <- W1 %>%
    summarize(EY1=sum(EY_XZ * PrX * PrZ)) %>%
    mutate(X='All', Z='All')

  g <- PrXZ %>%
    filter(Z == 'A') %>%
    select(X, PrX)

  tmp <- EY0 %>%
    left_join(g, by='X')
  EY0All <- with(tmp, sum(EY0 * PrX))
  EY0a <- EY0 %>%
    add_row(X='All', EY0=EY0All)


  res <- W1 %>%
    select(X, Z, EY1=EY_XZ) %>%
    bind_rows(WZ, WX, WAll) %>%
    left_join(EY0a, by='X') %>%
    unite('XZ', X, Z, remove=FALSE, sep='_') %>%
    mutate(X=factor(X, levels=x_lev),
           Z=factor(Z, levels=z_lev),
           XZ=factor(XZ, levels=xz_lev))


  return(res)
}
