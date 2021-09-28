#' @name plot_diff
#' @title plot differences in lifts
#' @param Delta tibble
#' @export
plot_diff <- function(Delta) {

  D2 <- Delta %>%
    select(-contains('err')) %>%
    pivot_longer(cols= starts_with(c('A_', 'B_', 'diffAB')), names_to=c('stat', 'scope'),
                 names_sep='_', values_to='lift',
                 names_ptypes=list(stat=factor(), scope=factor())) %>%
    pivot_wider(names_from=stat, values_from=lift) %>%
    mutate(scope=fct_recode(scope, Exposed="exp", Audience="all"),
           X=fct_recode(X, Protons='P', Quarks='Q'))

  P2 <- D2 %>%
    ggplot() +
    geom_segment(aes(x=A, xend=B, y=diffAB, yend=diffAB), size=1.5, color="gray20") +
    geom_hline(aes(yintercept=diffAB), size=.5) +
    geom_label(aes(x=A + sign(A - B) * .02, y=diffAB), label='A',
               color='red', size=6) +
    geom_label(aes(x=B + sign(B - A) * .02, y=diffAB), label='B',
               color='blue', size=6) +
    facet_grid(rows=vars(X), cols=vars(scope)) +
    scale_x_continuous(expression("Lift "~lambda[Z])) +
    scale_y_continuous(expression(Delta == lambda[A] - lambda[B]), sec.axis=dup_axis()) +
    coord_cartesian(xlim=c(-.1, .7), ylim=c(-.6, .6)) +
    theme_bw() +
    theme(text=element_text(size=14))

  return(P2)

}
