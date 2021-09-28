#' @name mix_plot2
#' @title create a mixture diagram
#' @param XZ tibble with X, Z, postX, EY
#' @param All,g,M other parameters (define later)
#' @param prior_color color to refer to the prior
#' @export
mix_plot2 <- function(XZ, All, g, M, prior_color='darkgreen') {

  fmt3 <- scales::label_number(.001)
  fmt2 <- scales::label_number(.01)


  D2 <- XZ %>%
    dplyr::select(X, Z, post, lift) %>%
    pivot_wider(names_from=X, values_from=c(post, lift)) %>%
    left_join(select(All, Z, lift_targ), by='Z') %>%
    mutate(siglab=str_c("sigma[Q", Z, "]"),
           mix_color=rgb(get_PQ_mix(post_Q), max=255),
           prior_Q=c(g, g)
           )


  x_title  <- "Proportion of targeted who are Quants"
  y_title <- expression(bold(E)~"[ Lift | Z, targeted ]")


  y_labelsXZ <- str_c("lambda[", c('PA', 'PB', 'QA', 'QB'), "]")
  y_labelsTarg <- str_c("lambda[", c('A', 'B'), "]^plain(targ)")
  y_labelsATE <- str_c("lambda[", c('A', 'B'), "]^plain(ATE)")
  y_labels <- c(y_labelsXZ, y_labelsTarg, y_labelsATE)


  AB_labs <- tibble(
    X=c('P', 'P', 'Q', 'Q', rep('All', 4)),
    Z=rep(c('A', 'B'), 4),
    label=y_labels,
    y=c(D2$lift_P, D2$lift_Q, D2$lift_targ, D2$lift_All),
    x=c(rep(c(-.1, -.15), 2), rep(-.05, 2), rep(0, 2))) %>%
    mutate(
      mixQ=case_when(
        X == 'P' ~ 0,
        X == 'Q' ~ 1,
        X == 'All' & Z == 'A' ~ D2$post_Q[1],
        X == 'All' & Z == 'B' ~ D2$post_Q[2]),
      color=if_else(str_detect(label, "ATE"), prior_color, rgb(get_PQ_mix(mixQ), max=255))
    )

  Err_labs <- D2 %>%
    select(Z, lift_targ, lift_All) %>%
    mutate(
      xbars=c(1.01, 1.03),
      err_label=str_c("Error Ad ", Z),
      err_color=c(plot_colors$err_A, plot_colors$err_B),
      xlabs=xbars + .02)

  PQ_labs <- tribble(
    ~x, ~y, ~label,
    0, -.01, "All Poets",
    1, -.01, "All Quants")

  axis_labs <- tibble(y=c(0, max(XZ$lift + .02)),
                      lab=c(0,  fmt2(max(XZ$lift + .02))))

  prior_lab <- str_c("gamma[Q]==", 1 - g)


  mix_line <- D2 %>%
    select(Z, lift_P, lift_Q) %>%
    crossing(sig=seq(0, 1, length=50)) %>%
    mutate(y=(1 - sig) * lift_P + sig * lift_Q,
           color=rgb(get_PQ_mix(sig), max=255))

  P1 <- ggplot(data=D2) +
     geom_line(data=mix_line, aes(x=sig, y=y, color=color, group=Z), size=1) +
    geom_segment(data=D2, aes(x=post_Q, xend=post_Q, y= 0,
                              yend=lift_targ, color=mix_color),
                 linetype=2, size=1) +
    geom_segment(aes(x= -.01, xend= post_Q, y=lift_targ,
                     yend=lift_targ, color=mix_color),
                  linetype=2, size=1) +
    geom_text_repel(aes(x=post_Q, y= -.01, label=siglab,
                        color=mix_color), parse=TRUE, size=6,
                     direction='x', force=0, seed=123) +
    geom_text_repel(aes(x=post_Q, y= -.025, label=fmt3(post_Q), color=mix_color), size=5,
                    , direction='x',  force=0, seed=123) +
    geom_segment(aes(x=1 - g,xend=1 - g, y=0, yend=1),
                 color=prior_color, size=.75, linetype="dotted") +
    geom_segment(data=AB_labs, aes(y=y, yend=y, x= x,
                                   xend=1, color=color), linetype=1, size=.2) +
    ## geom_segment(aes(x=1 - g, xend=1, y=lift_All, yend=lift_All),
    ##              color=prior_color, size=.75, linetype="dotted") +
    geom_segment(data=axis_labs, aes(x= 0, xend=1, y=y, yend=y), color='gray10') +
    geom_text(data=axis_labs, aes(x= -.01, y=y, label=lab), color='gray10', hjust=1, size=6) +
    geom_text(data=PQ_labs, aes(x=x, y=y, label=label), color='black', size=5) +
    geom_label(data=AB_labs, aes(x=x, y=y, label=label, color=color),
               size=5, parse=TRUE,
               label.padding=unit(0.15, "lines"), hjust= 1) +
    annotate("label", x=1 - g, y=max(XZ$lift) + .01, label=prior_lab,
             color=prior_color, fill='white', size=5, parse=TRUE) +
    geom_point(aes(x=0, y=lift_P, shape=Z), color=plot_colors$P, size=rel(3)) +
    geom_point(aes(x=1, y=lift_Q, shape=Z), color=plot_colors$Q, size=rel(3)) +
    geom_point(aes(x=prior_Q, y=lift_All), color=prior_color,
               shape='circle open', stroke=1, size=4) +
    geom_point(aes(x=post_Q, y=lift_targ, color=mix_color),
               shape='circle open', stroke=1, size=4) +
    geom_errorbar(data=Err_labs, aes(x=xbars, ymin=lift_targ,
                                     ymax=lift_All, color=err_color), width=.02) +
    geom_text(data=Err_labs, aes(x=xlabs, y=(lift_targ + lift_All) / 2,
                                 label=err_label, color=err_color),
              size=6, hjust=0) +
    scale_color_identity() +
    scale_linetype_manual(values=c(A=2, B=4), guide='none') +
    scale_shape("Ad", guide='none') +
    scale_x_continuous(breaks=c(0, 1)) +
    scale_y_continuous(breaks=0) +
    ##    coord_cartesian(xlim=c(-.1, 1.1),ylim=c(-.02, min(1, 1.1 * max(AB_labs$y))), expand=TRUE) +
    coord_cartesian(xlim=c(-.15, 1.15), ylim=c(-.02, min(1, 1.1 * max(AB_labs$y))), expand=TRUE) +
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid.minor.y=element_blank(),
     ##     panel.grid.major.y=element_blank(),
      ##    panel.background=element_rect(color='blue')
          )

  return(P1)
}
