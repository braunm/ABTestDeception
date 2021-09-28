

#' @title make an egg plot
#' @param XZ data
#' @param g prior
#' @param zeta zeta
#' @param col column of XZ being used for the eggs (either PrTargXZ or EY?)
egg_plot2 <- function(XZ, g, zeta, col='PrTargXZ') {

  gx <- 10 * (2 * (1 - g) - 1)
  hx <- 10 * (2 * zeta - 1)

  fmt0 <- scales::label_number(1)

  q1 <- XZ %>%
    filter(X != "All") %>%
    select(X, Z, {{col}}) %>%
    pivot_wider(names_from=X, values_from = {{col}}) %>%
    mutate(PrZ=c(zeta, 1 - zeta)) %>%
    rowwise() %>%
    group_map(function(d, ...) {
      oval_dims(d$P, d$Q, Mt, d$Z, d$PrZ, g) %>%
        mutate(angle=0)
    }, .keep=TRUE) %>%
    bind_rows() %>%
    mutate(axislab=str_c("Ad ", Z, " (", fmt0(100 * ifelse(Z == 'A', zeta, 1 - zeta)), "%)"))


  q2 <- q1 %>%
    rowwise() %>%
    group_map(~func_ellipse(.x))

  labX <- tibble(Xname=c('Poets', 'Quants'),
                 PrX=c(g, 1 - g),
                 lab=str_c(Xname, " (", fmt0(100 * PrX), "%)"),
                 y0=(c(1, -1) * 10 + gx) / 2)

  P  <- ggplot() +
    geom_ribbon(data=q2[[1]], aes(x=x, ymin=pmax(gx, .data$yU),ymax=pmax(gx, .data$yL)),
                fill=plot_colors$P, alpha=1) +
    geom_ribbon(data=q2[[1]], aes(x=x, ymin=pmin(gx, .data$yU),ymax=pmin(gx, .data$yL)),
                fill=plot_colors$Q, alpha=1) +
    geom_ribbon(data=q2[[2]], aes(x=x, ymin=pmax(gx, .data$yU),ymax=pmax(gx, .data$yL)),
                fill=plot_colors$P, alpha=1) +
    geom_ribbon(data=q2[[2]], aes(x=x, ymin=pmin(gx, .data$yU),ymax=pmin(gx, .data$yL)),
                fill=plot_colors$Q, alpha=1) +
    scale_x_continuous(breaks = seq(-10, 10, by=1)) +
    scale_y_continuous(breaks = seq(-10, 10, by=1)) +
    annotate('rect', xmin= -10, xmax=hx - .05, ymin=gx + .05, ymax=10, color=plot_colors$P, alpha=0, size=1) +
    annotate('rect', xmin= hx + .05, xmax=10, ymin=gx + .05, ymax=10, color=plot_colors$P, alpha=0, size=1) +
    annotate('rect', xmin= -10, xmax=hx - .05, ymin=gx - .05, ymax= -10, color=plot_colors$Q, alpha=0, size=1) +
    annotate('rect', xmin= hx + .05, xmax=10, ymin=gx - .05, ymax= -10, color=plot_colors$Q, alpha=0, size=1) +
    geom_text(data=q1, aes(x=x0, y=11, label=axislab)) +
    geom_text(data=labX, aes(x= -11, y=y0, label=lab, color=Xname), angle=90) +
    scale_color_manual(values=c(Poets=plot_colors$P, Quants=plot_colors$Q), guide=guide_none()) +
    coord_fixed(ratio=1, xlim=c(-12, 11), ylim=c(-11, 12), expand = FALSE) +
    theme(axis.text=element_blank(),
          axis.title=element_blank())
return(P)

}
