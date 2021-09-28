
#' @title bias_plot
#' @description create a bias plot
#' @param pt_idx value of pt that will be highlighted in the plot
bias_plot <- function(pt_idx, ...) {

  x <- c(2^seq(-4,4, length=20), pt_idx)
  data <- F_bias(pt=x, ...)

  args <- list(...)

  D2 <- data %>%
    pivot_longer(cols = c('Err_A', 'Err_B', 'bias'), names_to='stat',
                 values_to='value') %>%
    mutate(stat=fct_recode(factor(stat, levels=c("Err_A", "Err_B", "bias")),
                           "Error Ad A"="Err_A",
                           "Error Ad B"="Err_B",
                           "Bias"="bias"))

  R <- D2 %>%
    filter(pt == pt_idx)

  P <- D2 %>%
    ggplot(aes(x=pt,y=value, color=stat)) +
    geom_line(size=1, alpha=.6) +
    geom_vline(xintercept=pt_idx, linetype=2) +
    geom_hline(yintercept=0) +
    geom_vline(xintercept=1) +
    geom_point(data=R, aes(x=pt, y=value, color=stat),
               shape='circle', size=5, stroke=1) +
    scale_x_continuous(expression(pi[tau]), trans='log2',
                       breaks=c(1 / 8, 1 / 4, 1 / 2, 1, 2, 4, 8),
                       labels=MASS::fractions) +
    scale_y_continuous("bias", breaks=c(0, as.numeric(R$value)),
                       labels=scales::label_number(.001),
                       limits = c(-.15, .15)) +
    scale_color_manual(values=c('Error Ad A'=plot_colors$err_A,
                                'Error Ad B'=plot_colors$err_B,
                                Bias='gray50'), guide=guide_none()) +
    facet_grid(cols=vars(stat)) +
    theme(text=element_text(size=16))

  return(P)

}
