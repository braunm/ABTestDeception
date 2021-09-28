
 library(tidyverse)
## library(rlang)
## library(ggrepel)
## library(bslib)
## library(sass)
## library(shinyWidgets)
## library(kableExtra)
## library(showtext)
## library(scales)
## library(thematic)

options(dplyr.summarise.inform=FALSE)

thematic::thematic_shiny(font='auto')

## possible values for slider
## steps <- MASS::fractions(c(1 / 6, 1 / 5, 1 / 4, 1 / 3, 1 / 2, 1, 2, 3, 4, 5, 6))

## steps_at <- MASS::fractions(c(1 / 2, 4 / 7, 2 / 3, 4 / 5, 1, 5 / 4, 3 / 2, 7 / 4, 2))
## steps_ay <- MASS::fractions(c(1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3, 1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4))

## steps_pt <- MASS::fractions(c(1 / 8, 2 / 15, 1 / 7, 2 / 13, 1 / 6, 2 / 11, 1 / 5, 2 / 9, 1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3,
##                               1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4, 9 / 2, 5, 11 / 2, 6, 13 / 2, 7, 15 / 2, 8))
## steps_py <- MASS::fractions(c(1 / 8, 2 / 15, 1 / 7, 2 / 13, 1 / 6, 2 / 11, 1 / 5, 2 / 9, 1 / 4, 2 / 7, 1 / 3, 2 / 5, 1 / 2,2 / 3,
##                               1, 3 / 2, 2, 5 / 2, 3, 7 / 2, 4, 9 / 2, 5, 11 / 2, 6, 13 / 2, 7, 15 / 2, 8))



## steps_rt <- MASS::fractions(c(1 / 8, 1 / 7, 1 / 6, 1 / 5, 1 / 4, 1 / 3, 1 / 2, 1:8))
## steps_ry <- MASS::fractions(c(1 / 8, 1 / 7, 1 / 6, 1 / 5, 1 / 4, 1 / 3, 1 / 2, 1:8))

## steps_gamma <- seq(.3, .7, by=.05)
## steps_zeta <- seq(.3, .7, by=.05)
## steps_Mt <- seq(.05, .25, by=.05)
## steps_My <- seq(.05, .25, by=.05)
## steps_EY0P <- seq(0, .05, by=.005)
## steps_EY0Q <- seq(0, .05, by=.005)


## fmt3 <- scales::label_number(.001)
## fmt2 <- scales::label_number(.01)
## fmt0 <- scales::label_number(1)

## smu_blue <- '#354c97'
## smu_red <- '#cc0000'

## princeton_orange <- rgb(231, 117, 1, maxColorValue=255)
## smu_teal <- rgb(89, 195, 195, maxColorValue = 255)


## plot_colors <- list(P=smu_red,
##                  Q=smu_blue,
##                  err_A=smu_teal,
##                  err_B=princeton_orange
##                  )


## get_PQ_mix <- colorRamp(c(plot_colors$P,plot_colors$Q)) ## a function on (0,1) for amount of red

## smu_purple <-  rgb(get_PQ_mix(.5), max=255)



## gray <- c('#f8f9fa', '#e9ecef', '#dee2e6', '#ced4da', '#adb5bd',
##           '#6c757d', '#495057', '#343a40', '#212529')




app_theme <-  bslib::bs_theme(version="5")

plot_theme <- theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        text=element_text(size=16),
        plot.margin=margin())

theme_set(plot_theme)


## Compile Sass into css file (this just seems to work better than bslib's functions.)
bs_path <- str_c(.libPaths(), "/bslib/lib/bs5/scss/") ## but use bslib-provided Bootstrap
sass::sass(input=sass::sass_file("sass/base.scss"),
     output="www/custom.css",
     options=sass::sass_options(include_path = bs_path),
     cache=FALSE)
