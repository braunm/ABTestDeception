options(dplyr.summarise.inform=FALSE)

thematic::thematic_shiny(font='auto')

fmt3 <- scales::label_number(.001)
fmt2 <- scales::label_number(.01)
fmt0 <- scales::label_number(1)

app_theme <-  bslib::bs_theme(version="5")

ggplot2::theme_set(plot_theme())


## Compile Sass into css file (this just seems to work better than bslib's functions.)
bs_path <- stringr::str_c(.libPaths(), "/bslib/lib/bs5/scss/") ## but use bslib-provided Bootstrap
sass::sass(input=sass::sass_file("sass/base.scss"),
     output="www/custom.css",
     options=sass::sass_options(include_path = bs_path),
     cache=FALSE)
