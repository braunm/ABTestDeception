
server <- function(input, output, session) {

  targ <- reactiveValues(at=1, pt=1, rt=1.00001)
  Y <- reactiveValues( ay=1, py=1, ry=1.00001)

  observe({
    targ$at <- eval(parse(text=input$at))
    targ$pt <- eval(parse(text=input$pt))
    targ$rt <- ifelse(input$rt == 1,
                      1.00001, eval(parse(text=input$rt)))
  })

  observe({
    Y$ay <- eval(parse(text=input$ay))
    Y$py <- eval(parse(text=input$py))
    Y$ry <- ifelse(input$ry == 1,
                   1.00001, eval(parse(text=input$ry)))
  })

  AA <- reactive({F_all(targ$at, targ$pt, targ$rt, input$Mt,
                        Y$ay, Y$py, Y$ry, input$My,
                        input$gamma, input$zeta, input$EY0P, input$EY0Q)})

  tmp <- reactive({
    AA()$XZ %>%
    dplyr::select(X, Z, post, lift) %>%
      pivot_wider(names_from=X, values_from=c(post, lift))
  })

  output$mix_plot <- renderPlot({
    mix_plot2(AA()$XZ, AA()$all, input$gamma, input$My)
  })


  output$areaTarg <- renderPlot({
    egg_plot2(AA()$XZ, input$gamma, input$zeta, col='PrTargXZ')
  })

  output$areaEY <- renderPlot({
    egg_plot2(AA()$XZ, input$gamma, input$zeta, col='EY')
  })

  output$bias_plot <- renderPlot({


    bias_plot(at=targ$at, pt_idx=targ$pt, rt=targ$rt,
              ay=Y$ay, py=Y$py, ry=Y$ry,
              Mt=input$Mt, My=input$My,
              g=input$gamma, zeta=input$zeta)
  })

  output$targ_table <- function() {
    AA()$XZ %>%
      dplyr::select(X, Z, PrTargXZ) %>%
      pivot_wider(names_from=Z, values_from = PrTargXZ,
                  names_prefix = "Ad ") %>%
        mutate(X= fct_recode(factor(X), Poets='P', Quants='Q', All='All'))  %>%
        rename("User type"="X") %>%
        knitr::kable("html", align = 'lcc', digits=c(0, 2, 2)) %>%
        kableExtra::kable_styling() %>%
        kableExtra::row_spec(1, color=plot_colors$P) %>%
        kableExtra::row_spec(2, color=plot_colors$Q) %>%
        kableExtra::add_header_above(c(" ", "being targeted"=2)) %>%
        kableExtra::add_header_above(c(" ", "Probability of"=2))
  }

  ## Full Audience
  output$EY_table <- function() {
    AA()$XZ %>%
      dplyr::select(X, Z, EY) %>%
      pivot_wider(names_from=Z, values_from = EY,
                  names_prefix = "Ad ") %>%
        mutate(X= fct_recode(factor(X), Poets='P', Quants='Q', All='All'))  %>%
        rename("User type"="X") %>%
        knitr::kable("html", align = 'lcc', digits=c(0, 2, 2)) %>%
        kableExtra::kable_styling() %>%
        kableExtra::row_spec(1, color=plot_colors$P) %>%
        kableExtra::row_spec(2, color=plot_colors$Q) %>%
        kableExtra::add_header_above(c(" ", "Conversion Rate"=2)) %>%
        kableExtra::add_header_above(c(" ", "Audience"=2))
  }

  output$agg_table <- function() {


    tmp <- AA()$all %>%
         dplyr::select(Ad=Z, Targ=lift_targ, Aud=lift_ATE) %>%
         mutate(Err=Targ - Aud)


    tmp %>%
      add_row(Ad='Bias',Targ=AA()$Delta_ATE,
              Aud=AA()$Delta_targ,
              Err=AA()$bias) %>%
      kable('html', align='crrr', digits=c(0, 2, 2, 2)) %>%
      kable_styling('condensed') %>%
      row_spec(1, color=plot_colors$err_A) %>%
      row_spec(2, color=plot_colors$err_B) %>%
      add_header_above(c("","lift"=2, ""))


  }

}
