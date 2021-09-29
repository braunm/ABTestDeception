
offcanvas <- div(class= "offcanvas offcanvas-top", tabindex="-1",
                 `data-bs-scroll`='true', `data-bs-backdrop`='false',
                 id="settings",
                 div(class="offcanvas-header",
                     tags$button(type="button", `data-bs-dismiss`="offcanvas",
                                 class="btn-close text-reset"),
                     h5(class="offcanvas-title", id="settingsLabel", "Additional Settings")
                     ),
                 div(class="offcanvas-body",
                     div(class='ratio-sliders',
                         this_sliderBox("My", withMathJax(sldr$My), selected=.1, width='150px'),
                         this_sliderBox("EY0P", sldr$EY0P, selected=0, width='150px'),
                         this_sliderBox("EY0Q", sldr$EY0Q, selected=0, width='150px'),
                         this_sliderBox("gamma", "$$\\gamma_P$$", selected=.5, width='150px'),
                         this_sliderBox("zeta", "$$\\zeta_A$$", selected=.5, width='150px'),
                         this_sliderBox("Mt", "$$\\tilde{\\Phi}$$", selected=.1, width='150px')
                         )
                     )
                 )

offcanvas_btn <- tags$a(class="btn btn-primary", `data-bs-toggle`="offcanvas",
                        href="#settings", role="button",
                        "Settings")



header <-     div(class='position-fixed bg-light border',
                  div(class='d-flex flex-nowrap',
                      offcanvas,
                      div(class='me-auto', offcanvas_btn),
                      div(class='me-auto', h3("Divergent Delivery and Response Heterogeneity"))),
                  div(class="row",
                      div(class='col-6 border',
                          div(class='row ratio-sliders border ',
                              div(class='d-flex flex-nowrap justify-content-center',
                                  "Ratios of targeting probabilities"),
                              this_sliderBox("at", sldr$at, selected=1, width='130px'),
                              this_sliderBox("pt", sldr$pt, selected=1, width='130px'),
                              this_sliderBox("rt", sldr$rt, selected=5, width='130px')
                              )),
                      div(class='col-6 border',
                          div(class='row ratio-sliders border',
                              div(class='d-flex flex-nowrap justify-content-center',
                                  "Ratios of expected conversion rates"),
                              this_sliderBox("ay", sldr$ay, selected=2, width='130px'),
                              this_sliderBox("py", sldr$py, selected=2, width='130px'),
                              this_sliderBox("ry", sldr$ry, selected=1, width='150px'))
                          )
                      )
                  )


body <-  div(
             div(class="row",style="padding-top: 230px;",
                 div(class='col-6',
                     div(class='d-flex flex-wrap align-items-start justify-content-center',
                         plotOutput("areaTarg", height='300px', width='50%'),
                         tableOutput("targ_table")
                         )
                     ),
                 div(class='col-6',
                     div(class='d-flex flex-wrap align-items-start justify-content-center',
                         plotOutput("areaEY", height='300px', width='50%'),
                         tableOutput("EY_table")
                         )
                     )
                 ),
                 div(class="d-flex flex-wrap align-items-center px-0 mx-0 border",
                     plotOutput("mix_plot")
                 ),
             div(class='d-flex flex-wrap align-items-start justify-content-around border',
                 tableOutput("agg_table"),
                 plotOutput("bias_plot", width="70%")
                 )
             )


ui <- bootstrapPage(
  theme=app_theme(),
  div(class='container-md justify-contents-center',
      header,
      div(class='container-md justify-content-center',
          body
          )
      )
)


  ## tags$link(
  ##     rel='stylesheet',
  ##     href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.0/dist/css/bootstrap.min.css",
  ##     integrity="sha384-KyZXEAg3QhqLMpG8r+8fhAXLRk2vvoC2f3B09zVXn8CA5QIVfZOJ3BCsw2P0p/We",
  ##     crossorigin="anonymous"
  ##   )
