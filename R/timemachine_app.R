#' The time machine app
#'
#' @param ... no entries suported here
#' @return shiny app
#'
#' @export
timemachine_app <- function(...) {

ui <- fluidPage(
    tabsetPanel(
#-------------------------------------------------------------------------------
# Tab panel 1
#-------------------------------------------------------------------------------
        tabPanel(
            "Trends",
            theme = shinythemes::shinytheme("united"),
            titlePanel("Past climate explorer"),
            fluidRow(
                column(
                    width = 12,
                    fluidRow(
#-------------------------------------------------------------------------------
# Overview plot
#-------------------------------------------------------------------------------
                        column(
                            h4("Overview"),
                            p(em("Select an area on this plot to zoom in on an
                                 interval."
                                 )
                              ),
                            width = 5,
                            plotOutput(
                                "plot1",
                                height = 300,
                                brush = brushOpts(
                                    id = "plot1_brush",
                                    resetOnNew = TRUE
                                    )
                                ),
                            plotOutput(
                                "chrono1",
                                height = 100
                                ),
                            fig_cap
                            ),
#-------------------------------------------------------------------------------
# Legend
#-------------------------------------------------------------------------------
                        column(
                            width = 2,
                            plotOutput("legend1", height = 100),
                            br(),
                            br(),
                            br(),
                            br(),
                            time_legbox
                            ),
#-------------------------------------------------------------------------------
# Zoom plot
#-------------------------------------------------------------------------------
                        column(
                            h4("Zoom"),
                            class = "well",
                            width = 5,
                            plotOutput("plot2", height = 300),
                            plotOutput("chrono2", height = 100)
                            )
                        )
                      )
                    ),
#-------------------------------------------------------------------------------
# References
#-------------------------------------------------------------------------------
                     ref_row
        ),
#-------------------------------------------------------------------------------
# Tab panel 2
#-------------------------------------------------------------------------------
    tabPanel(
        "Transients",
        theme = shinythemes::shinytheme("united"),
        titlePanel("A closer look at climate change"),
        fluidRow(
            column(width = 4),
            column(
                width = 4,
                radioButtons("events", "Select an interval", transients,
                             inline = TRUE
                             )
                ),
            column(
                width =4
                )
            ),
#-------------------------------------------------------------------------------
# Overview plot
#-------------------------------------------------------------------------------
        fluidRow(
            sidebarPanel(
                h4("Model rate of climate change"),
                p(em("The rate of climate change captured in a mathematical
                     expression."
                     )
                  ),
                br(),
                actionButton("simulate", "Fit curve"),
                br(),
                br(),
                textOutput("model_txt"),
                uiOutput("model_formula"),
                br(),
                p(em("Select an area on the fitted curve and click on button to
                     calculate rate."
                     )
                  ),
                br(),
                actionButton("calculate", "Calculate rate of change"),
                br(),
                br(),
                textOutput("avg_rate")
                ),
#-------------------------------------------------------------------------------
# Text and legend
#-------------------------------------------------------------------------------
            column(
                width = 4,
                htmlOutput("text1")
                ),
#-------------------------------------------------------------------------------
# Plot
#-------------------------------------------------------------------------------
            column(
                width = 4,
                plotOutput("plot3",
                           brush = brushOpts(
                               id = "plot3_brush",
                               resetOnNew = TRUE
                               )
                           ),
                plotOutput("chrono3", height = 100),
                fig_cap
                )
            ),
#-------------------------------------------------------------------------------
# References
#-------------------------------------------------------------------------------
            ref_row
        )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

# ------------------------------------------------------------------------------
# Linked plots
#-------------------------------------------------------------------------------
    # reactive values starters
    ranges1 <- reactiveValues(x = NULL, y = NULL)
    ranges2 <- reactiveValues(x = NULL, y = NULL)


    strat_plot1 <- reactive({
        chrono_bldr(
            time_plot(timemachine::temp_curve, Age, Proxy),
            capture_legend = TRUE
            )
    })

    proxy1 <- reactive(strat_plot1() %>% purrr::pluck("original"))
    chrono1 <- reactive(strat_plot1() %>% purrr::pluck("chrono") %>% plot)
    legbox <- reactive(strat_plot1() %>% purrr::pluck("legbox") %>% plot)

#-------------------------------------------------------------------------------
# Overview plot
#-------------------------------------------------------------------------------
    output$plot1 <- renderPlot({proxy1()})
    output$chrono1 <- renderPlot({chrono1()})
    output$legend1 <- renderPlot({legbox()})

#-------------------------------------------------------------------------------
# Zoom plot
#-------------------------------------------------------------------------------
    strat_plot2 <- reactive(
        chrono_bldr(
            time_plot(timemachine::temp_curve, Age, Proxy, explain = TRUE,
                      range_sh = ranges1$x
                      ) +
                coord_cartesian(
                    xlim = rev(ranges1$x),
                    ylim = ranges1$y,
                    expand = FALSE
                    ) +
                theme(
                    plot.background =
                        element_rect(
                          fill = bkgr,
                          color = bkgr
                         ),
                    panel.background =
                        element_rect(
                          fill = bkgr,
                          color = bkgr
                          )
                        ),
            reverse = TRUE
            )
        )

    proxy2 <- reactive(strat_plot2() %>% purrr::pluck("original"))
    chrono2 <- reactive(strat_plot2() %>% purrr::pluck("chrono") %>% plot)

#-------------------------------------------------------------------------------
# Brush 1
#-------------------------------------------------------------------------------
    observeEvent(input$plot1_brush,{
        output$plot2 <- renderPlot({proxy2()})
        output$chrono2 <- renderPlot({chrono2()})
        }
        )

#-------------------------------------------------------------------------------
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
#-------------------------------------------------------------------------------
    observe({
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges1$x <- c(brush$xmin, brush$xmax)
            ranges1$y <- c(brush$ymin, brush$ymax)

        } else {
            ranges1$x <- NULL
            ranges1$y <- NULL

        }
    }
    )

#-------------------------------------------------------------------------------
# Event data selection and curve fit preperation
#-------------------------------------------------------------------------------
    data_select <- reactive({

        switch(
            input$events,
            PETM = filter(timemachine::temp_curve,
                          between(.data$Age, 55.835, 56.135)
                          ),
            worst = filter(timemachine::temp_curve, .data$Age < 10^-3,
                           .data$scenario == "0" |
                               .data$scenario == "2"
                           ),
            best = filter(timemachine::temp_curve,  .data$Age < 10^-3,
                          .data$scenario == "0" |
                              .data$scenario == "1"
                          )
            )
        }
        )

    strat_plot3 <- reactive({
        chrono_bldr(time_plot(data_select(), Age, Proxy, events = FALSE))
        }
        )

    # fit a model
    fit <- reactive({curve_fit(data_select(), Age, Proxy)})

    # make a line
    curve <- reactive({
        lst(geom_line(
                  data = purrr::pluck(fit(), "df") ,
                  aes(x = .data$Age, y = .data$Proxy),
                  color = "red",
                  size = 1.1,
                  inherit.aes = FALSE
                  )
            )
        }
        )


#-------------------------------------------------------------------------------
# Transient overview plot
#-------------------------------------------------------------------------------
    proxy3 <- reactive({strat_plot3() %>% purrr::pluck("original") +
                           theme(legend.position = "top")
                        })
    chrono3 <- reactive(strat_plot3() %>% purrr::pluck("chrono") %>% plot)
    output$chrono3 <- renderPlot({chrono3()})

#-------------------------------------------------------------------------------
# Explanatory text of transient
#-------------------------------------------------------------------------------

    output$text1 <- renderText({
        switch(
            input$events,
            PETM = print(PETM_txt),
            worst = print(scenario2_txt),
            best = print(scenario1_txt)
            )
    }
    )

#-------------------------------------------------------------------------------
# Curve fit exercise
#-------------------------------------------------------------------------------

    # make a formula
    observeEvent(input$simulate, {
        output$model_formula <- renderUI({withMathJax(purrr::pluck(fit(),
                                                                  "form")
                                                     )
                                          })
        output$model_txt <- renderText({
            switch(purrr::pluck(fit(), "sel_mdl"),
                   model_lm = LC_txt,
                   model_exp = JC_txt,
                   model_logistic = SC_txt
                   )
                   })
        output$plot3 <- renderPlot({proxy3() + curve()})
        })

   # calculate rates
   rate_estimate <- reactive({
       if (is.null(ranges2$x) |button2$result == FALSE) {
           return()
       } else {
           period <- diff(ranges2$x) * 10 ^ 6 # year
           temp <- diff(ranges2$y)  # degree Celsius
           rate <- temp / period
           return(paste0(sprintf("%.3g", rate), " \u00B0 C / year"))
       }
       })

    output$avg_rate <- renderText({rate_estimate()})

#-------------------------------------------------------------------------------
# Remove Curve fit and model outcome upon switching event
#-------------------------------------------------------------------------------
    observeEvent(input$events, {
        output$plot3 <- renderPlot({proxy3()})
        output$model_formula <- renderUI(NULL)
        output$model_txt <- renderText(NULL)
    })

#-------------------------------------------------------------------------------
# reactive value to flip action buttion number two back to original state
#-------------------------------------------------------------------------------
    button2 <- reactiveValues(result = FALSE)

    observeEvent(input$calculate, {
        # 0 will be coerced to FALSE
        # 1+ will be coerced to TRUE
        button2$result <- input$calculate
    })

    observeEvent(input$events, {
        button2$result <- FALSE
    })

#-------------------------------------------------------------------------------
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
#-------------------------------------------------------------------------------
    observeEvent(input$plot3_brush, {
        brush2 <- brushedPoints(purrr::pluck(fit(), "df"), input$plot3_brush)
        if (!is.null(brush2)) {
            ranges2$x <- c(min(brush2$Age) , max(brush2$Age))
            ranges2$y <-c(min(brush2$Proxy) , max(brush2$Proxy))
        } else {
            ranges2$x <- NULL
            ranges2$y <- NULL
        }
        }
    )

    }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Run the application
shinyApp(ui = ui, server = server)

}


#-------------------------------------------------------------------------------
# Not exportet
#-------------------------------------------------------------------------------

# labeller for transients
transients <- rlang::set_names(
    c("PETM", "worst", "best"),
    nm = c(
        "PETM",
        "Anthropocene (worst case)",
        "Anthropocene (best case)"
        )
        )

# climate text
PETM_txt <- HTML(paste0("The Paleocene-Eocene Thermal Maximum (PETM; ~56 Ma) is
                        one of the most studied geological intervals of extreme
                        climate change. This particular event is associated with
                        rapidly increasing global temperatures, known as a
                        hyperthermal. The temperature rise has been attributed
                        to the melting of methane stored in the seabed in an
                        ice-like state and/or CO", tags$sub("2"), " release by
                        massive volcanism. This makes it an interesting interval
                        to compare with the modern situation."
                        )
                 )
scenario1_txt <- HTML(paste0("The Anthropocene (> 1850 AD = 0.1 ka on plot) is
                             the latest geological interval. This interval is
                             coined after the unprecedented footprint left by
                             humankind (anthro = human) on the Earth system as a
                             whole. The depicted modelled future projection is
                             the most optimistic scenario of human-induced
                             climate change. These scenario are callled
                             Representative Concentration Pathways (RCPs), where
                             the number represents the relative to
                             pre-industrial increase of radiative forcing in
                             watts per metre squared. In this optimistic
                             scenario, known as RCP2.6, it is suggested that
                             humankind can come-up with solutions to curb the
                             injection of fossil carbon, and emissions are
                             14% to 96% of what they were in 1990 AD by the year
                             2050 AD", tags$sup("6,"), tags$sup("7"), "."
                             )
                      )
scenario2_txt <- HTML(paste0("The Anthropocene (> 1850 AD = 0.1 ka on plot) is
                             the latest geological interval. This interval is
                             coined after the unprecedented footprint left by
                             humankind (anthro = human) on the Earth system as
                             a whole. The depicted modelled future projection is
                             the most pesimistic scenario of human-induced
                             climate change. These scenario are callled
                             Representative Concentration Pathways (RCPs), where
                             the number represents the relative to
                             pre-industrial increase of radiative forcing in
                             watts per metre squared. In this pestimistic
                             scenario, known as RCP8.5, often referred to as
                             \"business as usual\", it is suggested that humankind
                             will do nothing to reduce emmisions as we exploit
                             more-and-more of the fossil fuel reserves",
                             tags$sup("6,"), tags$sup("7"), "."
                             )
                      )

# time unit legends
time_legbox <- fluidRow(
    h6("Ma = million years before present"),
    h6("ka = kilo years before present"),
    h6("a = years before present"),
    h6(em("\"a\" stands for the Latin nominative singular \"annus\"")),
    h6(em("\"before present\" refers to before 1950 anno Domini (AD)"))
    )

# math text
LC_txt <- "Linear curve: Has a constant rate of change."
JC_txt <- "The J (exponential) curve: Has a rate of change that is proportional
          to the time unit, causing unbounded acceleration."
SC_txt <- "The S (logistic) curve: The exponential curve is bounded by an upper
          limit."

# references
ref1 <- h6("1) Westherhold et al., 2020. An astronomically dated record of
           Earth's climate and its predictability over the last 66 million
           years. Science 369: 1383-1388 (PANGAEA DOI:",
           a(href=paste0("https://doi.pangaea.de/10.1594/PANGAEA.917503"),
             "10.1594/PANGAEA.917503"), ")"
           )
ref2 <- h6("2) Marcott et al., 2013. A reconstruction of regional and global
           temperature for the past 11,300 years. Science 339: 1198-1201"
           )
ref3 <- h6("3) Climatic Research Unit (University of East Anglia) and Met
           Office"
           )
ref4 <- h6(a(href="https://climate4impact.eu/", "4) Climate4impact"))
ref5 <- h6("5) Hansen et al., 2013. Climate sensitivity, sea level and
           atmospheric carbon dioxide. Philosophical Transactions of the Royal
           Society A: Mathematical, Physical and Engineering Sciences 371: 1-31"
           )
ref6 <- h6(a(href="https://www.wcrp-climate.org/", "6) World Climate Research
             Program"
             )
           )
ref7 <- h6("7) IPCC, 2013: Climate Change 2013: The Physical Science Basis.
           Contribution of Working Group I to the Fifth Assessment Report of
           the Intergovernmental Panel on Climate Change [Stocker, T.F., D.
           Qin, G.-K. Plattner, M. Tignor, S.K. Allen, J. Boschung, A. Nauels,
           Y. Xia, V. Bex and P.M. Midgley (eds.)]. Cambridge University Press,
           Cambridge, United Kingdom and New York, NY, USA, 1535 pp."
           )

# Fig caption
fig_cap <- tags$figcaption(
    HTML(
        paste0("Data sources; sediments (benthic foraminifera \u03B4",
               tags$sup("18"), "O from ref. 1 and composite curve from ref.
               2) instrumental (HadCRUT4)", tags$sup("3"), ", model (BCC_CM1)",
               tags$sup("4"), ". ", "\u03B4",tags$sup("18"), "O conversion to
               temperature after ref 5."
        )
    )
)

ref_row <- fluidRow(
    HTML('<hr style="color: purple;">'),
    column(
        width = 12,
        h4("References"),
        ref1, ref2, ref3, ref4, ref5, ref6, ref7
    )
)

# zoom plot plot background
bkgr <- rgb(241, 241, 241, maxColorValue = 251)

