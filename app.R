#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
library(miscutils)
temp_curve <- readRDS("data/temp_curve.RDS") # get temp data
source("functions/timeplot.R") # get plot function
source("functions/climchange_model.R") # get model function
transients <- c("PETM", "worst", "best") %>% set_names(nm = c("PETM", "Anthropocene (worst case)", "Anthropocene (best case)"))

PETM_txt <- "The Paleocene-Eocene Thermal Maximum (PETM;  56 Ma) is one of the most studied geological interval of extreme climate change. This particular event is associated with rapidly increasing global temperatures, known as a hyperthermal, which has often been associated with the melting of methane in an ice-like state stored in the seabed. It has been proposed as a future analogue."
scenario1_txt <- paste0("The Anthropocene (> 1850 AD = 0.1 Kya on plot) is the latest geological interval. This interval is coined after the unprecedented footprint left by humankind (anthro = human) on the Earth system as a whole. The depicted modelled future projection is the most optimistic scenario of human-induced climate change. These scenario are callled Representative
Concentration Pathways (RCPs), where the number represents the relative to pre-industrial increase of radiative forcing in watts per metre squared. In this optimistic scenario, known as RCP2.6, it is suggested that humankind can come-up with solutions to curb the injection of fossil carbon, and emissions are 14% to 96% of what they were in 1990 AD by the year 2050 AD.", tags$sup("5"), tags$sup("6"))
scenario2_txt <- paste0("The Anthropocene (> 1850 AD = 0.1 Kya on plot) is the latest geological interval. This interval is coined after the unprecedented footprint left by humankind (anthro = human) on the Earth system as a whole. The depicted modelled future projection is the most pesimistic scenario of human-induced climate change. These scenario are callled Representative
Concentration Pathways (RCPs), where the number represents the relative to pre-industrial increase of radiative forcing in watts per metre squared. In this pestimistic scenario, known as RCP8.5, often referred to as business as usual, it is suggested that humankind will do nothing to reduce emmisions as we exploit more-and-more of the fossil fuel reserves.", tags$sup("5"), tags$sup("6"))

#plot background
bkgr <- rgb(241, 241, 241, maxColorValue = 251)

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
                            width = 5,
                            plotOutput(
                                "plot1",
                                height = 300,
                                brush = brushOpts(
                                    id = "plot2_brush",
                                    resetOnNew = TRUE
                                    )
                                ),
                            plotOutput(
                                "chrono1",
                                height = 100
                                ),
                            tags$figcaption(
                                HTML(
                                    paste0(
                                        "Select an area on this plot to zoom in
                                        on an interval. Data sources; sediments
                                        (benthic foraminifera &delta;",
                                        tags$sup("18"), "O from ref. 1 and
                                        composite curve from ref. 2)
                                        instrumental (HadCRUT4)", tags$sup("3"),
                                        ", model (BCC_CM1)", tags$sup("4")
                                        )
                                    )
                                )
                            ),
#-------------------------------------------------------------------------------
# Legend
#-------------------------------------------------------------------------------
                        column(
                            width = 2,
                            plotOutput("legend1", height = 100)
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
            fluidRow(
                HTML('<hr style="color: purple;">'),
                column(
                    width = 12,
                    h4("References"),
                    h6("1) Westherhold et al., 2020. An astronomically dated record of Earth's climate and its predictability over the last 66 million years. Science 369: 1383-1388"),
                    h6("2) Marcott et al., 2013. A Reconstruction of Regional and Global Temperature for the Past 11,300 Years. Science 339: 1198-1201"),
                    h6("3) Climatic Research Unit (University of East Anglia) and Met Office"),
                    a(href="https://climate4impact.eu/", "4) Climate4impact")

                    )
                )

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
                radioButtons("events", "Select an interval", transients, inline = TRUE)
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
                actionButton("simulate", "Simulate!"),
                uiOutput("model_formula")
            ),
            #-------------------------------------------------------------------------------
            # Text and legend
            #-------------------------------------------------------------------------------
            column(
                width = 4,
                textOutput("text1"),
                plotOutput("legend2", height = 100)
            ),
            #-------------------------------------------------------------------------------
            # Plot
            #-------------------------------------------------------------------------------
            column(
                width = 4,
                plotOutput("plot3"),
                plotOutput("chrono3", height = 100)
                )
            ),
        #-------------------------------------------------------------------------------
        # References
        #-------------------------------------------------------------------------------
        fluidRow(
            HTML('<hr style="color: purple;">'),
            column(
                width = 12,
                h4("References"),
                h6("1) Westherhold et al., 2020. An astronomically dated record of Earth's climate and its predictability over the last 66 million years. Science 369: 1383-1388"),
                h6("2) Marcott et al., 2013. A Reconstruction of Regional and Global Temperature for the Past 11,300 Years. Science 339: 1198-1201"),
                h6("3) Climatic Research Unit (University of East Anglia) and Met Office"),
                h6(a(href="https://climate4impact.eu/", "4) Climate4impact")),
                h6(a(href="https://www.wcrp-climate.org/", "5) World Climate Research Program")),
                h6("6) IPCC, 2013: Climate Change 2013: The Physical Science Basis. Contribution of Working Group I to the Fifth Assessment Report of the Intergovern-
mental Panel on Climate Change [Stocker, T.F., D. Qin, G.-K. Plattner, M. Tignor, S.K. Allen, J. Boschung, A. Nauels, Y. Xia, V. Bex and P.M. Midgley
(eds.)]. Cambridge University Press, Cambridge, United Kingdom and New York, NY, USA, 1535 pp.")

            )
        )
        )
)
)


# Define server logic required to draw a histogram
server <- function(input, output) {

# ------------------------------------------------------------------------------
# Linked plots
#-------------------------------------------------------------------------------
    ranges <- reactiveValues(x = NULL, y = NULL)

    strat_plot1 <- reactive(chrono_bldr(time_plot(temp_curve, Age, Proxy), capture_legend = TRUE))

    proxy1 <- reactive(strat_plot1() %>% pluck("original"))
    chrono1 <- reactive(strat_plot1() %>% pluck("chrono") %>% plot)
    legbox <- reactive(strat_plot1() %>% pluck("legbox") %>% plot)

    output$plot1 <- renderPlot({proxy1()})
    output$chrono1 <- renderPlot({chrono1()})
    output$legend1 <- renderPlot({legbox()})

    strat_plot2 <- reactive(
        chrono_bldr(
            time_plot(temp_curve, Age, Proxy, explain = TRUE, range_sh = ranges$x) +
                coord_cartesian(xlim = rev(ranges$x), ylim = ranges$y, expand = FALSE) +
                theme(
                    plot.background = element_rect(
                        fill = bkgr,
                        color = bkgr
                        ),
                    panel.background = element_rect(
                        fill = bkgr,
                        color = bkgr
                        )
                        ),
            reverse = TRUE
            )
        )

    proxy2 <- reactive(strat_plot2() %>% pluck("original"))
    chrono2 <- reactive(strat_plot2() %>% pluck("chrono") %>% plot)

    observeEvent(input$plot2_brush,
                 { output$plot2 <- renderPlot({proxy2()})
                   output$chrono2 <- renderPlot({chrono2()})
                   }
                 )

#-------------------------------------------------------------------------------
# When a double-click happens, check if there's a brush on the plot.
# If so, zoom to the brush bounds; if not, reset the zoom.
#-------------------------------------------------------------------------------
    observe({
        brush <- input$plot2_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)

        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })

#-------------------------------------------------------------------------------
# events
#-------------------------------------------------------------------------------
    data_select <- reactive({

        switch(
            input$events,
            PETM = filter(temp_curve, between(Age, 55.835, 56.135)),
            worst = filter(temp_curve, Age < 10^-3, scenario == "0" | scenario == "2"),
            best = filter(temp_curve,  Age < 10^-3, scenario == "0" | scenario == "1")
            )

        })

    strat_plot3 <- reactive({
        chrono_bldr(time_plot(data_select(), Age, Proxy, events = FALSE))
        })

    # fit a model
    fit <- reactive({curve_fit(data_select(), Age, Proxy)})

    # make a line
    curve <- reactive({

        lst(geom_line(
                  data = pluck(fit(), "df") ,
                  aes(x = Age, y = .pred),
                  color = "red",
                  size = 1.1
                  ))
        })


    proxy3 <- reactive(strat_plot3() %>% pluck("original"))
    chrono3 <- reactive(strat_plot3() %>% pluck("chrono") %>% plot)


    output$plot3 <- renderPlot({proxy3()})

    observeEvent(input$simulate, {
        output$plot3 <- renderPlot({proxy3() + curve()})
    })

    output$chrono3 <- renderPlot({chrono3()})
    output$text1 <- renderText({
        switch(
            input$events,
            PETM = print(PETM_txt),
            worst = print(scenario2_txt),
            best = print(scenario1_txt)
            )
    })

    # make a formula
    observeEvent(input$simulate, {
        output$model_formula <- renderUI(withMathJax(pluck(fit(), "form")))
        })



    }

# Run the application
shinyApp(ui = ui, server = server)
