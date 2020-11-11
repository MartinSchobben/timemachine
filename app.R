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
indepth <- c("middle Eocene", "PETM", "anthropocene")



ui <- fluidPage(
    theme = shinythemes::shinytheme("united"),
    titlePanel("Climate of the past"),
    fluidRow(
    column(
        width = 12,
        fluidRow(
            column(
                h4("Overview"),
                width = 6,
                plotOutput(
                    "plot1",
                    height = 300,
                    brush = brushOpts(id = "plot2_brush", resetOnNew = TRUE)
                    ),
                plotOutput(
                    "chrono1",
                    height = 100
                    )
                ),
            column(
                h4("Zoom"),
                width = 6,
                plotOutput("plot2", height = 300),
                plotOutput("chrono2", height = 100)
                )
            )
        )
    ),
    fluidRow(
        checkboxGroupInput("clim", "A closer look at climate change", indepth, inline = TRUE)
        ),
   sidebarLayout(
            sidebarPanel(

            ),
            mainPanel(
                plotOutput("distPlot")
            )
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

# ------------------------------------------------------------------------------
# Linked plots
#-------------------------------------------------------------------------------
    ranges <- reactiveValues(x = NULL, y = NULL)

    strat_plot1 <- reactive(chrono_bldr(time_plot(temp_curve, Age, Proxy)))

    proxy1 <- reactive(strat_plot1() %>% pluck("original"))
    chrono1 <- reactive(strat_plot1() %>% pluck("chrono") %>% plot)

    output$plot1 <- renderPlot({proxy1()})
    output$chrono1 <- renderPlot({chrono1()})


    strat_plot2 <- reactive(
        chrono_bldr(
            time_plot(temp_curve, Age, Proxy) +
                coord_cartesian(xlim = rev(ranges$x), ylim = ranges$y, expand = FALSE)
            )
        )

    proxy2 <- reactive(strat_plot2() %>% pluck("original"))
    chrono2 <- reactive(strat_plot2() %>% pluck("chrono") %>% plot)

    output$plot2 <- renderPlot({proxy2()})
    output$chrono2 <- renderPlot({chrono2()})

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




    }

# Run the application
shinyApp(ui = ui, server = server)
