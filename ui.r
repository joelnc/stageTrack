library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(leaflet)
library(tictoc)

siteCoor <<- readRDS("siteCoords.rds")
histDaily <<- readRDS("histDailyFlow_Formatted.rds")

navbarPage(
    title = 'Stage Tracker',
    id = 'x0',
    tabPanel('Tabular',
             fluidRow(
                 column(3,
                        wellPanel(
                            numericInput("daysBack", "Days of data to fetch prior to today:", 0, min=0,
                                     max=30, step=1),
                            actionButton("go", "Initialize Table"),
                            br(),
                            actionButton("refrData", "Refresh"),
                            checkboxInput("autoRef", "Auto. Refresh?")
                        )
                        ),
                 column(9,
                        leafletOutput("mapy")
                        )
             ),
             hr(),
             DT::dataTableOutput('x1'))
)


