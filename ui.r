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
             leafletOutput("mapy"),
             hr(),
             fluidRow(
                 column(1),
                 column(2,
                        actionButton("resetButton", "Reset with n days")
                        ),
                 column(5,
                        numericInput("daysBack",
                                     "Days of data to fetch prior to today:",
                                     0, min=0, max=30, step=1)
                        ),
                 column(2,
                        actionButton("refButton", "Refresh")
                        )
             ),
             DT::dataTableOutput('x1'))
)


