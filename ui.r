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
                            h4("Initialize", font="bold"),
                            sliderInput("dSlide","Days Prior", min=0, max=14, value=1),
                            hr(),
                            h4("Get Most Recent Data"),
                            actionButton("updData", "Refresh")
                        )
                        ),
                 column(9,
                        leafletOutput("mapy")
                        )
             ),
             hr(),
             DT::dataTableOutput('x1'))
)


