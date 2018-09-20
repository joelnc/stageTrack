library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(leaflet)

siteCoor <<- readRDS("siteCoords.rds")

navbarPage(
    title = 'Stage Tracker',
    id = 'x0',
    tabPanel('Tabular',
             leafletOutput("mapy"),
             hr(),
             fluidRow(
                 column(1),
                 column(2,
                        actionButton("go", "Get Data")
                        ),
                 column(5,
                        numericInput("daysBack", "Days of data to fetch prior to today:", 0, min=0,
                                     max=30, step=1)
                        )
             ),
             DT::dataTableOutput('x1')),
    tabPanel("Mapular"#,
             ## leafletOutput("mapy")
             )
)


