library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(leaflet)
library(tictoc)

siteCoor <<- readRDS("siteCoordsFloodStage.rds")
histDaily <<- readRDS("histDailyFlow_FormattedPoints.rds")
histFlowTab <<- readRDS("histFlowTable.rds")

navbarPage(
    title = 'Stage Tracker',
    id = 'x0',
    tabPanel('Tabular',
             fluidRow(
                 column(3,
                        wellPanel(
                            h4("Initialize", font="bold"),
                            ## sliderInput("dSlide","Days Prior", min=0, max=21, value=1),
                            numericInput("dNum","Days Prior", min=0, max=21, value=0),
                            hr(),
                            h4("Get Most Recent Data"),
                            actionButton("updData", "Refresh"),
                            hr(),
                            h4("Map Options"),
                            radioButtons(inputId="mapDef",
                                         label="Map Flood Frac.",
                                         choices=c("Stage", "Flood Fraction"))
                        )
                        ),
                 column(9,
                        h3("Most Recent Conditions", align="center"),
                        leafletOutput("mapy")
                        )
             ),
             hr(),
             DT::dataTableOutput('x1'))
)


