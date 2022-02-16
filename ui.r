library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(leaflet)
library(tictoc)

siteCoor <- readRDS("siteCoordsFloodStage.rds")
histDaily <<- readRDS("histDailyFlow_FormattedPoints.rds")
histFlowTab <<- readRDS("histFlowTable.rds")

siteCoor <<- siteCoor[-c(37,38),]

## rm(list=ls())
## siteCoor <<- readRDS("stageTrack/siteCoordsFloodStage.rds")
## histDaily <<- readRDS("stageTrack/histDailyFlow_FormattedPoints.rds")
## histFlowTab <<- readRDS("stageTrack/histFlowTable.rds")

navbarPage(
    title = 'Stage Tracker',
    id = 'x0',
    tabPanel('Tabular',
             includeCSS("styles.css"),
             fluidRow(
                 column(3,
                        wellPanel(
                            h4("Initialize", font="bold"),
                            ## sliderInput("dSlide","Days Prior", min=0, max=21, value=1),
                            ## numericInput("dNum","Days Prior", min=0, max=21, value=0),
                            selectInput("dNum","Days Prior (select one, then wait)",
                                        choices=c(0,1,2,3,4,5,6,7),
                                        selected=0),           
                            HTML('<button data-toggle="collapse" data-target="#demo2" class="button" style="horizontal-align:middle"><span><b>About</b></span></button>'),
                            tags$div(id = 'demo2',  class="collapse",
                                     h6("Graphs embedded in the table below will initially include only data from today.  Increasing Days Prior will increase the period of record shown in the graphs.  Setting Days Prior to 3 may take 60-90 seconds to fetch all of those data."),
                                     br(),
                                     h6("NOTE:  Each change will initiate a new pull of all records.  If useing, decide how many days of prior you want, then select that value and wait.")),
                            hr(),
                            h4("Get Most Recent Data"),
                            actionButton("updData", "Refresh"),
                            hr(),
                            h4("Map Options"),
                            radioButtons(inputId="mapDef",
                                         label="Map Flood Frac.",
                                         choices=c("Flood Fraction", "Stage")),
                            HTML('<button data-toggle="collapse" data-target="#demo3" class="button" style="horizontal-align:middle"><span><b>About</b></span></button>'),
                            tags$div(id = 'demo3',  class="collapse",
                                     h6("Flood Fraction will color code site points on the map according to how close to flood stage they are (i.e., the first numeric column in the table below)."),
                                     br(),
                                     h6("Stage will color code the site points based on most recent stage readings at each site.")
                                     )
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

