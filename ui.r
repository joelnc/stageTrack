library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)

navbarPage(
    title = 'Stage Tracker',
    id = 'x0',
    tabPanel('Tabular',
             fluidRow(
                 column(1),
                 column(2,
                        actionButton("go", "Get Data")
                        )
             ),
             DT::dataTableOutput('x1'))
)

