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
                        ),
                 column(3,
                        numericInput("daysBack", "Days of data to fetch prior to today:", 1, min=0,
                                     max=30, step=1)
                        )
             ),
             DT::dataTableOutput('x1'))
)


