library(shiny)
library(dataRetrieval)
library(dplyr)
library(DT)
library(formattable)


navbarPage(
    title = 'Interaction with Table Cells',
    id = 'x0',
    tabPanel('Table',
             fluidRow(
                 column(1),
                 column(2,
                        actionButton("go", "Get Data")
                        )
             ),
             DT::dataTableOutput('x1'))
)

