# import_panel loads a csv file
library(pastecs)

import_panel <- tabPanel("Raw Data",
                         #titlePanel("Import Files"),
#                          sidebarLayout(
#                              sidebarPanel(
#                                  fileInput('file1', 'Choose CSV File',
#                                            accept=c('text/csv', 
#                                                     'text/comma-separated-values,text/plain', 
#                                                     '.csv')),
#                                  tags$hr(),
#                                  checkboxInput('header', 'Header', TRUE),
#                                  radioButtons('sep', 'Separator',
#                                               c(Comma=',',
#                                                 Semicolon=';',
#                                                 Tab='\t'),
#                                               ','),
#                                  radioButtons('quote', 'Quote',
#                                               c(None='',
#                                                 'Double Quote'='"',
#                                                 'Single Quote'="'"),
#                                               '"')
#                              ),
                             mainPanel( tabsetPanel(tabPanel("Raw Data", DT::dataTableOutput("fulltable")), 
                                                    tabPanel("Summary", DT::dataTableOutput("fullsummary"))  ))
                         #)
)

# save_panel <- tabPanel("Save",
#                        print("This panel is under construction.")
# )

