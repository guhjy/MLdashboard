# panels 

whole_panel <- tabPanel("Machine Learning",  
                        sidebarLayout(
                            sidebarPanel(
                                h3("Machine Learning"),
                                actionButton("pcaButton", "Run", icon("play"), 
                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width="80px"),
                                hr(),
                                uiOutput("dependent"),
                                uiOutput("algorithms"),
                                uiOutput("bnalgo"),
                                uiOutput("bnlayout"),
                                uiOutput("subschk"),
                                uiOutput("subs"),
                                uiOutput("subsvar"),
                                uiOutput("usepcs"),
                                uiOutput("prep"),
                                #uiOutput("pcagroup"),
                                
#                                 uiOutput("popsize"),
#                                 uiOutput("mutrate"),
#                                 uiOutput("sexrate"),
#                                 uiOutput("imm"),
                                
                                
                                uiOutput("allvars"),
                                uiOutput("predgroups"),
                                
#                                 uiOutput("eigvars"),
#                                 uiOutput("phenovars"),
#                                 uiOutput("demovars"),
#                                 uiOutput("t1vars"),
#                                 uiOutput("cytokine"),
#                                 uiOutput("t3vars"),
#                                 uiOutput("bcells"),
                                uiOutput("independents"),
                                uiOutput("removed"),
                                uiOutput("which2pcs"),
                                uiOutput("whichpcs"),
                                #uiOutput("labcol"),
                                uiOutput("aggbyvars"),
                                uiOutput("unsigned"),
                                uiOutput("cortype"),
                                
                                uiOutput("netlayout"),
                                uiOutput("distfun"),
                                uiOutput("p"),
                                uiOutput("hfun"),
                                uiOutput("corthresh"),
                                uiOutput("dendheight"),
                                uiOutput("labelheight"),
                                
                                
                                #uiOutput('ui.action'), # instead of conditionalPanel
                                uiOutput("resamplechk"),
                                uiOutput("resampleid"),
                                uiOutput("subschktest"),
                                uiOutput("subsvartest"),
                                uiOutput("pcttrain"),
                                uiOutput("ntune"),
                                uiOutput("nfold")

                             

                            ),
                            mainPanel(tabsetPanel(id="panelID",
                                                  tabPanel("PCA Biplot",value = "pcabiplot", plotOutput("pca", width="auto")),
                                                  tabPanel("PCA Stats",value = "pcastats", plotOutput("pca2", width = "auto")),
                                                  tabPanel("BiCluster", value = "bicluster", plotOutput("cluster", width = "auto")),
                                                  tabPanel("VarCluster", value = "predcluster", plotOutput("corrs",  width = "auto")),
                                                  #tabPanel("PredCircle", value = "predcircle", plotOutput("circle",  width = "auto")),
                                                  tabPanel("Network", value = "network", plotOutput("plotnetwork", width = "auto")),
                                                  #tabPanel("IDCluster", value = "idcluster", plotOutput("idcorrs", height = 700, width = "auto")),
                                                  tabPanel("Model Fit", value = "modelfit", verbatimTextOutput('contents')),
                                                  tabPanel("Model Predict", value = "modelpredict", plotOutput("prediction", width = "auto")),
                                                  tabPanel("Model VIP", value = "modelvip", plotOutput("vip", width = "auto")),
                                                  tabPanel("Model Plot", value = "modelplot", plotOutput("plotmodel", width="auto")),
                                                  tabPanel("Model Summary", value = "modelsummary", verbatimTextOutput("summary")),
                                                  tabPanel("Model Details", value = "modeldetails", verbatimTextOutput("summary2")),
                                                  tabPanel("Bayesian Network", value = "bayesiannetwork", plotOutput("bn", width="auto"))
                                                  #tabPanel("Decision Tree", value = "cart", plotOutput("decisiontree", height = 700, width="auto")),
                                                  #tabPanel("GLMulti Summary", value = "glmultitab2", verbatimTextOutput("contentsglmulti")),
                                                  #tabPanel("GLMultiVIP", value = "glmultitab1", plotOutput("glmultibarplot", height = 1000, width="auto"))
                                                  
                            ))
                        )
)


plots_panel <- tabPanel("GLMM",  
                        sidebarLayout(
                            sidebarPanel(
                                h3("MultiLevel Model"),
                                #h6("Find interactions: Y = A*B + C"),
                                hr(),
                                uiOutput("dependent2"),
                                #uiOutput("independent2"), 
                                uiOutput("grouping2"),
                                uiOutput("controlling"),
                                uiOutput("randomeffects"),
                                uiOutput("randomslope"),
                                uiOutput("subschkGLMM"),
                                uiOutput("subsGLMM"),
                                uiOutput("subsvarGLMM"),
                                #uiOutput("rptmeas"),
                                #uiOutput("traces"),
                                hr(),
                                uiOutput("plottype"),
                                uiOutput("pointchk")
                            ),
                            mainPanel(tabsetPanel(id="panelID2",
                                                  tabPanel("Plots", value = "plotterplot", plotOutput("plotter", width="auto")),
                                                  tabPanel("P-values", value = "pvalues", verbatimTextOutput("pairsummary"))
                            ))
                        )
)

# save_panel <- tabPanel("Save",
#                        sidebarLayout(
#                            sidebarPanel(
#                                h5(tags$b("Export Data (RData)")),
#                                h6("Results learned by a single model can be exported to an RData file"),
#                                downloadButton('downloadData', 'Save to file')
#                            ),
#                            mainPanel(tabsetPanel(
#                                print("This panel is under construction.")
#                            ))
#                        )
#                        
#                        
# )

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

pred_panel <- tabPanel("Predictor Groupings",                     
                         mainPanel(  DT::dataTableOutput("predtable") )
)