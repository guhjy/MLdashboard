
whole_panel <- tabPanel("Machine Learning",  
                      sidebarLayout(
                          sidebarPanel(
                              #uiOutput("analysis"),
                              uiOutput("dependent"),
                              uiOutput("bnalgo"),
                              uiOutput("bnlayout"),
                              #hr(),
                              uiOutput("subschk"),
                              uiOutput("subs"),
                              uiOutput("subsvar"),
                              uiOutput("prep"),
                              uiOutput("pcagroup"),
                              
                              # predictor subgroups
                              #hr(),
                              
                              #h5(strong("Select predictors: ")),
                              uiOutput("allvars"),
                              uiOutput("eigvars"),
                              uiOutput("phenovars"),
                              uiOutput("demovars"),
                              uiOutput("t1vars"),
                              uiOutput("t3vars"),
                              uiOutput("bcells"),
                              uiOutput("cytokine"),
                              uiOutput("independents"),
                              uiOutput("removed"),
                              uiOutput("labcol"),
                              uiOutput("cortype"),
                              uiOutput("distfun"),
                              uiOutput("hfun"),
                              #hr(),
                             
                              uiOutput("algorithms"),
                              #uiOutput('ui.action'), # instead of conditionalPanel
                              #br(),
                              uiOutput("resamplechk"),
                              uiOutput("resampleid"),
                              uiOutput("subschktest"),
                              uiOutput("subsvartest"),
                              uiOutput("pcttrain"),
                              uiOutput("ntune")
                          ),
                          
                          mainPanel(tabsetPanel(id="panelID",
                              tabPanel("PCA Biplot",value = "pcabiplot", plotOutput("pca", height = 680, width="auto")),
                              tabPanel("PCA Stats",value = "pcastats", plotOutput("pca2", height = 1700, width = "auto")),
                              tabPanel("BiCluster", value = "bicluster", plotOutput("cluster", height = 2000, width = "110%")),
                              tabPanel("CoCluster", value = "cocluster", plotOutput("corrs", height = 2400, width = "180%")),
                              tabPanel("IDCluster", value = "idcluster", plotOutput("idcorrs", height = 1600, width = "150%")),
                              tabPanel("Model Fit", value = "modelfit", verbatimTextOutput('contents')),
                              tabPanel("Model Predict", value = "modelpredict", plotOutput("prediction", height = 680, width = "auto")),
                              tabPanel("Model VIP", value = "modelvip", plotOutput("vip", height = 2200, width = "auto")),
                              tabPanel("Model Summary", value = "modelsummary", verbatimTextOutput("summary")),
                              tabPanel("Model Details", value = "modeldetails", verbatimTextOutput("summary2")),
                              tabPanel("Bayesian Network", value = "bayesiannetwork", plotOutput("bn", height = 1200, width="200%"))
                          ))
                      )
)



plots_panel <- tabPanel("Viewer",  
                        sidebarLayout(
                            sidebarPanel(
                                #uiOutput("analysis"),
                                uiOutput("dependent2"),
                                uiOutput("independent2"), 
                                uiOutput("grouping2"),
                                uiOutput("controlling"),
                                uiOutput("rptmeas"),
                                uiOutput("traces"),
                                hr(),
                                uiOutput("plottype")
                                #uiOutput("bnalgo"),
                                #uiOutput("bnlayout"),
                                #hr(),
                                #uiOutput("subschk"),
                                #uiOutput("subs"),
                                #uiOutput("subsvar"),
                                #uiOutput("prep"),
                                #uiOutput("pcagroup"),
                                
                                # predictor subgroups
                                #hr(),
                                
                                #h5(strong("Select predictors: ")),
                                #uiOutput("allvars"),
                                #uiOutput("eigvars"),
                                #uiOutput("phenovars"),
                                #uiOutput("demovars"),
                                #uiOutput("t1vars"),
                                #uiOutput("t3vars"),
                                #uiOutput("bcells"),
                                #uiOutput("cytokine"),
                                #uiOutput("independents"),
                                #uiOutput("removed"),
                                #uiOutput("labcol"),
                                #uiOutput("cortype"),
                                #uiOutput("distfun"),
                                #uiOutput("hfun"),
                                #hr(),
                                
                                #uiOutput("algorithms"),
                                #uiOutput('ui.action'), # instead of conditionalPanel
                                #br(),
                                #uiOutput("resamplechk"),
                                #uiOutput("resampleid"),
                                #uiOutput("subschktest"),
                                #uiOutput("subsvartest"),
                                #uiOutput("pcttrain"),
                                #uiOutput("ntune")
                            ),
                            
                            mainPanel(tabsetPanel(id="panelID2",
                                                  tabPanel("Plotter", value = "plotterplot", plotOutput("plotter", height = 600, width="auto")),
                                                  tabPanel("P-value", value = "pvalues", verbatimTextOutput("pairsummary"))
#                                                   tabPanel("PCA Biplot",value = "pcabiplot", plotOutput("pca", height = 1400, width="auto")),
#                                                   tabPanel("PCA Stats",value = "pcastats", plotOutput("pca2", height = 1700, width = "auto")),
#                                                   tabPanel("BiCluster", value = "bicluster", plotOutput("cluster", height = 2000, width = "110%")),
#                                                   tabPanel("CoCluster", value = "cocluster", plotOutput("corrs", height = 2400, width = "180%")),
#                                                   tabPanel("IDCluster", value = "idcluster", plotOutput("idcorrs", height = 1600, width = "150%")),
#                                                   tabPanel("Model Fit", value = "modelfit", verbatimTextOutput('contents')),
#                                                   tabPanel("Model Predict", value = "modelpredict", plotOutput("prediction", height = 680, width = "auto")),
#                                                   tabPanel("Model VIP", value = "modelvip", plotOutput("vip", height = 2200, width = "auto")),
#                                                   tabPanel("Model Summary", value = "modelsummary", verbatimTextOutput("summary")),
#                                                   tabPanel("Model Details", value = "modeldetails", verbatimTextOutput("summary2")),
#                                                   tabPanel("Bayesian Network", value = "bayesiannetwork", plotOutput("bn", height = 1200, width="200%"))
                            ))
                        )
)






# pca_panel <- tabPanel("PCA",  
#                       sidebarLayout(
#                           sidebarPanel(
#                               uiOutput("pcagroup"),
#                               uiOutput("subs"),
#                               uiOutput("subsvar"),
#                               
#                               # predictor subgroups
#                               shiny::hr(),
#                               #uiOutput("dependent"),
#                               h5(strong("Select predictors: ")),
#                               uiOutput("allvars"),
#                               uiOutput("eigvars"),
#                               uiOutput("phenovars"),
#                               uiOutput("demovars"),
#                               uiOutput("independents"),
#                               uiOutput("removed")
#                               #hr(),
#                               #uiOutput("algorithms"),
#                               #uiOutput("prep"),
#                               #uiOutput('ui.action') # instead of conditionalPanel
#                               #hr(),
#                               #uiOutput("pcttrain"),
#                               #uiOutput("ntune")
#                           ),
#                       
#                           mainPanel(tabsetPanel(
#                               #tabPanel("R",plotOutput("corrs", height = 650, width = "auto")),
#                               tabPanel("Biplot",plotOutput("pca", height = 650, width="auto")),
#                               tabPanel("Contributions",plotOutput("pca2", height = 650, width = "auto"))
#                               #tabPanel("Fit",verbatimTextOutput('contents')),
#                               #tabPanel("Predict",plotOutput("prediction", height = 650, width = "auto")),
#                               #tabPanel("VIP",plotOutput("vip", height = 650, width = "auto")),
#                               #tabPanel("Summary",verbatimTextOutput("summary")),
#                               #tabPanel("Model",verbatimTextOutput("summary2"))
#                           ))
#                       )
# )
# 
# clustering_panel <- tabPanel("Clustering",
#                               sidebarLayout(
#                                   sidebarPanel(
#                                       uiOutput("subs2"),
#                                       uiOutput("subsvar2"),
#                                       shiny::hr(),
#                                       h5(strong("Choose which variables to include: ")),
#                                       # predictor subgroups
#                                       uiOutput("allvars2"),
#                                       uiOutput("eigvars2"),
#                                       uiOutput("phenovars2"),
#                                       uiOutput("demovars2"),
#                                       uiOutput("independents2"),
#                                       uiOutput("removed2")
#                                   ),
# #                                  
#                                   mainPanel(tabsetPanel(
#                                       #tabPanel("Biplot",plotOutput("pca", height = 650, width="auto")),
# #                                      #tabPanel("Summary",plotOutput("pca2", height = 650, width = "auto")),
#                                       tabPanel("Correlation",plotOutput("corrs", height = 650, width = "auto"))
#                                   ))
#                               )
#  )
# 
# 
# network_panel <- tabPanel("Optimization Control",
#                           sidebarLayout(
#                               sidebarPanel(
#                                   #                                   uiOutput("pcagroup"),
#                                   #                                   uiOutput("subs"),
#                                   #                                   uiOutput("subsvar"),
#                                   #                                   
#                                   #                                   # predictor subgroups
#                                   #                                   hr(),
#                                   #                                   uiOutput("dependent2"),
#                                   #                                   h5(strong("Select predictors: ")),
#                                   #                                   uiOutput("allvars"),
#                                   #                                   uiOutput("eigvars"),
#                                   #                                   uiOutput("phenovars"),
#                                   #                                   uiOutput("demovars"),
#                                   #                                   uiOutput("independents"),
#                                   #                                   uiOutput("removed"),
#                                   #                                   hr(),
#                                   #                                   uiOutput("algorithms"),
#                                   #                                   uiOutput("prep"),
#                                   #                                   uiOutput('ui.action') ,# instead of conditionalPanel
#                                   #                                   hr(),
#                                   uiOutput("pcttrain"),
#                                   uiOutput("ntune")
#                               ),
#                               mainPanel(tabsetPanel(
#                                   
#                                   #     tabPanel("Biplot",plotOutput("pca", height = 650, width="auto")),
#                                   #     tabPanel("PCA",plotOutput("pca2", height = 650, width = "auto")),
#                                   #     tabPanel("R",plotOutput("corrs", height = 650, width = "auto")),
#                                   #     tabPanel("Fit",verbatimTextOutput('contents')),
#                                   #     tabPanel("Predict",plotOutput("prediction", height = 650, width = "auto")),
#                                   #     tabPanel("VIP",plotOutput("vip", height = 650, width = "auto")),
#                                   #     tabPanel("Summary",verbatimTextOutput("summary")),
#                                   #     tabPanel("Model",verbatimTextOutput("summary2"))
#                               ))
#                           )
# )
# 
# predictive_panel <- tabPanel("Predictive Models",
#                              print("This panel is under construction.")
# )
# 
# bn_panel <- tabPanel("Bayesian Network",  
#                      sidebarLayout(
#                          sidebarPanel(
#                              uiOutput("analysis"),
#                              uiOutput("bnalgo"),
#                              uiOutput("dependent"),
#                              #hr(),
#                              uiOutput("subs"),
#                              uiOutput("subsvar"),
#                              uiOutput("prep"),
#                              uiOutput("pcagroup"),
#                              
#                              # predictor subgroups
#                              #hr(),
#                              
#                              #h5(strong("Select predictors: ")),
#                              uiOutput("allvars"),
#                              uiOutput("eigvars"),
#                              uiOutput("phenovars"),
#                              uiOutput("demovars"),
#                              uiOutput("independents"),
#                              uiOutput("removed"),
#                              #hr(),
#                              
#                              #uiOutput("algorithms"),
#                              uiOutput('ui.action') # instead of conditionalPanel
#                              #br(),
#                              #uiOutput("pcttrain"),
#                              #uiOutput("ntune")
#                          ),
#                          
#                          mainPanel(tabsetPanel(
#                              tabPanel("Network",plotOutput("bn", height = 620, width="auto"))
#                              #tabPanel("PCA Stats",plotOutput("pca2", height = 620, width = "auto")),
#                              #tabPanel("Clustering",plotOutput("corrs", height = 650, width = "auto")),
#                              #tabPanel("Model Fit",verbatimTextOutput('contents')),
#                              #tabPanel("Model Predict",plotOutput("prediction", height = 600, width = "auto")),
#                              #tabPanel("Model VIP",plotOutput("vip", height = 620, width = "auto")),
#                              #tabPanel("Model Summary",verbatimTextOutput("summary")),
#                              #tabPanel("Model Details",verbatimTextOutput("summary2"))
#                          ))
#                      )
# )