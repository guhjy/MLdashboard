# save_panel saves data for download

save_panel <- tabPanel("Save",
                       sidebarLayout(
                           sidebarPanel(
                               h5(tags$b("Export Data (RData)")),
                               h6("Results learned by a single model can be exported to an RData file"),
                               downloadButton('downloadData', 'Save to file')
                           ),
                           mainPanel(tabsetPanel(
                               print("This panel is under construction.")
                           ))
                       )
                       
                       
)

pca_panel <- tabPanel("PCA",  
                      sidebarLayout(
                          sidebarPanel(
                              uiOutput("pcagroup"),
                              uiOutput("subs"),
                              uiOutput("subsvar"),
                              
                              # predictor subgroups
                              uiOutput("allvars"),
                              uiOutput("eigvars"),
                              uiOutput("phenovars"),
                              uiOutput("demovars"),
                              uiOutput("independents"),
                              uiOutput("removed")
                          ),
                      
                          mainPanel(tabsetPanel(
                              tabPanel("Biplot",plotOutput("pca", height = 650, width="auto")),
                              tabPanel("Summary",plotOutput("pca2", height = 650, width = "auto")),
                              tabPanel("Cor",plotOutput("corrs", height = 650, width = "auto"))
                          ))
                      )
)

clustering_panel <- tabPanel("Clustering",
                             print("This panel is under construction.")
)


network_panel <- tabPanel("Network",
                             print("This panel is under construction.")
)

predictive_panel <- tabPanel("Predictive Models",
                             print("This panel is under construction.")
)

bn_panel <- tabPanel("Bayesian Network",
                     print("This panel is under construction.")
)