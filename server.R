library(shiny)
library(shinythemes)
library(party)
#library(glmulti)
library(rJava)
library(igraph)
library(scales)
library(circlize)
library(dendextend)
#library(rattle)
library(lme4)
library(lmerTest)
#library(plotly)
#library(fastcluster)
library(Cubist)
library(C50)
library(minerva)
library(flashClust)
   ## library(cluster)
library(caret)
library(ipred)
#library(ada)
#library(caretEnsemble)
library(arm)
library(glmnet)
library(e1071)
library(kernlab)
library(pls)
library(pROC)
library(mlbench)
library(taRifx)
#library(corrplot)
library(gplots)
library(psych)
library(gbm)
library(rpart)
library(randomForest)
library(earth)
library(kohonen)
#library(ranger)
library(nnet)
library(kerndwd)
library(caroline)
library(ggplot2)
library(GGally)
   ## library(plyr)
library(grid)
library(gridExtra)
require(factoextra)
require(ggbiplot)
require(bnlearn)
require(Rgraphviz)
library(rpart.plot)
library(NeuralNetTools)
#library(WGCNA)



shinyServer(function(input, output, session) {
    
#     filedata <- reactive({
#         infile <- input$file1
#         if (is.null(infile)){
#             return(NULL)      
#         }
#         read.csv(infile$datapath, header=input$header, sep=input$sep, quote=input$quote, na.strings=c("NA", "?", ""))
#     })
    
    
    
#     csv <- read.csv("combinedMODdata2.csv", na.strings = c("", "NA"))
#     csv$Age <- as.numeric(as.character(csv$Age))
#     csv$PatientID <- as.factor(csv$PatientID)
#     csv$SOFA <- as.numeric(csv$SOFA)
#     csv$Platelets <- as.numeric(csv$Platelets)
#     csv$GCS <- as.numeric(csv$GCS)
#     csv$Glucose <- as.numeric(csv$Glucose)
#     csv$RespiratoryRate <- as.numeric(csv$RespiratoryRate)
#  
#     names(csv)
#     csv <- csv[,-1]
#     names(csv)

    load("combined_data.RData")
    #write.csv(names(csv), "MOD predictor groups.csv")
    predcsv <- read.csv("MOD predictor groups.csv")
    
    #csv <- read.csv("JANINE4.csv")
    #ints <- sapply(csv, is.integer)
    #csv[,ints] <- sapply(csv[,ints], as.numeric)
    #load("NBA full data_RevC.RData")
    #csv <- csv[csv$Record %% 2 == 0,]  # remove flipped pairs
    #load("NBA full data.RData")
    #predcsv <- read.csv("NBA predictor groups.csv")
    
     #tree2 <- train(death~., data=csv, method="nnet")
#     prp(tree2$finalModel)

    
    filedata <- function() csv
    
   # unique(paste(predcsv$selfgroup, predcsv$group,  sep="_"))
   # predcsv$names <- as.factor(paste(predcsv$selfgroup, predcsv$group,  sep="_"))
    predcsv$names <- as.factor(predcsv$group)
    uniquegroupnames <- levels(predcsv$names)
    
    
    # group variables into eigengenes, phenotypes, and demographics
    #levelnames <- levels(predcsv$names)
    predgroupnames <- list()
    for(i in 1:length(uniquegroupnames)){
        predgroupnames[[i]] <- names(filedata())[which(predcsv$names == uniquegroupnames[i])]
    }
    names(predgroupnames) <- uniquegroupnames
    #print(uniquegroupnames)
    #print(uniquegroupnames[c(15,19, 8, 11)])
    
    source("initializations.R", local=TRUE)
    
    #source("old_server_stuff.R", local=TRUE)
    #load("plotnnet.RData")
    source("variable controls.R", local=TRUE)
    #source("decisiontree.R", local=TRUE)
    source("pca.R", local=TRUE)
    source("clustering.R", local=TRUE)
    
    source("bayesian network.R", local=TRUE)
    source("plotter.R", local=TRUE)
    source("machine learning.R", local=TRUE)
    #source("glmulti.R", local=TRUE)
    
#     predgroupnames <- list(a=names(filedata())[19:32], # phenovars
#                            b=names(filedata())[4:17], # demovars
#                            c=names(filedata())[33:51], # eigvars
#                            d=names(filedata())[151:188], # cytokines
#                            e=names(filedata())[52:91], # t1vars
#                            f=names(filedata())[92:127], # t3 vars
#                            g=names(filedata())[128:150] # bcells
#     )
    
    #demovars <- names(filedata())[4:17]
    #phenovars <- names(filedata())[19:32]
    #eigvars <- names(filedata())[33:51]
    #t1vars <- names(filedata())[52:91]
    #t3vars <- names(filedata())[92:127]
    #bcells <- names(filedata())[128:150]
    #cytokine <- names(filedata())[151:188]

    # data processing
#     for(i in 1:length(cytokine)){
#         csv[,cytokine[i]] <- as.numeric(csv[,cytokine[i]])
#     }
#     for(i in 1:length(bcells)){
#         csv[,bcells[i]] <- as.numeric(csv[,bcells[i]])
#     }
#     csv <- csv[-c(20,64,93,97,102),]
#     csv[c(60,63),cytokine] <- NA
#     
#     save(csv,file="combined_data.RData")

 

    
    output$fullsummary <- DT::renderDataTable({
        infile <- filedata()
        stats <- stat.desc(infile)
        index <- sapply(stats, is.numeric)
        stats[,index] <- signif(stats[,index], digits=2)
        DT::datatable(stats, options = list(pageLength = 20))
    })
    
    output$fulltable <- DT::renderDataTable({
        DT::datatable(filedata(), filter="top", options = list(pageLength = 20))
    })
    
    output$predtable <- DT::renderDataTable({
        DT::datatable(predcsv, filter="top", options = list(pageLength = 150))
    })
    
    
    # Save section
    output$downloadData <- downloadHandler(
        # This function returns a string which tells the client
        # browser what name to use when saving the file.
        filename = function() {
            paste(input$algorithms, input$ntune, input$dependent, ".RData", sep = "_")
        },
        # This function should write data to a file given to it by
        # the argument 'file'.
        content = function(file) {
            save(model, vip, rmatrix.plot, vip.plot, preds.plot, preds.plot.1, biplots, f1,f2,f3, file=file)
        }
    )
    
})


