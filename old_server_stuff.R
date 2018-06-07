# stuff from the first version
library(shiny)
library(shinythemes)
library(plotly)
library(fastcluster)
library(minerva)
library(flashClust)
library(cluster)
library(caret)
library(ipred)
library(ada)
library(caretEnsemble)
library(arm)
library(glmnet)
#library(obliqueRF)
library(e1071)
library(kernlab)
library(pls)
library(pROC)
library(mlbench)
library(taRifx)
library(corrplot)
library(gplots)
library(psych)
library(gbm)
library(rpart)
library(randomForest)
library(earth)
library(kohonen)
library(ranger)
library(nnet)
library(kerndwd)
library(caroline)
library(ggplot2)
library(GGally)
library(plyr)

library(grid)
library(gridExtra)

require(factoextra)
require(ggbiplot)

require(bnlearn)
require(Rgraphviz)




anals <- c("...", "PCA", "Clustering", "Modeling", "Bayesian Network")

#output$analysis <- renderUI({
#    df <- filedata()
#    if (is.null(df)) return(NULL)
#    selectInput("analysis","Select type of analysis:", anals, selected=NULL, selectize=T)
#})

# control which sliders appear for which tabs
analysis <- reactive({
    if(is.null(input$panelID)) return("...")
    if(input$panelID %in% c("pcabiplot", "pcastats")) { return("PCA")}
    if(input$panelID %in% c("bicluster", "cocluster", "idcluster")) { return("Clustering")}
    if(input$panelID %in% c("modelfit", "modelpredict", "modelvip", "modelsummary", "modeldetails")) { return("Modeling")}
    if(input$panelID %in% c("bayesiannetwork")) { return("Bayesian Network")}
})

# subset data
output$subschk  <- renderUI({
    df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("subschk", "Take subgroup", value=input$subschk, width='100%')
})
output$subs  <- renderUI({
    df <- filedata()
    #if (is.null(df)) return(NULL)
    if(input$subschk==F) return()
    items=names(df)
    #names(items)=items
    selectInput("subs", "Choose subset variable", items, selected=input$subs, selectize=T)
})
output$subsvar  <- renderUI({
    df <- filedata()
    if(input$subschk==F) return()
    #if (is.null(df) | is.null(input$subs)) return(NULL)
    #if (is.null(df) | input$subs=="") return(NULL)
    #if (input$subs==F) return()
    selectInput("subsvar","Choose levels", levels(df[,input$subs]), multiple=T, selected=input$subsvar, selectize=T)
})

# choose predictors
output$allvars <- output$allvars2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    checkboxInput("allvars", "Use all variables as predictors", value=F, width='100%')
   
})
output$eigvars <- output$eigvars2 <- renderUI({
    df <- filedata()
    if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
    if(input$allvars==T ) return(NULL)
    checkboxInput("eigvars", "include all eigengenes", value=T, width='100%')
})
output$t1vars <- renderUI({
    df <- filedata()
    if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
    if(input$allvars==T ) return(NULL)
    checkboxInput("t1vars", "include all T1", value=F, width='100%')
})
output$t3vars <- renderUI({
    df <- filedata()
    if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
    if(input$allvars==T ) return(NULL)
    checkboxInput("t3vars", "include all T3", value=F, width='100%')
})
output$bcells <- renderUI({
    df <- filedata()
    if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
    if(input$allvars==T ) return(NULL)
    checkboxInput("bcells", "include all B Cells", value=F, width='100%')
})
output$cytokine <- renderUI({
    df <- filedata()
    if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
    if(input$allvars==T ) return(NULL)
    checkboxInput("cytokine", "include all cytokines", value=F, width='100%')
})


output$phenovars <- output$phenovars2 <- renderUI({
    df <- filedata()
    if (is.null(df) | is.null(input$allvars)) return(NULL)
    if(input$allvars==T) return(NULL)
    checkboxInput("phenovars", "include all clinical phenotypes", value=F, width='100%')
})
output$demovars <- renderUI({
    df <- filedata()
    if (is.null(df) | is.null(input$allvars)) return(NULL)
    if(input$allvars==T) return(NULL)
    checkboxInput("demovars", "include all demographics", value=F, width='100%')
})
output$independents  <- renderUI({
    df <- filedata()
    if (is.null(df) | is.null(input$allvars)) return(NULL)
    if (input$allvars==T) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("independents","Add predictors:",items,multiple=TRUE, selected=NULL, selectize=T)
})
output$removed <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("removed","Remove predictors:",items,multiple=TRUE, selected=c("PatientID","Sensitized","olderthan60","Transfusion","Race","SuccessfulBridgeToTransplant","HeartFailure","Plasmapheresis","Sex"), selectize=T)
})

# PCA
output$pcagroup <- renderUI({
    df <- filedata()
    #if (is.null(df)) return(NULL)
    if (analysis() %in% c("PCA")){
        items=names(df)
        names(items)=items
        selectInput("pcagroup","Select grouping for PCA:", items, selected="Survivor", selectize=T)
    } else return(NULL)
    
})
getpca <- reactive({
    df <- filedata()
    # isolate({
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$pcagroup))]
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$pcagroup, input$removed))]
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents)
        if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        if(input$demovars == T) {predicto <- c(predicto, demovars)}
        if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        if(input$bcells == T) {predicto <- c(predicto, bcells)}
        if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) {
            index <- which(predicto %in% input$removed)
            print(paste("hi ",index),sep="")
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
        if(input$pcagroup %in% predicto) predicto <- predicto[-which(predicto==input$pcagroup)]
    }
    #print(which(predicto %in% input$removed))
    print(predicto)
    
    if(input$subschk==F) {
        dat <- droplevels( na.omit( df[,c(input$dependent, predicto, input$pcagroup)] ) )
        
    } else {
        dat <- droplevels( na.omit( subset(
            df[,c(input$dependent, predicto, input$pcagroup)],  df[,input$subs] %in% input$subsvar 
        )))
    }
    #print(dat)
    #dat <- na.omit(df[,c(input$dependent, predicto)])
    
    nums <- sapply(dat, is.numeric)
    #print(nums)
    out <- list(pca=prcomp(dat[,nums], center = TRUE, scale. = TRUE), dat=dat[,nums], wfactor=dat)
    return(out)
    # })
})
output$pca <- renderPlot({
    #if (is.null(input$action)) return()
    #if(analysis()=="...") return("Choose an analysis.")
    pca <- getpca()
    #print(pca)
    #library(devtools)
    #install_github("ggbiplot", "vqv")
    
    g <- ggbiplot(pca$pca, obs.scale = 1, var.scale = 1, groups = as.factor(pca$wfactor[,input$pcagroup]), ellipse = TRUE, circle = F, alpha=.6, var.axes=T)
    g <- g + scale_color_discrete(name = '') + theme_bw()
    g <- g +  ggtitle(input$pcagroup) 
    biplots <<- g
    
    #f1 <- fviz_screeplot(pca, ncp=ncol(dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
    #f2 <- fviz_contrib(pca, choice="var", axes = 1 ,  sort.val="none") + coord_flip() 
    #f3 <- fviz_contrib(pca, choice="var", axes = 2 ,  sort.val="none") + coord_flip() 
    
    grid.arrange(g)
    #multiplot(g, f1,f2,f3, cols=1)
    
})
output$pca2 <- renderPlot({
    #if (is.null(input$action)) return()
    #if(analysis()=="...") return()
    pca <- getpca()
    f1 <<- fviz_screeplot(pca$pca, ncp=ncol(pca$dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
    f2 <<- fviz_contrib(pca$pca, choice="var", axes = 1 ,  sort.val="none") + coord_flip() 
    f3 <<- fviz_contrib(pca$pca, choice="var", axes = 2 ,  sort.val="none") + coord_flip() 
    
    grid.arrange(f1,f2,f3, ncol = 2, layout_matrix = rbind(c(1,1),c(2,3)))
    #multiplot(g, f1,f2,f3, cols=1)
    
})

# Clustering section
output$labcol <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Clustering")){
        items=names(df)
        names(items)=items
        selectInput("labcol","Label samples by:", items,multiple=TRUE, selected=input$labcol, selectize=T)
    } else return(NULL)
})
output$cortype <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Clustering")){
        items=c("Spearman", "Pearson", "Kendall", "MIC (Maximal Information Coefficient)", "MAS (Maximum Asymmetry Score)", "MEV (Maximum Edge Value)", "MCN (Minimum Cell Number)", "MIC-R2")
        selectInput("cortype","Choose association metric:", items, multiple=F, selected=input$cortype, selectize=T)
    } else return(NULL)
})
output$distfun <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Clustering")){
        items=c("Unsigned","Signed","L2", "L1")
        selectInput("distfun","Choose distance metric:", items, multiple=F, selected=input$distfun, selectize=T)
    } else return(NULL)
})
output$hfun <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Clustering")){
        items=c("Ward", "Single", "Complete","Average","Mcquitty","Median","Centroid")
        selectInput("hfun","Choose agglomeration method:", items, multiple=F, selected=input$hfun, selectize=T)
    } else return(NULL)
})
output$cluster <- renderPlot({
    #if (is.null(input$action)) return()
    df <- filedata()
    
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$removed))]
        predicto <- c(predicto)
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents)
        if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        if(input$demovars == T) {predicto <- c(predicto, demovars)}
        if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        if(input$bcells == T) {predicto <- c(predicto, bcells)}
        if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) {
            index <- which(predicto %in% input$removed)
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
    }
    #print(which(predicto %in% input$removed))
    print(predicto)
    
    if(input$subschk==F) {
        dat <-  droplevels(df[,c(input$labcol, predicto)] )
        
    } else {
        dat <- droplevels(subset(
            df[,c(input$labcol, predicto)],  df[,input$subs] %in% input$subsvar
        ))
    }
    
    nums <- sapply(dat, is.numeric)
    print(nums)
    
    M <- dat[,nums]
    N <- data.matrix(M)
    print(dim(N))
    
    rname.tmp <- dat[,input$labcol]
    if(length(input$labcol)>=2) {
        rownames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
    } else {rownames(N) <- rname.tmp}
    
    #rownames(N) <- paste(dat[,input$labcol], collapse=",")
    
    my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
    if(input$hfun=="Single") hcluster <- function(x) fastcluster::hclust(x, method="single")
    if(input$hfun=="Ward") hcluster <- function(x) fastcluster::hclust(x, method="ward.D")
    if(input$hfun=="Complete") hcluster <- function(x) fastcluster::hclust(x, method="complete")
    if(input$hfun=="Average") hcluster <- function(x) fastcluster::hclust(x, method="average")
    if(input$hfun=="Mcquitty") hcluster <- function(x) fastcluster::hclust(x, method="mcquitty")
    if(input$hfun=="Median") hcluster <- function(x) fastcluster::hclust(x, method="median")
    if(input$hfun=="Centroid") hcluster <- function(x) fastcluster::hclust(x, method="centroid")
    #if(input$distfun=="Unsigned L1") distfunc1 <- function(x) as.dist(1-abs(x))
    #if(input$distfun=="Unsigned L2") distfunc1 <- function(x) as.dist(sqrt(1-x^2))
    #if(input$distfun=="Signed L1") distfunc1 <- function(x) as.dist(1-x)
    #if(input$distfun=="Signed L2") distfunc1 <- function(x) as.dist(sign(1-x)*sqrt(1-x^2))
    print(N)
    heatmap.2(t(scale(N)),
              #hclustfun = hcluster,
              #distfun = distfunc1,
              #distfun <- function(x) daisy(x,metric="euclidean"),
              #distfun = function(x) as.dist(1 - cor(x, use="pairwise.complete.obs")),
              
              #cellnote = mat_data,  # same data set for cell labels
              #main = "Clustered Correlation Matrix", # heat map title
              key=T, key.title = "color key", key.xlab = "z-score",
              #keysize=.8, 
              #symkey=T, 
              #symm=F, 
              #symbreaks=T, 
              #denscol="grey",
              #notecol="black",      # change font color of cell labels to black
              #density.info="none",  # turns off density plot inside color legend
              trace="none",         # turns off trace lines inside the heat map
              margins =c(3,8),     # widens margins around plot
              col=my_palette,# use on color palette defined earlier
              scale="none",
              labCol =  rownames(N)
              
              #breaks=col_breaks,    # enable color transition at specified limits
              #dendrogram="both",     # only draw a row dendrogram
              #Colv=T, Rowv=T
              )            # turn off column clustering
    
    rmatrix.plot <<- recordPlot()
    replayPlot(rmatrix.plot) # redraw
})
output$corrs <- renderPlot({
    #if (is.null(input$action)) return()
    df <- filedata()
    
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$removed))]
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents, input$pcagroup)
        if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        if(input$demovars == T) {predicto <- c(predicto, demovars)}
        if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        if(input$bcells == T) {predicto <- c(predicto, bcells)}
        if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) {
            index <- which(predicto %in% input$removed)
            print(paste("hi ",index),sep="")
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
    }
    #print(which(predicto %in% input$removed))
    print(predicto)
    
    if(input$subschk==F) {
        dat <-droplevels( df[,c(predicto)] )
        
    } else {
        dat <- droplevels(subset(
            df[,c(predicto)],  df[,input$subs] %in% input$subsvar
        ))
    }
    
    nums <- sapply(dat, is.numeric)
    
    if(input$cortype=="Pearson")  M <- cor(dat[,nums], use="pairwise.complete.obs")
    if(input$cortype=="Spearman")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="spearman")
    if(input$cortype=="Kendall")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="kendall")
    m2 <- mine(data.matrix(na.omit(dat[,nums])))
    if(input$cortype=="MIC (Maximal Information Coefficient)")  M <- data.matrix(m2$MIC)
    if(input$cortype=="MAS (Maximal Asymmetry Score)")  M <- data.matrix(m2$MAS)
    if(input$cortype=="MEV (Maximum Edge Value)")  M <- data.matrix(m2$MEV)
    if(input$cortype=="MCN (Minimum Cell Number)")  M <- data.matrix(m2$MCN)
    if(input$cortype=="MIC-R2")  M <- data.matrix(m2$MICR2)

    
    #         cor.mtest <- function(mat, ...) {
    #             mat <- as.matrix(mat)
    #             n <- ncol(mat)
    #             p.mat<- matrix(NA, n, n)
    #             diag(p.mat) <- 0
    #             for (i in 1:(n - 1)) {
    #                 for (j in (i + 1):n) {
    #                     tmp <- cor.test(mat[, i], mat[, j], ...)
    #                     p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    #                 }
    #             }
    #             colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    #             p.mat
    #         }
    #         # matrix of the p-value of the correlation
    #         p.mat <- cor.mtest(df[,nums])
    

    my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
    col_breaks = c(seq(-1,-3,length=100),  # for red
                   seq(-.3,0.3,length=100),              # for yellow
                   seq(0.3,1,length=100))
    #     corrplot(M, method="color", col=col(200),  
    #              order="hclust", addgrid.col="grey", outline=T, hclust.method="average",
    #              #addCoef.col = "black", addCoefasPercent = T, # Add coefficient of correlation
    #              tl.col="black", tl.cex=30/nrow(M), tl.srt=45, #Text label color and rotation
    #              # Combine with significance
    #              #p.mat = p.mat, sig.level = 0.05, insig = "blank", 
    #              # hide correlation coefficient on the principal diagonal
    #              diag=FALSE 
    #     )
    if(input$distfun=="Unsigned") distfunc1 <- function(x) as.dist(1-abs(x))
    #if(input$distfun=="Unsigned L2") distfunc1 <- function(x) as.dist(sqrt(1-x^2))
    if(input$distfun=="Signed") distfunc1 <- function(x) as.dist(1-x)
    #if(input$distfun=="Signed L2") distfunc1 <- function(x) as.dist(sign(1-x)*sqrt(1-x^2))
    if(input$distfun=="L2") distfunc1 <- function(x) dist(x)
    if(input$distfun=="L1") distfunc1 <- function(x) dist(x, method="manhattan")
    
    #hcluster <- function(x) hclust(x, method="average")
    if(input$hfun=="Ward") hcluster <- function(x) fastcluster::hclust(x, method="ward.D")
    if(input$hfun=="Single") hcluster <- function(x) fastcluster::hclust(x, method="single")
    if(input$hfun=="Complete") hcluster <- function(x) fastcluster::hclust(x, method="complete")
    if(input$hfun=="Average") hcluster <- function(x) fastcluster::hclust(x, method="average")
    if(input$hfun=="Mcquitty") hcluster <- function(x) fastcluster::hclust(x, method="mcquitty")
    if(input$hfun=="Median") hcluster <- function(x) fastcluster::hclust(x, method="median")
    if(input$hfun=="Centroid") hcluster <- function(x) fastcluster::hclust(x, method="centroid")
    heatmap.2(data.matrix(M),
              distfun = distfunc1,
              hclustfun = hcluster,
              #cellnote = mat_data,  # same data set for cell labels
              main = "Clustered Predictors", # heat map title
              key=T, key.title = "color key", key.xlab = "correlation",
              #keysize=.8, 
              symkey=T, symm=T, symbreaks=T, #denscol="grey",
              #notecol="black",      # change font color of cell labels to black
              #density.info="none",  # turns off density plot inside color legend
              trace="none",         # turns off trace lines inside the heat map
              margins =c(9,9),     # widens margins around plot
              col=my_palette,       # use on color palette defined earlier
              #breaks=col_breaks,    # enable color transition at specified limits
              dendrogram="both",     # only draw a row dendrogram
              Colv=T)            # turn off column clustering
    
    rmatrix.plot <<- recordPlot()
    replayPlot(rmatrix.plot) # redraw
})
output$idcorrs <- renderPlot({
    #if (is.null(input$action)) return()
    df <- filedata()
    
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$removed))]
        predicto <- c(predicto)
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents)
        if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        if(input$demovars == T) {predicto <- c(predicto, demovars)}
        if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        if(input$bcells == T) {predicto <- c(predicto, bcells)}
        if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) {
            index <- which(predicto %in% input$removed)
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
    }
    #print(which(predicto %in% input$removed))
    print(predicto)
    
    if(input$subschk==F) {
        dat <- droplevels( df[,c(input$labcol, predicto)] )
        
    } else {
        dat <-droplevels( subset(
            df[,c(input$labcol, predicto)],  df[,input$subs] %in% input$subsvar
        ))
    }
    
    nums <- sapply(dat, is.numeric)
    
    #cor(na.omit(t(csv[,nums])), use="pairwise.complete.obs")
    #cor(t(as.matrix(csv[-20,nums])), use="pairwise.complete.obs")
    
    
    M <- cor(t(dat[,nums]), use="pairwise.complete.obs")
    if(input$cortype=="Pearson")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs")
    if(input$cortype=="Spearman")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs", method="spearman")
    if(input$cortype=="Kendall")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs", method="kendall")
    
    if(input$cortype=="MIC (Maximal Information Coefficient)"){
        m1 <- mine(t(data.matrix(na.omit(dat[,nums]))))
        M <- m1$MIC
    }  
    if(input$cortype=="MAS (Maximal Asymmetry Score)")  M <- m1$MAS
    if(input$cortype=="MEV (Maximum Edge Value)")  M <- m1$MEV
    if(input$cortype=="MCN (Minimum Cell Number)")  M <- m1$MCN
    if(input$cortype=="MIC-R2")  M <- m1$MICR2
    #M <- dat[,nums]
    N <- data.matrix(M)
    
    rname.tmp <- dat[,input$labcol]
    if(length(input$labcol)>=2) {
        rownames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
        colnames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
    } else {
        rownames(N) <- rname.tmp
        colnames(N) <- rname.tmp
    }
    
    my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
    #col_breaks = c(seq(-1,-3,length=100),  # for red
    #               seq(-.3,0.3,length=100),              # for yellow
    #               seq(0.3,1,length=100))
    #     corrplot(M, method="color", col=col(200),  
    #              order="hclust", addgrid.col="grey", outline=T, hclust.method="average",
    #              #addCoef.col = "black", addCoefasPercent = T, # Add coefficient of correlation
    #              tl.col="black", tl.cex=30/nrow(M), tl.srt=45, #Text label color and rotation
    #              # Combine with significance
    #              #p.mat = p.mat, sig.level = 0.05, insig = "blank", 
    #              # hide correlation coefficient on the principal diagonal
    #              diag=FALSE 
    #     )
    if(input$distfun=="Unsigned") distfunc <- function(x) as.dist(1-abs(x))
    #if(input$distfun=="Unsigned L2") distfunc <- function(x) as.dist(sqrt(1-x^2))
    if(input$distfun=="Signed") distfunc <- function(x) as.dist(1-x)
    if(input$distfun=="L2") distfunc <- function(x) dist(x)
    if(input$distfun=="L1") distfunc <- function(x) dist(x, method="manhattan")
    if(input$hfun=="Single") hcluster <- function(x) fastcluster::hclust(x, method="single")
    if(input$hfun=="Ward") hcluster <- function(x) fastcluster::hclust(x, method="ward.D")
    if(input$hfun=="Complete") hcluster <- function(x) fastcluster::hclust(x, method="complete")
    if(input$hfun=="Average") hcluster <- function(x) fastcluster::hclust(x, method="average")
    if(input$hfun=="Mcquitty") hcluster <- function(x) fastcluster::hclust(x, method="mcquitty")
    if(input$hfun=="Median") hcluster <- function(x) fastcluster::hclust(x, method="median")
    if(input$hfun=="Centroid") hcluster <- function(x) fastcluster::hclust(x, method="centroid")
    #if(input$distfun=="Signed L2") distfunc <- function(x) as.dist(sign(1-x)*sqrt(1-x^2))
    heatmap.2(N, distfun = distfunc, hclustfun = hcluster,
              #cellnote = mat_data,  # same data set for cell labels
              main = "Clustered Entities", # heat map title
              key=T, key.title = "color key", key.xlab = "correlation",
              #keysize=.8, 
              symkey=F, symm=T, symbreaks=F, #denscol="grey",
              #notecol="black",      # change font color of cell labels to black
              #density.info="none",  # turns off density plot inside color legend
              trace="none",         # turns off trace lines inside the heat map
              margins =c(5,5),     # widens margins around plot
              col=my_palette,       # use on color palette defined earlier
              #breaks=col_breaks,    # enable color transition at specified limits
              dendrogram="both",     # only draw a row dendrogram
              Colv=T)            # turn off column clustering
    
    rmatrix.plot <<- recordPlot()
    replayPlot(rmatrix.plot) # redraw
})


# Training models
output$dependent <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
        items=names(df)
        names(items)=items
        selectInput("dependent","Select variable to predict:", items, selected="Survivor", selectize=T)
    #}
    #else return(NULL)
})
output$algorithms <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){
        selectInput("algorithms","Machine learning algorithm:", 
                    c("GLM (Partial Least Squares)" = "pls",
                      "GLM"="glm",
                      "GLM (Bayesian)"="bayesglm",
                      "GLM (step AIC)"="glmStepAIC",
                      "GLM (Elastic Net)"="glmnet",
                      "PCA"="pcr",
                      "ICA"="icr",
                      "Linear Discriminant Analysis"="lda",
                      "Decision Tree"="rpart",
                      "Decision Tree (boosted)"="gbm",
                      "Random Forest"= "rf",
                      "SVM (Linear)"="svmLinear",
                      "SVM (Radial)"="svmRadial",
                      "Kernal Partial Least Squares" = "widekernelpls",
                      "Linear Distance Weighted Discrimination"="dwdLinear",
                      "Adaptive Regression Spline"="earth",
                      "Neural Network"="nnet",
                      "Penalized Multinomial Regression"="multinom",
                      "Self-Organizing Map"="xyf",
                      "Naive Bayes"="nb",
                      "k-nearest neighbors"="knn")
                    ,multiple=F, selected=input$algorithms)
    } else return(NULL)
})
output$prep <- output$prep2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){ 
        checkboxInput("prep", "Center and scale variables", value=T, width='100%') 
        } else return(NULL)
})

output$pcttrain <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        #if (input$details==F) return()
        sliderInput("pcttrain", "% split used for training/testing", value=input$pcttrain, min = 50, max = 100, step = 5, width = "100%")
    } else return(NULL)
})
output$ntune <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
    #if (input$details==F) return()
        sliderInput("ntune", "Number of tuning points:", value=input$ntune, min = 1, max = 10, step = 1, width = "100%")
    } else return(NULL)
})
output$nfold <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
    #if (input$details==F) return()
        sliderInput("nfold", "Number of CV folds:", min = 2, max = 30, value=input$nfold, step = 1, width = "100%")
    } else return(NULL)
})

# subset training/testing data
output$resamplechk  <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        checkboxInput("resamplechk", "Respect repeated measures", value=input$resamplechk, width='100%')
    }
})
output$resampleid  <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){
        if(input$resamplechk==F) return()
        items=names(df)
        selectInput("resampleid", "Choose ID label", items, selected=input$resampleid, selectize=T)
    }
})
output$subschktest  <- renderUI({
    df <- filedata()
    #if (is.null(df)) return(NULL)
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        checkboxInput("subschktest", "Test on subgroup", value=input$subschktest, width='100%')
    }
})
output$subsvartest  <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        if(input$subschktest==T){
            items <- levels(df[,input$subs])
            selectInput("subsvartest","Choose testing levels", items[-which(items %in% input$subsvar)], multiple=T, selected=input$subsvartest, selectize=T)
        }
    }
    #if(is.null(input$subschktest) | input$subschktest==0 | input$subschktest==F) return()
    #if (is.null(df) | is.null(input$subs)) return(NULL)
    #if (is.null(df) | input$subs=="") return(NULL)
    #if (input$subs==F) return()
    
})


output$ui.action <- renderUI({
    #if (is.null(input$file1)) return()
    if (analysis() %in% c("Modeling")){
        actionButton("action", "Learn", icon=icon("play"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    } else return(NULL)
})
model.fit <- reactive({
    #if (is.null(input$action)) return()
    #if (input$action==0) return()
    
    #isolate({
        df <- filedata()
        # isolate({
        if(input$allvars == TRUE){
            predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
        } else {
            predicto <- c(input$independents)
            if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
            if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
            if(input$demovars == T) {predicto <- c(predicto, demovars)}
            if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
            if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
            if(input$bcells == T) {predicto <- c(predicto, bcells)}
            if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
            if(!is.null(input$dependent)) {
                index <- which(predicto %in% input$dependent)
                if(any(index)==T){
                    predicto <- predicto[-index]
                } else { predicto <- predicto}
            }
            if(!is.null(input$removed)) {
                index <- which(predicto %in% input$removed)
                if(any(index)==T){
                    predicto <- predicto[-index]
                } else { predicto <- predicto}
                
            }
        }
        
        
        fmla <- as.formula(paste(input$dependent," ~ ."))
        

        
        # conditions
        byID <- input$resamplechk
        bySubset <- input$subschk
        testSubset <- input$subschktest
        dependents <- input$dependent
        whichSubset <- input$subs
        subsetLevel <- input$subsvar
        subsetLevelTest <- input$subsvartest
        identifier <- input$resampleid
        
        #subsetLevelTest <- "t5"
        #subsetLevel <- "t1" 
        #dependents <- "Survivor"
        #predicto <- c("MELDXI","SOFA","Timepoint")
        #identifier <- "PatientID"
        #whichSubset <- "Timepoint"
        
        get_testing_ids <- function(df){
            uniqID.df <- df[ !duplicated(df[,identifier]),c(identifier,dependents) ]
            temp <- createDataPartition(y = uniqID.df[,dependents], p = input$pcttrain/100, list = FALSE) 
            inTrainID <- droplevels(uniqID.df[-temp,identifier])
            #inTrainIndex <- which(df[,identifier] %in% inTrainID)
            #inTrainIndex
            inTrainID
        }
        
        
        if(bySubset==T){
            if(testSubset==T){
                # training and testing different subsets
                if(byID==T){
                    # repeated measures blocking
                    # do na.omit on training and testing subsets separately
                    training.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
                    testing.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevelTest )))
                    # split the testing set by splitting at ID level, get the IDs for the test entities
                    trainID <- get_testing_ids(testing.tmp)
                    # get the testing set 
                    testing <- testing.tmp[which(testing.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
                    # get the training set by excluding any members that are in the testing set.
                    training <- training.tmp[-which(training.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
                    
                } else if(byID==F){
                    # don't respect ID
                    training <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
                    testing <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevelTest )))
                    #training <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,input$subs] %in% subsetLevel )))
                    #testing <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,input$subs] %in% subsetLevelTest )))
                }
            } else if(testSubset==F){
                # training and testing on same subset
                if(byID==T){
                    # repeated measures blocking
                    # do na.omit on training and testing subsets separately
                    data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
                    # split the testing set by splitting at ID level, get the IDs for the test entities
                    trainID <- get_testing_ids(data.tmp)
                    # get the testing set 
                    testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
                    # get the training set by excluding any members that are in the testing set.
                    training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
                } else if(byID==F){
                    # don't respect ID
                    data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
                    inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
                    #inTrain <- createDataPartition(y = data.tmp[,input$dependent], p = input$pcttrain/100, list = FALSE)  
                    training <- data.tmp[ inTrain,]
                    testing  <- data.tmp[-inTrain,]
                }
            }
        } else if(bySubset==F){
            # training and testing on whole
            if(byID==T){
                # repeated measures blocking
                # do na.omit on dataset
                data.tmp <- droplevels(na.omit( df[,c(dependents, predicto, identifier)] ))
                # split the testing set by splitting at ID level, get the IDs for the test entities
                trainID <- get_testing_ids(data.tmp)
                # get the testing set 
                testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
                # get the training set by excluding any members that are in the testing set.
                training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
            } else if(byID==F){
                # don't respect ID
                #dat <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
                data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
                inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
                #inTrain <- createDataPartition(y = data.tmp[,input$dependent], p = input$pcttrain/100, list = FALSE)  
                training <- data.tmp[ inTrain,]
                testing  <- data.tmp[-inTrain,]
            }
        }
        
        
        
#         if(input$subschk==F) {
#             dat <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
#         } else if(input$subschk==T & (subschktest==F | is.null(subschktest))){
#             dat <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar )))
#             
#         } else if(input$subschk==T & subschktest==T){
#             dat <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar )))
#         }
#         
#         
#         if(is.null(input$subschktest) | input$subschktest==F){
#             inTrain <- createDataPartition(y = dat[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#             training <- dat[ inTrain,]
#             testing  <- dat[-inTrain,]
#         } else if(input$subchktest==T){
#             training <- dat
#             testing <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvartest )))
#         }
        
        
        if(class(df[,input$dependent]) %in% c("factor","character")){
            ctrl <- trainControl(method = "LOOCV", 
                                 #number = input$nfold, 
                                 classProbs = T, summaryFunction = twoClassSummary)
            if(input$prep==F){
                invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
            } else{
                invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
            }
            pred <- predict(model1, newdata=training)
            conf <- confusionMatrix(pred, training[,input$dependent])
            pred2 <- predict(model1, newdata=testing)
            conf2 <- confusionMatrix(pred2, testing[,input$dependent])
            
        } else{
            ctrl <- trainControl(method = "LOOCV"
                                 #, number = input$nfold
            )
            if(input$prep==F){
                invisible(model1 <- train(fmla,method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune))
            } else{
                invisible(model1 <- train(fmla,method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
            }
            
            pred <- predict(model1, newdata=training)
            conf <- paste("R^2 =", signif(cor(pred, training[,input$dependent])^2,3))
            pred2 <- predict(model1, newdata=testing)
            conf2 <- paste("R^2 =", signif(cor(pred2, testing[,input$dependent])^2,3))
        }
        return(list(fit=model1, conf=conf, conf2=conf2, pred=pred, pred2=pred2, training=training, testing=testing, predictors=predictors))
#    })
    
})
output$contents <- renderPrint({
    #if (is.null(input$action)) {print("You must press Learn to train the model."); return()}
    #if (input$action==0) {print("You must press Learn to train the model."); return()}
    model <- model.fit()
    #if(is.null(model)) return(NULL)
    print("Accuracy on training set")
    print(model$conf)
    print("Accuracy on testing set")
    print(model$conf2)
    vip <<- varImp(model$fit, scale = T)
    vip
})
output$vip <- renderPlot({
    model <- model.fit()
    #if(is.null(model)) return(NULL)
    plot( varImp(model$fit, scale = F), main=input$algorithms)
})
output$prediction <- renderPlot({
    model <<- model.fit()
    #if(is.null(model)) return(NULL)
    data.pred <- model$testing
    data.pred.1 <- model$training
    
    lm_eqn <- function(df){
        m <- lm(pr ~ ref, df);
        paste("R^2 = ", signif(summary(m)$r.squared,3), sep="" )               
    }
    
    if(class(data.pred[,input$dependent]) %in% c("factor","character")){
        df2 <<- data.frame(pr=predict(model$fit, newdata=data.pred, type="prob"), ref=data.pred[,c(input$dependent)])
        df2.1 <<- data.frame(pr=predict(model$fit, newdata=data.pred.1, type="prob"), ref=data.pred.1[,c(input$dependent)])
        
        p <- ggplot(df2, aes_string(x="ref", y=names(df2)[1], fill="ref")) + geom_violin() + ylim(0,1)+ scale_color_brewer(palette="Dark2")  + ggtitle("Testing Set Performance")
        preds.plot <<- p + geom_boxplot(notch=T, width=0.35, size=1, alpha=.1) + geom_jitter(shape=16, position=position_jitter(0.2), alpha=.5) + theme_bw() +ylab("Predicted Class") +xlab("Actual Class") 
        
        p.1 <- ggplot(df2.1, aes_string(x="ref", y=names(df2.1)[1], fill="ref")) + geom_violin() + ylim(0,1) + scale_color_brewer(palette="Dark2")  + ggtitle("Training Set Performance")
        preds.plot.1 <<- p.1 + geom_boxplot(notch=T, width=0.35, size=1, alpha=.1) + geom_jitter(shape=16, position=position_jitter(0.2), alpha=.5) + theme_bw() +ylab("Predicted Class") +xlab("Actual Class") 
        
        grid.arrange(preds.plot.1, preds.plot, ncol = 2)
        
    } else if(class(data.pred[,input$dependent]) %in% c("integer","numeric")){
        df2 <<- data.frame(pr=model$pred2, ref=data.pred[,c(input$dependent)])
        df2.1 <<- data.frame(pr=model$pred, ref=data.pred.1[,c(input$dependent)])
        
        p <- ggplot(df2, aes_string(x="ref", y=names(df2)[1])) + scale_color_brewer(palette="Dark2") 
        preds.plot <<- p + geom_point(shape=16, alpha=.5) + theme_bw() +ylab("Predicted Value") +xlab("Actual Value") +stat_smooth(method="lm") + ggtitle(paste("Testing Set Performance: ",lm_eqn(df2),sep=""))
        
        p.1 <- ggplot(df2.1, aes_string(x="ref", y=names(df2.1)[1])) + scale_color_brewer(palette="Dark2") 
        preds.plot.1 <<- p.1 + geom_point(shape=16, alpha=.5) + theme_bw() +ylab("Predicted Value") +xlab("Actual Value") +stat_smooth(method="lm") + ggtitle(paste("Training Set Performance: ",lm_eqn(df2.1),sep=""))
        
        grid.arrange(preds.plot.1, preds.plot, ncol = 2)
    }
})
output$summary <- renderPrint({
    model <- model.fit()
    print(summary(model$fit))
})
output$summary2 <- renderPrint({
    model <- model.fit()
    print(model$fit$finalModel)
})

# Bayesian network
output$bnalgo <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Bayesian Network")){
        items=names(df)
        names(items)=items
        selectInput("bnalgo","Select BN algorithm:", c("Grow-Shrink (GS)"="gs",
                                                              "Incremental Association (IAMB)"="iamb", 
                                                              "Fast Incremental Association (fIAMB)"="fast.iamb",
                                                              "Interleaved Incremental Association (Inter-IAMB)"="inter.iamb",
                                                              "hill-climbing (HC)"="hc", 
                                                              "Tabu Search"="tabu",
                                                              "Max-Min Hill-Climbing"="mmhc",
                                                              "Restricted Maximization"="rsmax2",
                                                              "Max-Min Parents and Children"="mmpc",
                                                              "Hiton Parents and Children"="si.hiton.pc",
                                                              "Chow-Liu"="chow.liu",
                                                              "ARACNE"="aracne"),
                    
                    selected="hc", selectize=T)
    }
    else return(NULL)
})
output$bnlayout <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Bayesian Network")){
        selectInput("bnlayout","Select BN layout:", c("dot"="dot",
                                                       "neat"="neato", 
                                                       "circular"="twopi",
                                                       "perfect circle"="circo",
                                                       "fdp"="fdp" 
                                                       ),
                    
                    selected="fdp", selectize=T)
    }
    else return(NULL)
})

output$bn <- renderPlot({
    #if (is.na(input$bnalgo)) return()
    df <- filedata()
    # isolate({
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent))]
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$removed, input$dependent))]
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents)
        if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        if(input$demovars == T) {predicto <- c(predicto, demovars)}
        if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        if(input$bcells == T) {predicto <- c(predicto, bcells)}
        if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) { index <- which(predicto %in% c(input$removed, input$dependent)) }
        if(is.null(input$removed)) { index <- which(predicto %in% c(input$dependent)) }
        if(any(index)==T){
                predicto <- predicto[-index]
        } else { predicto <- predicto}
        
    }
    #print(which(predicto %in% input$removed))
    print(predicto)
    
    if(input$subschk==F) {
        bndata <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
        
    } else {
        bndata <-droplevels( na.omit( subset(
            df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar
        )))
    }
    
    fillcolors <- rep("white", ncol(bndata))
    fillcolors[which(names(bndata) %in% demovars)] <- "green"
    fillcolors[which(names(bndata) %in% phenovars)] <- "pink"
    fillcolors[which(names(bndata) %in% eigvars)] <- "cyan"
    fillcolors[which(names(bndata) %in% t1vars)] <- "orange"
    fillcolors[which(names(bndata) %in% t3vars)] <- "magenta"
    fillcolors[which(names(bndata) %in% bcells)] <- "turquoise"
    fillcolors[which(names(bndata) %in% cytokine)] <- "red"
    fillcolors[which(names(bndata) %in% input$dependent)] <- "yellow"
    

    if(input$bnalgo=="gs") alarm <- gs(bndata, score="aic")
    if(input$bnalgo=="iamb") alarm <- iamb(bndata)
    if(input$bnalgo=="fast.iamb") alarm <- fast.iamb(bndata)
    if(input$bnalgo=="inter.iamb") alarm <- inter.iamb(bndata)
    if(input$bnalgo=="mmpc") alarm <- mmpc(bndata)
    if(input$bnalgo=="hc") alarm <- hc(bndata,restart=1, perturb=2)
    if(input$bnalgo=="tabu") alarm <- tabu(bndata)
    if(input$bnalgo=="mmhc") alarm <- mmhc(bndata)
    if(input$bnalgo=="rsmax2") alarm <- rsmax2(bndata)
    if(input$bnalgo=="si.hiton.pc") alarm <- si.hiton.pc(bndata)
    if(input$bnalgo=="aracne") alarm <- aracne(bndata)
    if(input$bnalgo=="chow.liu") alarm <- chow.liu(bndata)
    highlight.opts <- list(nodes = names(bndata), col = "black", fill = fillcolors)
    bn.layout <- input$bnlayout
    graphviz.plot(alarm, highlight = highlight.opts, shape="rectangle", main="Bayesian Network", layout=bn.layout)
    #arcs = boot.strength(bndata, algorithm = input$bnalgo, R=10)
    #graphviz.plot(averaged.network(arcs, threshold=.35), highlight = highlight.opts, shape="rectangle", main="Averaged Bayesian Netowrk")
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