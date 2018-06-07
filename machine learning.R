# machine learning
#print("machine learning")

npred_model <- function(){
    print("npred_model")
    preds <- model.fit()
    #screenheight * (.5 + ncol(pca$dat)/300)
    if(ncol(preds$training)<20){ 600 } else{200+ ncol(preds$training)*15}
}

#print("machine learning 2")
# Training models
output$dependent <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    #selectInput("dependent","Select variable to predict:", items, selected="Self_Win", selectize=T)
    selectInput("dependent","Select variable to predict:", items, selected=dependent.init, selectize=T)
    #}
    #else return(NULL)
})


#print("machine learning 3")
output$algorithms <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){
        selectInput("algorithms","Machine learning algorithm:", 
                    c(
                      "GLM"="glm",
                      "GLM (Partial Least Squares)" = "pls",
                     
                      "GLM (Bayesian)"="bayesglm",
                      "GLM (step AIC)"="glmStepAIC",
                      "GLM (Elastic Net)"="glmnet",
                      "PCA"="pcr",
                      "ICA"="icr",
                      "Linear Discriminant Analysis"="lda",
                      "Conditional Inference Tree" = "ctree",
                      "Decision Tree"="rpart",
                      "Decision Tree C5.0"="C5.0",
                      "Decision Tree (boosted)"="gbm",
                      "Cubist"="cubist",
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

#print("machine learning 4")
output$usepcs <- renderUI({
    #if(is.null(input$algorithms)) return()
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling","GLMulti")){ 
        check <- T
        if(is.null(input$usepcs)) {check <- F} else { check <- input$usepcs}
        #if(input$algorithms=="rpart") { check <- F }
        checkboxInput("usepcs", "Use principal components", value=check, width='100%') 
    } else return(NULL)
})

#print("machine learning 5")
output$prep <- renderUI({
    #if(is.null(input$algorithms)) return()
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling","GLMulti")){ 
        check <- T
        if(is.null(input$prep)) {check <- T} else { check <- input$prep}
        #if(input$algorithms=="rpart") { check <- F }
        checkboxInput("prep", "Center and scale variables", value=check, width='100%') 
    } else return(NULL)
})

#print("machine learning 6")
output$pcttrain <- renderUI({
    #df <- filedata()
    #if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        #if (input$details==F) return()
        sliderInput("pcttrain", "% split used for training/testing", value=input$pcttrain, min = 70, max = 100, step = 5, width = "100%")
    } else return(NULL)
})

#print("machine learning 7")
output$ntune <- renderUI({
    #df <- filedata()
    #if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        #if (input$details==F) return()
        nvalue <- input$ntune
        #if(input$algorithms=="rpart") {nvalue <- 30}
        sliderInput("ntune", "Number of tuning points:", value=input$ntune, min = 2, max = 50, step = 1, width = "100%")
    } else return(NULL)
})

#print("machine learning 8")
output$nfold <- renderUI({
    #df <- filedata()
    #if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        #if (input$details==F) return()
        sliderInput("nfold", "Number of CV folds:", min = 4, max = 30, value=input$nfold, step = 1, width = "100%")
    } else return(NULL)
})

# subset training/testing data
#print("machine learning 9")
output$resamplechk  <- renderUI({
    #df <- filedata()
    #if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        if(resamplechk.init == T){
            values <- T
        } else values<-input$resamplechk
        checkboxInput("resamplechk", "Respect repeated measures", value=values, width='100%')
    }
})

#print("machine learning 10")
output$resampleid  <- renderUI({
    if(is.null(input$resamplechk)) return(NULL)
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){
        if(input$resamplechk==F) return()
        items=names(df)
        selectInput("resampleid", "Choose ID label", items, selected=input$resampleid, selectize=T)
    }
})

#print("machine learning 11")
output$subschktest  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    if (analysis() %in% c("Modeling")){
        checkboxInput("subschktest", "Test on subgroup", value=input$subschktest, width='100%')
    }
})

#print("machine learning 12")
output$subsvartest  <- renderUI({
    if(is.null(input$subschktest)) return(NULL)
    df <- filedata()
    if (is.null(df) ) return(NULL)
    if (analysis() %in% c("Modeling")){
        if(input$subschktest==T){
            if(length(input$subs)>1){
                items <- unlist(sapply(df[,input$subs],levels))
                names(items) <- items
            } else {
                items <- levels(df[,input$subs])
            }
            
            #selectInput("subsvartest","Choose testing levels", items[-which(items %in% input$subsvar)], multiple=T, selected=input$subsvartest, selectize=T)
            selectInput("subsvartest","Choose testing levels", items, multiple=T, selected=input$subsvartest, selectize=T)
        }
    }
    #if(is.null(input$subschktest) | input$subschktest==0 | input$subschktest==F) return()
    #if (is.null(df) | is.null(input$subs)) return(NULL)
    #if (is.null(df) | input$subs=="") return(NULL)
    #if (input$subs==F) return()
    
})


# output$ui.action <- renderUI({
#     #if (is.null(input$file1)) return()
#     if (analysis() %in% c("Modeling")){
#         actionButton("action", "Learn", icon=icon("play"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
#     } else return(NULL)
# })

#print("machine learning 13")
# model.fit <- reactive({
# 
# #      if (is.null(input$action)) return()
# #      if (input$action==0) return()
# #       if(is.null(input$allvars)) return()
# #       if(is.null(input$subschk)) return()
# #       if(is.null(input$resamplechk)) return()
# #       if(is.null(input$prep)) return()
# #      if(is.null(input$usepcs)) stop("usepcs")
#     
#     if(input$usepcs==T){
#         pca <- getpca()
#         pcs <- pca$pca$x
#         keep95 <- (cumsum(pca$pca$sdev)/sum(pca$pca$sdev))*100 < 95
#         y <- pca$wfactor[,input$dependent]
#         df <- data.frame(pcs[,keep95])
#         df[,input$dependent] <- y
#         #names(df)[1] <- input$dependent
#         #print(df)
#         inTrain <- createDataPartition(y = pca$wfactor[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#         #inTrain <- createDataPartition(y = data.tmp[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#         training <- df[ inTrain,]
#         testing  <- df[-inTrain,]
#         fmla <- as.formula(paste(input$dependent," ~ ."))
#         #pcdf <<- df
#         # conditions
#         if(is.null(input$resamplechk)) {
#             byID <- F
#         } else { byID <- input$resamplechk }
#         
#         if(is.null(input$subschk)) {
#             bySubset <- F
#         } else { bySubset <- input$subschk }
#         
#         if(is.null(input$subschktest)) {
#             testSubset <- F
#         } else { testSubset <- input$subschktest }
#         
#         if(is.null(input$prep)) {
#             prepvar <- T
#         } else { prepvar <- input$prep }
#         
#         #bySubset <- input$subschk
#         #testSubset <- input$subschktest
#         dependents <- input$dependent
#         whichSubset <- input$subs
#         subsetLevel <- input$subsvar
#         subsetLevelTest <- input$subsvartest
#         identifier <- input$resampleid
#     } else if(input$usepcs==F){
#         #isolate({
#         df <- filedata()
#         # isolate({
#         if(input$allvars == TRUE){
#             predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
#         } else {
#             predicto <- c(input$independents)
#             
#             predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
#             #if(input$predgroups == "a"){ predicto <- c(predicto, predgroupnames[["a"]]) }
#             
#             #         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
#             #         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
#             #         if(input$demovars == T) {predicto <- c(predicto, demovars)}
#             #         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
#             #         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
#             #         if(input$bcells == T) {predicto <- c(predicto, bcells)}
#             #         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
#             if(!is.null(input$dependent)) {
#                 index <- which(predicto %in% input$dependent)
#                 if(any(index)==T){
#                     predicto <- predicto[-index]
#                 } else { predicto <- predicto}
#             }
#             if(!is.null(input$removed)) {
#                 index <- which(predicto %in% input$removed)
#                 if(any(index)==T){
#                     predicto <- predicto[-index]
#                 } else { predicto <- predicto}
#                 
#             }
#         }
#         
#         
#         fmla <- as.formula(paste(input$dependent," ~ ."))
#         
#         
#         
#         # conditions
#         if(is.null(input$resamplechk)) {
#             byID <- F
#         } else { byID <- input$resamplechk }
#         
#         if(is.null(input$subschk)) {
#             bySubset <- F
#         } else { bySubset <- input$subschk }
#         
#         if(is.null(input$subschktest)) {
#             testSubset <- F
#         } else { testSubset <- input$subschktest }
#         
#         if(is.null(input$prep)) {
#             prepvar <- T
#         } else { prepvar <- input$prep }
#         
#         #bySubset <- input$subschk
#         #testSubset <- input$subschktest
#         dependents <- input$dependent
#         whichSubset <- input$subs
#         subsetLevel <- input$subsvar
#         subsetLevelTest <- input$subsvartest
#         identifier <- input$resampleid
#         
#         #subsetLevelTest <- "t5"
#         #subsetLevel <- "t1" 
#         #dependents <- "Survivor"
#         #predicto <- c("MELDXI","SOFA","Timepoint")
#         #identifier <- "PatientID"
#         #whichSubset <- "Timepoint"
#         
#         get_testing_ids <- function(df2){
#             uniqID.df <- df2[ !duplicated(df2[,identifier]),c(identifier,dependents) ]
#             temp <- createDataPartition(y = uniqID.df[,dependents], p = input$pcttrain/100, list = FALSE) 
#             inTrainID <- droplevels(uniqID.df[-temp,identifier])
#             #inTrainIndex <- which(df[,identifier] %in% inTrainID)
#             #inTrainIndex
#             inTrainID
#         }
#         
#         
#         if(bySubset==T){
#             if(testSubset==T){
#                 # training and testing different subsets
#                 if(byID==T){
#                     # repeated measures blocking
#                     # do na.omit on training and testing subsets separately
#                     data.tmp <- subset( df[,c(dependents, predicto, identifier, whichSubset)],  df[,whichSubset] %in% c(subsetLevel, subsetLevelTest) )
#                     toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
#                     if(is.na(toomanymissing[1])){
#                         df <- droplevels(na.omit(data.tmp))
#                     } else {
#                         df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     }
#                     #df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     remv <- which(names(df)==whichSubset)
#                     training.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
#                     testing.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
#                     #training.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
#                     #testing.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevelTest )))
#                     
#                     # split the testing set by splitting at ID level, get the IDs for the test entities
#                     trainID <- get_testing_ids(testing.tmp)
#                     # get the testing set 
#                     testing <- testing.tmp[which(testing.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
#                     # get the training set by excluding any members that are in the testing set.
#                     training <- training.tmp[-which(training.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
#                     
#                 } else if(byID==F){
#                     # don't respect ID
#                     if(length(input$subs)>1){
#                         insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar, subsetLevelTest)}), 1, prod))
#                         data.tmp <- df[insubgroup, c(dependents, predicto, input$subs)] 
#                     } else {
#                         #insubgroup <- df[,input$subs] %in% input$subsvar
#                         data.tmp <- subset( df[,c(dependents, predicto, input$subs)],  df[,input$subs] %in% c(input$subsvar, subsetLevelTest)  )
#                     }
#                     
#                     
#                     #data.tmp <- subset( df[,c(dependents, predicto, whichSubset)],  df[,whichSubset] %in% c(subsetLevel, subsetLevelTest) )
#                     toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
#                     if(is.na(toomanymissing[1])){
#                         df <- droplevels(na.omit(data.tmp))
#                     } else {
#                         df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     }
#                     #df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     remv <- which(names(df) %in% whichSubset)
#                     
#                     
#                     if(length(input$subs)>1){
#                         insubgroup.train <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar)}), 1, prod))
#                         insubgroup.test <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% subsetLevelTest}), 1, prod))
#                         training <- df[insubgroup.train, -remv] 
#                         testing <- df[insubgroup.test, -remv]
#                         
#                         #training <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
#                         #testing <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
#                     } else {
#                         training <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
#                         testing <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
#                         #training <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
#                         #testing <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevelTest )))
#                     }
#                     
#                     
#                 }
#             } else if(testSubset==F){
#                 # training and testing on same subset
#                 if(byID==T){
#                     # repeated measures blocking
#                     # do na.omit on training and testing subsets separately
#                     data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
#                     # split the testing set by splitting at ID level, get the IDs for the test entities
#                     trainID <- get_testing_ids(data.tmp)
#                     # get the testing set 
#                     testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#                     # get the training set by excluding any members that are in the testing set.
#                     training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#                 } else if(byID==F){
#                     # don't respect ID
#                     #data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
#                     if(length(input$subs)>1){
#                         insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% input$subsvar}), 1, prod))
#                         data.tmp <- df[insubgroup, c(dependents, predicto)] 
#                     } else {
#                         #insubgroup <- df[,input$subs] %in% input$subsvar
#                         data.tmp <- subset( df[,c(dependents, predicto)],  df[,input$subs] %in% input$subsvar  )
#                     }
#                     
#                     
#                     #data.tmp <- subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )
#                     toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
#                     if(is.na(toomanymissing[1])){
#                         data.tmp <- droplevels(na.omit(data.tmp))
#                     } else {
#                         data.tmp <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     }
#                     #data.tmp <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                     #toomanymissing <- which(sapply(csv, function(x) sum(is.na(x)))/nrow(csv) > .3)
#                     
#                     inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#                     #inTrain <- createDataPartition(y = data.tmp[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#                     training <- data.tmp[ inTrain,]
#                     testing  <- data.tmp[-inTrain,]
#                 }
#             }
#         } else if(bySubset==F){
#             # training and testing on whole
#             if(byID==T){
#                 # repeated measures blocking
#                 # do na.omit on dataset
#                 data.tmp <- droplevels(na.omit( df[,c(dependents, predicto, identifier)] ))
#                 # split the testing set by splitting at ID level, get the IDs for the test entities
#                 trainID <- get_testing_ids(data.tmp)
#                 # get the testing set 
#                 testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#                 # get the training set by excluding any members that are in the testing set.
#                 training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#             } else if(byID==F){
#                 # don't respect ID
#                 #dat <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
#                 
#                 
#                 data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
#                 inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#                 #inTrain <- createDataPartition(y = data.tmp[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#                 training <- data.tmp[ inTrain,]
#                 testing  <- data.tmp[-inTrain,]
#             }
#         }
#         
#     }
#     
# 
#     
#     
#     #         if(input$subschk==F) {
#     #             dat <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
#     #         } else if(input$subschk==T & (subschktest==F | is.null(subschktest))){
#     #             dat <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar )))
#     #             
#     #         } else if(input$subschk==T & subschktest==T){
#     #             dat <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar )))
#     #         }
#     #         
#     #         
#     #         if(is.null(input$subschktest) | input$subschktest==F){
#     #             inTrain <- createDataPartition(y = dat[,input$dependent], p = input$pcttrain/100, list = FALSE)  
#     #             training <- dat[ inTrain,]
#     #             testing  <- dat[-inTrain,]
#     #         } else if(input$subchktest==T){
#     #             training <- dat
#     #             testing <- droplevels(na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvartest )))
#     #         }
#     
#     
#     if(class(df[,input$dependent]) %in% c("factor","character") & length(levels(df[,input$dependent]))==2){
#         if(trainmethod=="cv"){
#             ctrl <- trainControl(method = "cv", 
#                                  number = input$nfold, 
#                                  classProbs = T, summaryFunction = twoClassSummary)
#         } else if(trainmethod=="LOOCV"){
#             ctrl <- trainControl(method = "LOOCV", 
#                                  #number = input$nfold, 
#                                  classProbs = T, summaryFunction = twoClassSummary)
#         }
#         
#         if(prepvar==F){
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
#         } else{
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#         }
#         pred <- predict(model1, newdata=training)
#         conf <- confusionMatrix(pred, training[,input$dependent])
#         pred2 <- predict(model1, newdata=testing)
#         conf2 <- confusionMatrix(pred2, testing[,input$dependent])
#         
#     } else if(class(df[,input$dependent]) %in% c("factor","character") & length(levels(df[,input$dependent]))>=2){
#         if(trainmethod=="cv"){
#             ctrl <- trainControl(method = "cv", 
#                                  number = input$nfold, 
#                                  classProbs = T)
#         } else if(trainmethod=="LOOCV"){
#             ctrl <- trainControl(method = "LOOCV", 
#                                  #number = input$nfold, 
#                                  classProbs = T)
#         }
# 
#         if(prepvar==F){
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training,  trControl = ctrl, tuneLength=input$ntune))
#         } else{
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training,  trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#         }
#         pred <- predict(model1, newdata=training)
#         conf <- confusionMatrix(pred, training[,input$dependent])
#         pred2 <- predict(model1, newdata=testing)
#         conf2 <- confusionMatrix(pred2, testing[,input$dependent])
#         
#     } else{
#         if(trainmethod=="cv"){
#             ctrl <- trainControl(method = "cv", 
#                                  number = input$nfold)
#         } else if(trainmethod=="LOOCV"){
#             ctrl <- trainControl(method = "LOOCV") 
#         }
# 
#         if(prepvar==F){
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune))
#         } else{
#             if(input$algorithms=="nnet"){
#                 normalized <- function(x) (x-min(x))/(max(x)-min(x))
#                 training[,input$dependent] <- normalized(training[,input$dependent])
#             }
#             #invisible(model1 <- train(fmla,method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#         }
#         
#         pred <- predict(model1, newdata=training)
#         conf <- paste("R^2 =", signif(cor(pred, training[,input$dependent])^2,3))
#         pred2 <- predict(model1, newdata=testing)
#         conf2 <- paste("R^2 =", signif(cor(pred2, testing[,input$dependent])^2,3))
#     }
#     return(list(fit=model1, conf=conf, conf2=conf2, pred=pred, pred2=pred2, training=training, testing=testing, predictors=predictors))
#     #    })
#     
# })


# model.fit <- function(){
#     return(list(fit=NULL, conf=NULL, conf2=NULL, pred=NULL, pred2=NULL, training=NULL, testing=NULL, predictors=NULL))
# }
#     

source("model_fit.R", local=TRUE)

#print("machine learning 14")
output$contents <- renderPrint({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    
    
    #input$pcaButton
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         #if(is.null(input$resamplechk)) stop("resamplechk is null")
#         #if(is.null(input$prep)) stop("prep: center and scale")
        model <- model.fit()
        print("Goodness-of-fit on testing set")
        print(model$conf2)
        print("Goodness-of-fit on training set")
        print(model$conf)
       
    })
})

#print("machine learning 15")
output$vip <- renderPlot({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         if(is.null(input$prep)) stop("prep: center and scale")
#         if(is.null(input$resamplechk)) stop("resamplechk is null")
        model <- model.fit()
        #if(is.null(model)) return(NULL)
        plot( varImp(model$fit, scale = F), main=input$algorithms)
    })

}, height=npred_model)

#print("machine learning 16")
output$prediction <- renderPlot({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         if(is.null(input$prep)) stop("prep: center and scale")
#         if(is.null(input$resamplechk)) stop("resamplechk is null")
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
   
}, height=screenheight)

#print("machine learning 17")
output$summary <- renderPrint({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         if(is.null(input$prep)) stop("prep: center and scale")
#         if(is.null(input$resamplechk)) stop("resamplechk is null")
        model <- model.fit()
        vip <<- varImp(model$fit, scale = T)
        vip
        print(summary(model$fit))
    })
 
})

#print("machine learning 18")
output$summary2 <- renderPrint({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         if(is.null(input$prep)) stop("prep: center and scale")
#         if(is.null(input$resamplechk)) stop("resamplechk is null")
        model <- model.fit()
        print(model$fit$finalModel)
    })
 
})

#print("machine learning 19")
output$plotmodel <- renderPlot({
    if(is.null(input$prep)) stop("timing glitch. Starting calculation...")
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    input$pcaButton
    isolate({
        if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$prep) | is.null(input$dependent)) stop("timing glitch loading panel. Press RUN to refresh.")
    })
    isolate({
#         if(is.null(input$allvars)) stop("allvars is null")
#         if(is.null(input$subschk)) stop("subschk is null")
#         if(is.null(input$prep)) stop("prep: center and scale")
#         if(is.null(input$resamplechk)) stop("resamplechk is null")
        model <- model.fit()
        if(input$algorithms == "rpart"){
            #fancyRpartPlot(model$fit$finalModel)
            #plot(model$fit$finalModel)
            #text(model$fit$finalModel)
            #prp(model$fit$finalModel)
            prp(model$fit$finalModel,  type=0, fallen=T, branch=.3, round=0, leaf.round=9,
                clip.right.labs=F, under.cex=1,  branch.lwd=2,
                extra=101, under=T, lt=" < ", ge=" >= ", cex.main=1.5)
            
        }  else if(input$algorithms == "nnet"){
            plotnet(model$fit$finalModel, alpha=.6, pos_col="red", neg_col="blue", all_out=F, all_in=F, prune_col="grey")
        } else
        {
            plot(model$fit$finalModel)
            #plot(model$fit)
        }
    })
    
}, height=screenheight)