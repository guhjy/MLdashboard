# get_testing_ids <- function(df, identifier, dependents, pcttrain){
#     uniqID.df <- df[ !base::duplicated(df[,identifier]),c(identifier,dependents) ]
#     temp <- createDataPartition(y = uniqID.df[,dependents], p = pcttrain/100, list = FALSE) 
#     inTrainID <- droplevels(uniqID.df[-temp,identifier])
#     inTrainID
# }
# 


modelfit_getpreds <- reactive({
    df <- filedata()
    if(input$allvars == TRUE){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
    } else {
        predicto <- c(input$independents)
        predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
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
    return(predicto)
})


set_fit_vars <- reactive({
    fmla <<- as.formula(paste(input$dependent," ~ ."))
    
    # conditions
    if(is.null(input$resamplechk)) {
        byID <<- F
    } else { byID <<- input$resamplechk }
    
    if(is.null(input$subschk)) {
        bySubset <<- F
    } else { bySubset <<- input$subschk }
    
    if(is.null(input$subschktest)) {
        testSubset <<- F
    } else { testSubset <<- input$subschktest }
    
    if(is.null(input$prep)) {
        prepvar <<- T
    } else { prepvar <<- input$prep }
    
    dependents <<- input$dependent
    whichSubset <<- input$subs
    subsetLevel <<- input$subsvar
    subsetLevelTest <<- input$subsvartest
    identifier <<- input$resampleid
})

make_traintestset <- reactive({
    df <- filedata()
    predicto <- modelfit_getpreds()
    set_fit_vars()
 
 #   for repeated measures testing
            get_testing_ids <<- function(df, identifier, dependents, pcttrain){
                uniqID.df <- df[ !base::duplicated(df[,identifier]),c(identifier,dependents) ]
                temp <- createDataPartition(y = uniqID.df[,dependents], p = pcttrain/100, list = FALSE) 
                inTrainID <- droplevels(uniqID.df[-temp,identifier])
                inTrainID
            }
    
    if(bySubset==T){
        if(testSubset==T){
            # training and testing different subsets
            if(byID==T){
                                    # repeated measures blocking
                                    # do na.omit on training and testing subsets separately
                                    data.tmp <- subset( df[,c(dependents, predicto, identifier, whichSubset)],  df[,whichSubset] %in% c(subsetLevel, subsetLevelTest) )
                                    toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
                                    if(is.na(toomanymissing[1])){
                                        df <- droplevels(na.omit(data.tmp))
                                    } else {
                                        df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
                                    }
                                    remv <- which(names(df)==whichSubset)
                                    training.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
                                    testing.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
                                    
                                    # split the testing set by splitting at ID level, get the IDs for the test entities
                                    trainID <- get_testing_ids(testing.tmp, identifier, dependents, input$pcttrain)
                                    # get the testing set 
                                    testing <- testing.tmp[which(testing.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
                                    # get the training set by excluding any members that are in the testing set.
                                    training <- training.tmp[-which(training.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
                
            } else if(byID==F){
                # don't respect ID
                if(length(input$subs)>1){
                    insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar, subsetLevelTest)}), 1, prod))
                    data.tmp <- df[insubgroup, c(dependents, predicto, input$subs)] 
                } else {
                    data.tmp <- subset( df[,c(dependents, predicto, input$subs)],  df[,input$subs] %in% c(input$subsvar, subsetLevelTest)  )
                }
                
                toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
                if(is.na(toomanymissing[1])){
                    df <- droplevels(na.omit(data.tmp))
                } else {
                    df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
                }
                remv <- which(names(df) %in% whichSubset)
                
                
                if(length(input$subs)>1){
                    insubgroup.train <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar)}), 1, prod))
                    insubgroup.test <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% subsetLevelTest}), 1, prod))
                    training <- df[insubgroup.train, -remv] 
                    testing <- df[insubgroup.test, -remv]
                } else {
                    training <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
                    testing <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
                }
            }
        } else if(testSubset==F){
            # training and testing on same subset
            if(byID==T){
                                    # repeated measures blocking
                                    # do na.omit on training and testing subsets separately
                                    data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
                                    # split the testing set by splitting at ID level, get the IDs for the test entities
                                    trainID <- get_testing_ids(data.tmp, identifier, dependents, input$pcttrain)
                                    # get the testing set 
                                    testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
                                    # get the training set by excluding any members that are in the testing set.
                                    training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
            } else if(byID==F){
                # don't respect ID
                if(length(input$subs)>1){
                    insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% input$subsvar}), 1, prod))
                    data.tmp <- df[insubgroup, c(dependents, predicto)] 
                } else {
                    data.tmp <- subset( df[,c(dependents, predicto)],  df[,input$subs] %in% input$subsvar  )
                }
                
                toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
                if(is.na(toomanymissing[1])){
                    data.tmp <- droplevels(na.omit(data.tmp))
                } else {
                    data.tmp <- droplevels(na.omit(data.tmp[,-toomanymissing]))
                }
                
                inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
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
                            trainID <- get_testing_ids(data.tmp, identifier, dependents, input$pcttrain)
                            # get the testing set 
                            testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
                            # get the training set by excluding any members that are in the testing set.
                            training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
        } else if(byID==F){
            # don't respect ID
            data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
            inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
            training <- data.tmp[ inTrain,]
            testing  <- data.tmp[-inTrain,]
        }
    }
    return(list(df=df, training=training, testing=testing))

})





model.fit <- reactive({
    if(input$resamplechk==T & is.null(input$resampleid)) stop("timing glitch: press RUN to initiate computation. ")
    
    # Use principal components as inputs
    if(input$usepcs==T){
       #isolate({
           pca1 <- getpca()
           pcs <- pca1$pca$x
           keep95 <- (cumsum(pca1$pca$sdev)/sum(pca1$pca$sdev))*100 < 90
           y <- pca1$wfactor[,input$dependent]
           df <- data.frame(pcs[,keep95])
           df[,input$dependent] <- y
           
           inTrain <- createDataPartition(y = pca1$wfactor[,input$dependent], p = input$pcttrain/100, list = FALSE)  
           training <- df[ inTrain,]
           testing  <- df[-inTrain,]
           fmla <- as.formula(paste(input$dependent," ~ ."))
           
           #setvars()
#                     # conditions
#                    if(is.null(input$resamplechk)) {
#                        byID <- F
#                    } else { byID <- input$resamplechk }
#                    
#                    if(is.null(input$subschk)) {
#                        bySubset <- F
#                    } else { bySubset <- input$subschk }
#                    
#                    if(is.null(input$subschktest)) {
#                        testSubset <- F
#                    } else { testSubset <- input$subschktest }
#                    
#                    if(is.null(input$prep)) {
#                        prepvar <- T
#                    } else { prepvar <- input$prep }
#                    
#                    dependents <- input$dependent
#                    whichSubset <- input$subs
#                    subsetLevel <- input$subsvar
#                    subsetLevelTest <- input$subsvartest
#                    identifier <- input$resampleid
           
    #   }) 
        
        
        
    # Don't use principal components as inputs
    } else if(input$usepcs==F){
        splitdata <- make_traintestset()
        df <- splitdata$df
        training <- splitdata$training
        testing <- splitdata$testing
        }

    
    if(class(df[,input$dependent]) %in% c("factor","character") & length(levels(df[,input$dependent]))==2){
        if(trainmethod=="cv"){
            ctrl <- trainControl(method = "cv", 
                                 number = input$nfold, 
                                 classProbs = T, summaryFunction = twoClassSummary)
        } else if(trainmethod=="LOOCV"){
            ctrl <- trainControl(method = "LOOCV", 
                                 #number = input$nfold, 
                                 classProbs = T, summaryFunction = twoClassSummary)
        }
        
        if(prepvar==F){
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
        } else{
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
        }
        pred <- predict(model1, newdata=training)
        conf <- confusionMatrix(pred, training[,input$dependent])
        pred2 <- predict(model1, newdata=testing)
        conf2 <- confusionMatrix(pred2, testing[,input$dependent])
        
    } else if(class(df[,input$dependent]) %in% c("factor","character") & length(levels(df[,input$dependent]))>=2){
        if(trainmethod=="cv"){
            ctrl <- trainControl(method = "cv", 
                                 number = input$nfold, 
                                 classProbs = T)
        } else if(trainmethod=="LOOCV"){
            ctrl <- trainControl(method = "LOOCV", 
                                 #number = input$nfold, 
                                 classProbs = T)
        }
        
        if(prepvar==F){
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training,  trControl = ctrl, tuneLength=input$ntune))
        } else{
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training,  trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
        }
        pred <- predict(model1, newdata=training)
        conf <- confusionMatrix(pred, training[,input$dependent])
        pred2 <- predict(model1, newdata=testing)
        conf2 <- confusionMatrix(pred2, testing[,input$dependent])
        
    } else{
        if(trainmethod=="cv"){
            ctrl <- trainControl(method = "cv", 
                                 number = input$nfold)
        } else if(trainmethod=="LOOCV"){
            ctrl <- trainControl(method = "LOOCV") 
        }
        
        if(prepvar==F){
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune))
        } else{
            if(input$algorithms=="nnet"){
                normalized <- function(x) (x-min(x))/(max(x)-min(x))
                training[,input$dependent] <- normalized(training[,input$dependent])
            }
            model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
        }
        
        pred <- predict(model1, newdata=training)
        conf <- paste("R^2 =", signif(cor(pred, training[,input$dependent])^2,3))
        pred2 <- predict(model1, newdata=testing)
        conf2 <- paste("R^2 =", signif(cor(pred2, testing[,input$dependent])^2,3))
    }
    return(list(fit=model1, conf=conf, conf2=conf2, pred=pred, pred2=pred2, training=training, testing=testing, predictors=predictors))
    
})


# model.fit <- reactive({
#     df <- filedata()
#     if(input$allvars == TRUE){
#         predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
#     } else {
#         predicto <- c(input$independents)
#         predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
#         if(!is.null(input$dependent)) {
#             index <- which(predicto %in% input$dependent)
#             if(any(index)==T){
#                 predicto <- predicto[-index]
#             } else { predicto <- predicto}
#         }
#         if(!is.null(input$removed)) {
#             index <- which(predicto %in% input$removed)
#             if(any(index)==T){
#                 predicto <- predicto[-index]
#             } else { predicto <- predicto}
#         }
#     }
#     
#     # conditions
#     if(is.null(input$resamplechk)) {
#         byID <- F
#     } else { byID <- input$resamplechk }
#     
#     if(is.null(input$subschk)) {
#         bySubset <- F
#     } else { bySubset <- input$subschk }
#     
#     if(is.null(input$subschktest)) {
#         testSubset <- F
#     } else { testSubset <- input$subschktest }
#     
#     if(is.null(input$prep)) {
#         prepvar <- T
#     } else { prepvar <- input$prep }
#     
#     dependents <- input$dependent
#     whichSubset <- input$subs
#     subsetLevel <- input$subsvar
#     subsetLevelTest <- input$subsvartest
#     identifier <- input$resampleid
#     
#     if(bySubset==T){
#         if(testSubset==T){
#             # training and testing different subsets
#             if(byID==T){
# #                 # repeated measures blocking
# #                 # do na.omit on training and testing subsets separately
# #                 data.tmp <- subset( df[,c(dependents, predicto, identifier, whichSubset)],  df[,whichSubset] %in% c(subsetLevel, subsetLevelTest) )
# #                 toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
# #                 if(is.na(toomanymissing[1])){
# #                     df <- droplevels(na.omit(data.tmp))
# #                 } else {
# #                     df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
# #                 }
# #                 remv <- which(names(df)==whichSubset)
# #                 training.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
# #                 testing.tmp <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
# #                 
# #                 # split the testing set by splitting at ID level, get the IDs for the test entities
# #                 trainID <- get_testing_ids(testing.tmp, identifier, dependents, input$pcttrain)
# #                 # get the testing set 
# #                 testing <- testing.tmp[which(testing.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
# #                 # get the training set by excluding any members that are in the testing set.
# #                 training <- training.tmp[-which(training.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
#                 
#             } else if(byID==F){
#                 # don't respect ID
#                 if(length(input$subs)>1){
#                     insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar, subsetLevelTest)}), 1, prod))
#                     data.tmp <- df[insubgroup, c(dependents, predicto, input$subs)] 
#                 } else {
#                     data.tmp <- subset( df[,c(dependents, predicto, input$subs)],  df[,input$subs] %in% c(input$subsvar, subsetLevelTest)  )
#                 }
#                 
#                 toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
#                 if(is.na(toomanymissing[1])){
#                     df <- droplevels(na.omit(data.tmp))
#                 } else {
#                     df <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                 }
#                 remv <- which(names(df) %in% whichSubset)
#                 
#                 
#                 if(length(input$subs)>1){
#                     insubgroup.train <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar)}), 1, prod))
#                     insubgroup.test <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% subsetLevelTest}), 1, prod))
#                     training <- df[insubgroup.train, -remv] 
#                     testing <- df[insubgroup.test, -remv]
#                 } else {
#                     training <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevel )))
#                     testing <- droplevels(na.omit( subset( df[,-remv],  df[,whichSubset] %in% subsetLevelTest )))
#                 }
#             }
#         } else if(testSubset==F){
#             # training and testing on same subset
#             if(byID==T){
# #                 # repeated measures blocking
# #                 # do na.omit on training and testing subsets separately
# #                 data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
# #                 # split the testing set by splitting at ID level, get the IDs for the test entities
# #                 trainID <- get_testing_ids(data.tmp, identifier, dependents, input$pcttrain)
# #                 # get the testing set 
# #                 testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
# #                 # get the training set by excluding any members that are in the testing set.
# #                 training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#             } else if(byID==F){
#                 # don't respect ID
#                 if(length(input$subs)>1){
#                     insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% input$subsvar}), 1, prod))
#                     data.tmp <- df[insubgroup, c(dependents, predicto)] 
#                 } else {
#                     data.tmp <- subset( df[,c(dependents, predicto)],  df[,input$subs] %in% input$subsvar  )
#                 }
#                 
#                 toomanymissing <- which(sapply(data.tmp, function(x) sum(is.na(x)))/nrow(data.tmp) > na.pct)
#                 if(is.na(toomanymissing[1])){
#                     data.tmp <- droplevels(na.omit(data.tmp))
#                 } else {
#                     data.tmp <- droplevels(na.omit(data.tmp[,-toomanymissing]))
#                 }
#                 
#                 inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#                 training <- data.tmp[ inTrain,]
#                 testing  <- data.tmp[-inTrain,]
#             }
#         }
#         
#     } else if(bySubset==F){
#         # training and testing on whole
#         if(byID==T){
# #             # repeated measures blocking
# #             # do na.omit on dataset
# #             data.tmp <- droplevels(na.omit( df[,c(dependents, predicto, identifier)] ))
# #             # split the testing set by splitting at ID level, get the IDs for the test entities
# #             trainID <- get_testing_ids(data.tmp, identifier, dependents, input$pcttrain)
# #             # get the testing set 
# #             testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
# #             # get the training set by excluding any members that are in the testing set.
# #             training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#         } else if(byID==F){
#             # don't respect ID
#             data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
#             inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#             training <- data.tmp[ inTrain,]
#             testing  <- data.tmp[-inTrain,]
#         }
#     }
#     
#     
#     
#     
#     fmla <- as.formula(paste(input$dependent," ~ ."))
#     
# #     data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
# #     inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
# #     training <- data.tmp[ inTrain,]
# #     testing  <- data.tmp[-inTrain,]
# #     ctrl <- trainControl(method = "cv", 
# #                          number = input$nfold, 
# #                          classProbs = T, summaryFunction = twoClassSummary)
#     
#     
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
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, metric = "ROC", trControl = ctrl, tuneLength=input$ntune))
#         } else{
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
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training,  trControl = ctrl, tuneLength=input$ntune))
#         } else{
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
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune))
#         } else{
#             if(input$algorithms=="nnet"){
#                 normalized <- function(x) (x-min(x))/(max(x)-min(x))
#                 training[,input$dependent] <- normalized(training[,input$dependent])
#             }
#             model1 <- do.call(train, list(form=fmla, method=input$algorithms,data=training, trControl = ctrl, tuneLength=input$ntune, preProc = c("center", "scale")))
#         }
#         
#         pred <- predict(model1, newdata=training)
#         conf <- paste("R^2 =", signif(cor(pred, training[,input$dependent])^2,3))
#         pred2 <- predict(model1, newdata=testing)
#         conf2 <- paste("R^2 =", signif(cor(pred2, testing[,input$dependent])^2,3))
#     }
#     
#     
#     return(list(fit=model1, conf=conf, conf2=conf2, pred=pred, pred2=pred2, training=training, testing=testing, predictors=predicto))
# })