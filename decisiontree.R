# decision tree

output$decisiontree <- renderPlot({
    df <- filedata()
    if(is.null(df)) return()
    
    if(input$allvars == TRUE){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
    } else {
        predicto <- c(input$independents)
        
        predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
        #if(input$predgroups == "a"){ predicto <- c(predicto, predgroupnames[["a"]]) }
        
        #         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        #         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        #         if(input$demovars == T) {predicto <- c(predicto, demovars)}
        #         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        #         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        #         if(input$bcells == T) {predicto <- c(predicto, bcells)}
        #         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
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
    
    
    
    
    
    ctrl <- trainControl(method = "cv", 
                         number = input$nfold)
    tree2 <- train(death~., data=training, method="rpart", trControl = ctrl, tuneLength=input$ntune)
    prp(tree2$finalModel)
    
})