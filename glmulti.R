# glmulti

output$popsize <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("GLMulti")){ 
        numericInput("popsize", "Population size", value=100, min=10, max=1000, step=50) 
    } else return(NULL)
})
output$mutrate <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("GLMulti")){ 
        numericInput("mutrate", "Mutation rate", value=.001, min=.0001, max= .1, step=.005) 
    } else return(NULL)
})
output$sexrate <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("GLMulti")){ 
        numericInput("sexrate", "Sex rate", value=.1, min=.01, max= .9, step=.1) 
    } else return(NULL)
})

output$imm <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("GLMulti")){ 
        numericInput("imm", "Immigration rate", value=.3, min=.1, max= .9, step=.1) 
    } else return(NULL)
})

glmulti.fit <- reactive({
    #if (is.null(input$action)) return()
    #if (input$action==0) return()
    
    #isolate({
    df <- filedata()
    if(is.null(df)) return()
    # isolate({
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
    
    #print(predicto)
    fmla <- as.formula(paste(input$dependent," ~ ."))
    training <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
    testing <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
    #fmla <- as.formula(paste(input$dependent," ~ . + (1|PatientID)"))
    #training <- droplevels(na.omit( df[,c(input$dependent, predicto, "PatientID")] ))
    #testing <- droplevels(na.omit( df[,c(input$dependent, predicto, "PatientID")] ))
    
    
    # conditions
#     byID <- input$resamplechk
#     bySubset <- input$subschk
#     testSubset <- input$subschktest
#     dependents <- input$dependent
#     whichSubset <- input$subs
#     subsetLevel <- input$subsvar
#     subsetLevelTest <- input$subsvartest
#     identifier <- input$resampleid
#     
#     get_testing_ids <- function(df){
#         uniqID.df <- df[ !duplicated(df[,identifier]),c(identifier,dependents) ]
#         temp <- createDataPartition(y = uniqID.df[,dependents], p = input$pcttrain/100, list = FALSE) 
#         inTrainID <- droplevels(uniqID.df[-temp,identifier])
#         inTrainID
#     }
#     
#     if(bySubset==T){
#         if(testSubset==T){
#             # training and testing different subsets
#             if(byID==T){
#                 # repeated measures blocking
#                 # do na.omit on training and testing subsets separately
#                 training.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
#                 testing.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevelTest )))
#                 # split the testing set by splitting at ID level, get the IDs for the test entities
#                 trainID <- get_testing_ids(testing.tmp)
#                 # get the testing set 
#                 testing <- testing.tmp[which(testing.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
#                 # get the training set by excluding any members that are in the testing set.
#                 training <- training.tmp[-which(training.tmp[,identifier] %in% trainID),][,-which(names(testing.tmp)==identifier)]
#                 
#             } else if(byID==F){
#                 # don't respect ID
#                 training <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
#                 testing <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevelTest )))
#             }
#             
#         } else if(testSubset==F){
#             # training and testing on same subset
#             if(byID==T){
#                 # repeated measures blocking
#                 # do na.omit on training and testing subsets separately
#                 data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto, identifier)],  df[,whichSubset] %in% subsetLevel )))
#                 # split the testing set by splitting at ID level, get the IDs for the test entities
#                 trainID <- get_testing_ids(data.tmp)
#                 # get the testing set 
#                 testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#                 # get the training set by excluding any members that are in the testing set.
#                 training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#             } else if(byID==F){
#                 # don't respect ID
#                 data.tmp <- droplevels(na.omit( subset( df[,c(dependents, predicto)],  df[,whichSubset] %in% subsetLevel )))
#                 inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#                 training <- data.tmp[ inTrain,]
#                 testing  <- data.tmp[-inTrain,]
#             }
#         }
#     } else if(bySubset==F){
#         # training and testing on whole
#         if(byID==T){
#             # repeated measures blocking
#             # do na.omit on dataset
#             data.tmp <- droplevels(na.omit( df[,c(dependents, predicto, identifier)] ))
#             # split the testing set by splitting at ID level, get the IDs for the test entities
#             trainID <- get_testing_ids(data.tmp)
#             # get the testing set 
#             testing <- data.tmp[which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#             # get the training set by excluding any members that are in the testing set.
#             training <- data.tmp[-which(data.tmp[,identifier] %in% trainID),][,-which(names(data.tmp)==identifier)]
#         } else if(byID==F){
#             # don't respect ID
#             data.tmp <- droplevels(na.omit( df[,c(dependents, predicto)] ))
#             inTrain <- createDataPartition(y = data.tmp[,dependents], p = input$pcttrain/100, list = FALSE)  
#             training <- data.tmp[ inTrain,]
#             testing  <- data.tmp[-inTrain,]
#         }
#     }
    
    # manual scaling
    if(input$prep==T){
        nums <- sapply(training, is.numeric)
        training[,nums] <- scale(training[,nums])
    }
    
    if(class(df[,input$dependent]) %in% c("factor","character") & length(levels(df[,input$dependent]))==2){
        #model1 <- glmulti(fmla, data=training, level=2, method="g", crit="bic", family=binomial(link="logit") , plotty=F)
        #model <- glm(fmla, data=training, family=binomial(link="logit"))
        #model1 <- glmulti(model, level=1, crit="aic")
        model1 <- do.call(glmulti, list(y=fmla, data=training, level=1, method="g", crit="bic", plotty=F, family=binomial(link="logit"),
                                        popsize= input$popsize , mutrate= input$mutrate ,sexrate= input$sexrate , imm= input$imm))
        #model1 <- do.call(glmulti, list(y=fmla, data=training, level=1, method="g", crit="bic", plotty=F, family=binomial(link="logit"), fitfunction="glmer"))
    } else{
        model1 <- do.call(glmulti, list(y=fmla, data=training, level=1, method="g", crit="bic", plotty=F, fitfunction="lm",
                                        popsize= input$popsize , mutrate= input$mutrate ,sexrate= input$sexrate , imm= input$imm))
        #model1 <- do.call(glmulti, list(y=fmla, data=training, level=1, method="g", crit="bic", plotty=F, fitfunction="lmer"))
        #model1 <- do.call(lm, list(fmla, data=training ))
    }

   #print(summary(model1))
    return(model1)
})

output$glmultibarplot <- renderPlot({
    model_local <- glmulti.fit()
    if(is.null(model_local)) return(NULL)
    #plot(model$fit, type = "p")
    #plot(model$fit, type = "s")
    #plot(model, type = "s")
    #glmplot <<- recordPlot()
    #replayPlot(glmplot) # redraw
    ww = exp(-(model_local@crits - model_local@crits[1])/2)
    ww = ww/sum(ww)
    clartou = function(x) {
        pieces <- sort(strsplit(x, ":")[[1]])
        if (length(pieces) > 1) 
            paste(pieces[1], ":", pieces[2], sep = "")
        else x
    }
    tet = lapply(model_local@formulas, function(x) sapply(attr(delete.response(terms(x)), "term.labels"), clartou))
    allt <- unique(unlist(tet))
    imp <- sapply(allt, function(x) sum(ww[sapply(tet, function(t) x %in% t)]))
#     barplot(sort(imp), xlab = "", xlim = c(0, 1), ylab = "", 
#             horiz = T, las = 2, names.arg = allt[order(imp)], 
#             main = "Model-averaged importance of terms", ...)
#     abline(v = 0.8, col = "red")
    
    plotdf <- data.frame(preds=names(imp), importance=imp)
    gg <- ggplot(plotdf, aes(x=reorder(preds,importance), y=importance)) + geom_bar(stat = "identity") + coord_flip() 
    gg + scale_fill_gradient() + theme_bw()
})



output$contentsglmulti <- renderPrint({
    #if (is.null(input$action)) {print("You must press Learn to train the model."); return()}
    #if (input$action==0) {print("You must press Learn to train the model."); return()}
    model_local <- glmulti.fit()
    if(is.null(model_local)) return(NULL)
    #print("Accuracy on training set")
    #print(model$conf)
    #print("Accuracy on testing set")
    #print(model$conf2)
    #print(model$fit$bestmodel)
    #print(model$fit)
    print(summary(model_local))
    print(model_local)
#     ww = exp(-(model@crits - model@crits[1])/2)
#     ww = ww/sum(ww)
#     clartou = function(x) {
#         pieces <- sort(strsplit(x, ":")[[1]])
#         if (length(pieces) > 1) 
#             paste(pieces[1], ":", pieces[2], sep = "")
#         else x
#     }
#     tet = lapply(model@formulas, function(x) sapply(attr(delete.response(terms(x)), "term.labels"), clartou))
#     allt <- unique(unlist(tet))
#     imp <- sapply(allt, function(x) sum(ww[sapply(tet, function(t) x %in% t)]))
#     imp
})

# model1@objects
# 
# ww = exp(-(model1@crits - model1@crits[1])/2)
# ww = ww/sum(ww)
# clartou = function(x) {
#     pieces <- sort(strsplit(x, ":")[[1]])
#     if (length(pieces) > 1) 
#         paste(pieces[1], ":", pieces[2], sep = "")
#     else x
# }
# tet = lapply(model1@formulas, function(x) sapply(attr(delete.response(terms(x)), 
#                                                  "term.labels"), clartou))
# allt <- unique(unlist(tet))
# imp <- sapply(allt, function(x) sum(ww[sapply(tet, function(t) x %in% 
#                                                   t)]))
# par(oma = c(0, 3, 0, 0))
# barplot(sort(imp), xlab = "", xlim = c(0, 1), ylab = "", 
#         horiz = T, las = 2, names.arg = allt[order(imp)], 
#         main = "Model-averaged importance of terms", ...)
# abline(v = 0.8, col = "red")
# 
# ggplot(summary.mtc, aes(x = factor(gear), y = meanwt)) + geom_bar(stat = "identity")