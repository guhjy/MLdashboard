# All the controls common accross tabs

anals <- c("...", "PCA", "Clustering", "Modeling", "Bayesian Network", "GLMulti")

# control which sliders appear for which tabs
analysis <- reactive({
    if(is.null(input$panelID)) return("...")
    if(input$panelID %in% c("pcabiplot")) { return("PCA1")}
    if(input$panelID %in% c( "pcastats")) { return("PCA2")}
    if(input$panelID %in% c("bicluster")) { return("biClustering")}
    if(input$panelID %in% c("cocluster", "predcluster")) { return("coClustering")}
    if(input$panelID %in% c("predcircle")) { return("circleClustering")}
    if(input$panelID %in% c("network")) { return("predNetwork")}
    if(input$panelID %in% c("modelfit", "modelpredict", "modelvip", "modelplot","modelsummary", "modeldetails", "cart")) { return("Modeling")}
    if(input$panelID %in% c("bayesiannetwork")) { return("Bayesian Network")}
    if(input$panelID %in% c("glmultitab1","glmultitab2")) { return("GLMulti")}
})

# subset data
output$subschk  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("subschk", "Take subgroup of samples", value=subschk.init, width='100%')
})
output$subs  <- renderUI({
    if(is.null(input$subschk)) return()
    if(input$subschk==F) return()
    df <- filedata()
    #if (is.null(df)) return(NULL)
    items=names(df)
    #names(items)=items
    selectInput("subs", "Choose subgroup variable", items, multiple=T, selected=subs.init, selectize=T)
})
output$subsvar  <- renderUI({
    if(is.null(input$subschk)) return()
    if(input$subschk==F) return()
    df <- filedata()
    
   
    #if (is.null(df) | is.null(input$subs)) return(NULL)
    #if (is.null(df) | input$subs=="") return(NULL)
    #if (input$subs==F) return()
    #selectInput("subsvar","Choose levels", levels(df[,input$subs]), multiple=T, selected=subsvar.init, selectize=T)
    if(length(input$subs)>1){
        items <- unlist(sapply(df[,input$subs],levels))
        names(items) <- items
    } else {
        items <- levels(df[,input$subs])
    }
   
    
    selectInput("subsvar","Choose levels", items, multiple=T, selected=subsvar.init, selectize=T)
})



# subset data
output$randomeffects  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    textInput(inputId="randomeffects", label="Random effects", value = randomeffects.init, width = NULL, placeholder = "DATASET/sTEAMS")
    #checkboxInput("subschk", "Take subgroup of samples", value=input$subschk, width='100%')
})



# subset data
output$subschkGLMM  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("subschkGLMM", "Take subgroup of samples", value=subschkGLMM.init, width='100%')
})
output$subsGLMM  <- renderUI({
    if(is.null(input$subschkGLMM)) return()
    if(input$subschkGLMM==F) return()
    df <- filedata()
    #if (is.null(df)) return(NULL)
    items=names(df)
    #names(items)=items
    selectInput("subsGLMM", "Choose subgroup variable", items, multiple=T, selected=subsGLMM.init, selectize=T)
})
output$subsvarGLMM  <- renderUI({
    if(is.null(input$subschkGLMM)) return()
    if(input$subschkGLMM==F) return()
    df <- filedata()
    if(length(input$subsGLMM)>1){
        items <- unlist(sapply(df[,input$subsGLMM],levels))
        names(items) <- items
    } else {
        items <- levels(df[,input$subsGLMM])
    }
    selectInput("subsvarGLMM","Choose levels", items, multiple=T, selected=subsvarGLMM.init, selectize=T)
})







# choose predictors
output$allvars <- output$allvars2 <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("allvars", "Use all variables as predictors", value=allvars.init, width='100%')
    
})

# output$predgroups <- renderUI({
#     df <- filedata()
#     if (is.null(df) | is.null(input$allvars) | input$allvars==T) return(NULL)
#     items <- c("Dobutamine"="a",
#                "Demographics"="b",
#                "Condition"="c",
#                "History"="d",
#                "Additional"="e"
#                #"T Cell FACS"="f",
#                #"B Cell FACS"="g"
#                )
#     selectInput("predgroups", "Include subgroup of predictors:", items, multiple=T, selected=c("a","b","c","d","e"))
# })

output$predgroups <- renderUI({
    #if (is.null(df) | is.null(input$allvars)) return(NULL)
    if (is.null(input$allvars)) return(NULL)
    if(input$allvars==T) return(NULL)
    #df <- filedata()
    items <- uniquegroupnames
    selectInput("predgroups", "Include subgroup of predictors:", items, multiple=T, selected=predgroups.init)
})


# output$eigvars <- output$eigvars2 <- renderUI({
#     df <- filedata()
#     if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
#     if(input$allvars==T ) return(NULL)
#     checkboxInput("eigvars", "include all eigengenes", value=F, width='100%')
# })
# output$phenovars <- output$phenovars2 <- renderUI({
#     df <- filedata()
#     if (is.null(df) | is.null(input$allvars)) return(NULL)
#     if(input$allvars==T) return(NULL)
#     checkboxInput("phenovars", "include all clinical phenotypes", value=T, width='100%')
# })
# output$demovars <- renderUI({
#     df <- filedata()
#     if (is.null(df) | is.null(input$allvars)) return(NULL)
#     if(input$allvars==T) return(NULL)
#     checkboxInput("demovars", "include all demographics", value=T, width='100%')
# })
# 
# output$t1vars <- renderUI({
#     df <- filedata()
#     if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
#     if(input$allvars==T ) return(NULL)
#     checkboxInput("t1vars", "include all innate immune", value=F, width='100%')
# })
# output$t3vars <- renderUI({
#     df <- filedata()
#     if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
#     if(input$allvars==T ) return(NULL)
#     checkboxInput("t3vars", "include all T cells", value=F, width='100%')
# })
# output$bcells <- renderUI({
#     df <- filedata()
#     if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
#     if(input$allvars==T ) return(NULL)
#     checkboxInput("bcells", "include all B Cells", value=F, width='100%')
# })
# output$cytokine <- renderUI({
#     df <- filedata()
#     if (is.null(df) | (is.null(input$allvars)) ) return(NULL)
#     if(input$allvars==T ) return(NULL)
#     checkboxInput("cytokine", "include all cytokines", value=F, width='100%')
# })


output$independents  <- renderUI({
    if (is.null(df) | is.null(input$allvars)) return(NULL)
    if (input$allvars==T) return(NULL)
    df <- filedata()
    items=names(df)
    names(items)=items
    selectInput("independents","Add predictors:",items,multiple=TRUE, selected=independents.init, selectize=T)
})
output$removed <- renderUI({
    df <- filedata()
    #if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("removed","Remove predictors:",items,multiple=TRUE, selected=removed.init, selectize=T)
    
})