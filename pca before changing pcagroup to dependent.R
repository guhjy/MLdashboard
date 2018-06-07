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
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
        if(input$pcagroup %in% predicto) predicto <- predicto[-which(predicto==input$pcagroup)]
    }

    print(predicto)
    
    if(input$subschk==F) {
        dat <- droplevels( na.omit( df[,c(input$dependent, predicto, input$pcagroup)] ) )
    } else {
        dat <- droplevels( na.omit( subset(
            df[,c(input$dependent, predicto, input$pcagroup)],  df[,input$subs] %in% input$subsvar 
        )))
    }
    nums <- sapply(dat, is.numeric)
    out <- list(pca=prcomp(dat[,nums], center = TRUE, scale. = TRUE), dat=dat[,nums], wfactor=dat)
    return(out)
    # })
})
output$pca <- renderPlot({
    pca <- getpca()
    #library(devtools)
    #install_github("ggbiplot", "vqv")
    g <- ggbiplot(pca$pca, obs.scale = 1, var.scale = 1, varname.size=4, groups = as.factor(pca$wfactor[,input$pcagroup]), ellipse = TRUE, circle = F, alpha=.6, var.axes=T)
    g <- g + scale_color_discrete(name = '') + theme_bw()
    g <- g +  ggtitle(input$pcagroup) 
    biplots <<- g
    grid.arrange(g)
})
output$pca2 <- renderPlot({
    pca <- getpca()
    f1 <<- fviz_screeplot(pca$pca, ncp=ncol(pca$dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
    f2 <<- fviz_contrib(pca$pca, choice="var", axes = 1 ,  sort.val="none") + coord_flip() 
    f3 <<- fviz_contrib(pca$pca, choice="var", axes = 2 ,  sort.val="none") + coord_flip() 
    grid.arrange(f1,f2,f3, ncol = 2, layout_matrix = rbind(c(1,1),c(2,3)))
})
