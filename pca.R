# PCA






# output$pcagroup <- renderUI({
#     df <- filedata()
#     #if (is.null(df)) return(NULL)
#     if (analysis() %in% c("PCA")){
#         items=names(df)
#         names(items)=items
#         selectInput("pcagroup","Select grouping for PCA:", items, selected="Survivor", selectize=T)
#     } else return(NULL)
# })

output$which2pcs <- renderUI({
    #if (is.null(df) | is.null(input$allvars)) return(NULL)
    if(analysis()!="PCA1") return(NULL)
    if (is.null(input$allvars)) return(NULL)
    df <- filedata()
    items <- 1:ncol(df)
    selectInput("which2pcs", "Show which principal components:", items, multiple=T, selected=which2pcs.init)
})


output$whichpcs <- renderUI({
    #if (is.null(df) | is.null(input$allvars)) return(NULL)
    if(analysis()!="PCA2") return(NULL)
    if (is.null(input$allvars)) return(NULL)
    df <- filedata()
    items <- 1:ncol(df)
    selectInput("whichpcs", "Show which principal components:", items, multiple=T, selected=whichpcs.init)
})


getpca <- reactive({
    df <- filedata()
    # isolate({
    if(is.null(input$allvars)) return()
    if(is.null(input$subsvar) & input$subschk==T) return()
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent))]
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent, input$removed))]
    } else if(input$allvars == FALSE){
        predicto <- c(input$independents)
        predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
#         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
#         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
#         if(input$demovars == T) {predicto <- c(predicto, demovars)}
#         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
#         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
#         if(input$bcells == T) {predicto <- c(predicto, bcells)}
#         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        if(!is.null(input$removed)) {
            index <- which(predicto %in% input$removed)
            if(any(index)==T){
                predicto <- predicto[-index]
            } else { predicto <- predicto}
        }
        if(input$dependent %in% predicto) predicto <- predicto[-which(predicto==input$dependent)]
    }

    #print(predicto)
    
    if(input$subschk==F) {
        #dat <- droplevels( na.omit( df[,c(input$dependent, predicto)] ) )
        dat <-  df[,c(input$dependent, predicto)] 
        toomanymissing <- which(sapply(dat, function(x) sum(is.na(x)))/nrow(dat) > na.pct)
        if(is.na(toomanymissing[1])){
            dat <- droplevels(na.omit(dat))
        } else {
            dat <- droplevels(na.omit(dat[,-toomanymissing]))
        }
       
    } else {
            #dat <- droplevels( na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )))
        # original dat <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )
        
        #dat <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )
        #insubgroup <- as.logical(apply(sapply(csv[,c("DATASET","Self_Win")], function(x) { x %in% subsvar[-1]}), 1, prod))
        if(length(input$subs)>1){
            insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% input$subsvar}), 1, prod))
            dat <- df[insubgroup, c(input$dependent, predicto)] 
        } else {
            insubgroup <- df[,input$subs] %in% input$subsvar
            dat <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )
        }
        
        
        toomanymissing <- which(sapply(dat, function(x) sum(is.na(x)))/nrow(dat) > na.pct)
        if(is.na(toomanymissing[1])){
            dat <- droplevels(na.omit(dat))
        } else {
            dat <- droplevels(na.omit(dat[,-toomanymissing]))
        }
        #dat <- droplevels(na.omit(dat[,-toomanymissing]))
    }
    nums <- sapply(dat, is.numeric)
    #print(nums)
    #print(dat)
    pcadat <- dat[,nums]
    


    if(input$dependent %in% names(pcadat) & !(input$dependent %in% unlist(predgroupnames[input$predgroups]))){
        pcadat <- pcadat[,-which(names(pcadat)==input$dependent)]
    }
    out <- list(pca=prcomp(pcadat, center = TRUE, scale. = TRUE), dat=dat[,nums], wfactor=dat)
    return(out)
    # })
})
output$pca <- renderPlot({
    #if(is.null(input$dependent)) return()
    #if(is.null(input$subschk)) return()
    if(is.null(input$allvars)) return()

    #if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$dependent)) return()
    if(input$allvars==F & is.null(input$predgroups)) return()
#     validate(
#         need(input$dependent, 'dependent'),
#         need(input$subschk, 'subschke.'),
#         need(input$allvars, 'allvars.'),
#         need(input$prep, 'prep.')
#     )
    
    input$pcaButton
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    isolate({
        pca <- getpca()
        if(is.null(pca)) stop("Press RUN to initiate computation.")
        #library(devtools)
        #install_github("ggbiplot", "vqv")
        if(is.numeric(pca$wfactor[,input$dependent])) { grps <- cut(pca$wfactor[,input$dependent],4)} else {
            grps <- pca$wfactor[,input$dependent]
        }
        #print(input$whichpcs)
        g <- ggbiplot(pca$pca, choices=as.numeric(input$which2pcs[c(1,2)]), obs.scale = 1, var.scale = 1, varname.size=4, groups = grps, ellipse = TRUE, circle = F, alpha=alpha.init, var.axes=T)
        g <- g + scale_color_discrete(name = '') + theme_bw()
        g <- g +  ggtitle(input$dependent) 
        biplots <<- g
        grid.arrange(g)
    })
    
}, height = screenheight)

npred_pca <- function(){
    pca <- getpca()
    #screenheight * (.5 + ncol(pca$dat)/300)
    screenheight
}


# output$pca3 <- renderPlot({
#     #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
#     input$pcaButton
#     isolate({
#         pca <- getpca()
#         npred <- ncol(pca$dat)
#         #a=101
#         f1 <<- fviz_screeplot(pca$pca, ncp=ncol(pca$dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
#         f2 <<- fviz_contrib(pca$pca, choice="var", axes = 1 ,  sort.val="none") + coord_flip() 
#         f3 <<- fviz_contrib(pca$pca, choice="var", axes = 2 ,  sort.val="none") + coord_flip() 
#         grid.arrange(f1,f2,f3, ncol = 2, heights=c(1,2*ncol(pca$dat)/80), layout_matrix = rbind(c(1,1),c(2,3)))
#     })
# 
# },  height = npred_pca)

output$pca2 <- renderPlot({
    #if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    if(is.null(input$whichpcs)) return()
    input$pcaButton
    isolate({
        pca <- getpca()
        f1 <- fviz_screeplot(pca$pca, ncp=ncol(pca$dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
        #f1 <<- fviz_screeplot(pca$pca, ncp=ncol(pca$dat), addlabels=TRUE, hjust = -0.3) + theme_minimal()
#         f1 <<- fviz_contrib(pca$pca, choice="var", axes = 1 ,  sort.val="asc", top=30) + coord_flip() + theme_minimal() + labs(title = "")
#         f2 <<- fviz_contrib(pca$pca, choice="var", axes = 2 ,  sort.val="asc", top=30) + coord_flip() + theme_minimal() + labs(title = "")
#         f3 <<- fviz_contrib(pca$pca, choice="var", axes = 3 ,  sort.val="asc", top=30) + coord_flip() + theme_minimal() + labs(title = "")
#         f4 <<- fviz_contrib(pca$pca, choice="var", axes = 4 ,  sort.val="asc", top=30) + coord_flip() + theme_minimal() + labs(title = "")
        plots <- list()
        for(i in 1:length(input$whichpcs)){
            plots[[i]] <- fviz_contrib(pca$pca, choice="var", axes = i ,  sort.val="asc", top=20) + coord_flip() + theme_minimal() + labs(title = input$whichpcs[i])
        }
        plots[[length(input$whichpcs)+1]] <- f1
        
        #grid.arrange(f1,f2,f3,f4, ncol = 4)
        #do.call(grid.arrange, c(plots, list=(ncol=length(plots))))
        #recursive=TRUE
        do.call(grid.arrange, c(plots, list(ncol=length(plots), layout_matrix = rbind(c(1:length(input$whichpcs)),rep(length(plots),length(input$whichpcs) )))))  # Gives an error
    })
    
},  height = npred_pca)



