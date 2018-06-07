# clustering

# Clustering section
# output$labcol <- renderUI({
#     df <- filedata()
#     if (is.null(df)) return(NULL)
#     if (analysis() %in% c("Clustering")){
#         items=names(df)
#         names(items)=items
#         selectInput("labcol","Label samples by:", items,multiple=TRUE, selected=input$labcol, selectize=T)
#     } else return(NULL)
# })


npred_clust <- function(){
    preds <- get_preds()
    #screenheight * (.5 + ncol(pca$dat)/300)
    if(ncol(preds)<20){ 600 } else{200+ ncol(preds)*15}
}

output$corthresh <- renderUI({
    #df <- filedata()
    #if (is.null(df) ) return(NULL)
    if (analysis() %in% c("predNetwork")){
        #if (input$details==F) return()
        if(is.null(input$corthresh)) {
            val <- .3
            } else { val <- input$corthresh}
        sliderInput("corthresh", "Set link threshold:", min = 0, max = 1, value=val, step = .05, width = "100%")
    } else return(NULL)
})

output$aggbyvars <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("biClustering")){
        items=names(df)
        names(items)=items
#         if(is.null(input$aggbyvars)){
#             sel=aggbyvars.init
#         } else {
#             sel=input$aggbyvars
#         }
        selectInput("aggbyvars","Aggregate samples by:", items, multiple=TRUE, selected=aggbyvars.init, selectize=T)
    } else return(NULL)
})


output$cortype <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("coClustering", "circleClustering", "predNetwork")){
        items=c("Pearson", "Spearman", "Kendall", "MIC (Maximal Information Coefficient)", "MAS (Maximum Asymmetry Score)", "MEV (Maximum Edge Value)", "MCN (Minimum Cell Number)", "MIC-R2")
        selectInput("cortype","Choose association measure:", items, multiple=F, selected=input$cortype, selectize=T)
    } else return(NULL)
})

output$netlayout <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("predNetwork")){
        items=c("Fruchterman-Reingold"="layout.fruchterman.reingold", 
                "Multidimensional Scaling"="layout.mds", 
                "Circle"="layout.circle", 
                "Sphere"="layout.sphere", 
                "Reingold-Tilford"="layout.reingold.tilford", 
                "Kamada-Kawai"="layout.kamada.kawai")
        selectInput("netlayout","Choose network layout:", items, multiple=F, selected=input$netlayout, selectize=T)
    } else return(NULL)
})




get_mine <- reactive({
    dat <- get_preds()
    nums <- sapply(dat, is.numeric)
    m2 <- mine(data.matrix(na.omit(dat[,nums])))
    m2
})



output$unsigned  <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("biClustering", "coClustering", "circleClustering")){
        #items=c("Unsigned", "Signed")
        #selectInput("unsigned","Use absolute values:", items, multiple=F, selected=input$unsigned, selectize=T)
        checkboxInput("unsigned", "Use unsigned distance", value=input$unsigned, width='100%')
    } else return(NULL)
})

output$distfun <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("biClustering", "coClustering", "circleClustering")){
        items=c( "L1", "L2","Lp", "Pearson", "maximum", "canberra", "binary")
        selectInput("distfun","Choose distance metric:", items, multiple=F, selected=input$distfun, selectize=T)
    } else return(NULL)
})

output$p <- renderUI({
    if (is.null(input$distfun)) return(NULL)
    if (analysis() %in% c("biClustering", "coClustering", "circleClustering") & input$distfun=="Lp"){
        sliderInput("p","Choose Minkowski power:", value=input$p, min = 3, max = 500, step = 1, width = "100%")
    } else return(NULL)
})


output$dendheight <- renderUI({
    if (is.null(input$distfun)) return(NULL)
    if (analysis() %in% c("circleClustering")){
        sliderInput("dendheight","Adjust room for dendro:", value=.5, min = 0, max = 1, step = .05, width = "100%")
    } else return(NULL)
})

output$labelheight <- renderUI({
    if (is.null(input$distfun)) return(NULL)
    if (analysis() %in% c("circleClustering")){
        sliderInput("labelheight","Adjust room for labels:", value=.1, min = 0, max = 1, step = .05, width = "100%")
    } else return(NULL)
})

output$hfun <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("biClustering", "coClustering", "circleClustering")){
        items=c("Complete","Average","Mcquitty","Median","Centroid", "Ward", "Single")
        selectInput("hfun","Choose agglomeration method:", items, multiple=F, selected=input$hfun, selectize=T)
    } else return(NULL)
})


get_preds <- reactive({
    df <- filedata()
    # isolate({
    if(is.null(input$allvars)) return()
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
    dat
})




output$cluster <- renderPlot({
    #print("Let's cluster")
    if(is.null(input$allvars)) return()
    #if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$dependent)) return()
    if(input$allvars==F & is.null(input$predgroups)) return()
    #if(is.null(input$cortype)) return()
    if(is.null(input$distfun)) return()
    if(is.null(input$hfun)) return()
    if(is.null(input$aggbyvars)) return()
    input$unsigned
    #print("got through is.null's")
    
    input$pcaButton
    #if (is.null(input$action)) return()
#     df <- filedata()
#     
#     if(input$allvars == TRUE & is.null(input$removed)){
#         predicto <- names(df)
#     } else if(input$allvars == TRUE & !is.null(input$removed)){
#         predicto <- names(df)[-which(names(df) %in% c(input$removed))]
#         predicto <- c(predicto)
#     } else if(input$allvars == FALSE){
#         predicto <- c(input$independents)
#         predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
# #         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
# #         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
# #         if(input$demovars == T) {predicto <- c(predicto, demovars)}
# #         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
# #         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
# #         if(input$bcells == T) {predicto <- c(predicto, bcells)}
# #         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
#         if(!is.null(input$removed)) {
#             index <- which(predicto %in% input$removed)
#             if(any(index)==T){
#                 predicto <- predicto[-index]
#             } else { predicto <- predicto}
#         }
#     }
#     #print(which(predicto %in% input$removed))
#     print(predicto)
#     
#     if(input$subschk==F) {
#         dat <-  droplevels(df[,c(input$labcol, predicto)] )
#         
#     } else {
#         dat <- droplevels(subset(
#             df[,c(input$labcol, predicto)],  df[,input$subs] %in% input$subsvar ))
#     }
#     
    isolate({
        df <- filedata()
        #print(df)
        
        # isolate({
        if(is.null(input$allvars)) stop("allvars")
        if(input$allvars == TRUE & is.null(input$removed)){
            predicto <- names(df)
        } else if(input$allvars == TRUE & !is.null(input$removed)){
            predicto <- names(df)[-which(names(df) %in% c(input$removed))]
        } else if(input$allvars == FALSE){
            predicto <- c(input$independents, input$aggbyvars)
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
        }
        
        #print(predicto)
        #print("subschk")
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
        #print(dat)
        
        #dat <- get_preds()
        nums <- sapply(dat, is.numeric)
        #print(nums)
        M <- dat[,nums]
        #N <- data.matrix(M)
        #print(M)
        # test area    #
        
        nums <- sapply(dat, is.numeric)
        aggBy <- dat[, input$aggbyvars]
        #aggBy <- csv[,"DATASET"]
        
        if(length(input$aggbyvars)==1){
            #print("length 1, trying to cut")
            if(!is.factor(aggBy)) aggBy <- cut(aggBy, 4)
            #print(aggBy)
        } else {
            isfactor <- sapply(aggBy, is.factor)
            #print(isfactor)
            if(sum(!isfactor)>1){
                aggBy[,!isfactor] <- apply(aggBy[,!isfactor], 2, function(x) cut(x, 4))
            } else if(sum(!isfactor)==1){
                aggBy[,!isfactor] <- cut(aggBy[,!isfactor], 4)
            }
            
        }
        
        aggvar <- as.list(as.data.frame(aggBy))
        N <- aggregate(M, by=aggvar, FUN=mean, simplify=T)
        if(length(aggvar)==1){
            rownames(N) <- N[,1]
            N <- N[,-1]
        } else{
            rownames(N) <- apply(N[,c(1:length(aggvar))], 1, paste, collapse=", ")
            N <- N[,-c(1:length(aggvar))]
        }
        
        
        #####
        
        
        
       # print(dim(N))
        
        #     rname.tmp <- dat[,input$labcol]
        #     if(length(input$labcol)>=2) {
        #         rownames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
        #     } else {rownames(N) <- rname.tmp}
        
        #rownames(N) <- paste(dat[,input$labcol], collapse=",")
        
        distfunc1 <- get_distfunc()
        hcluster <- get_hclust()
        
        my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
#         if(input$hfun=="Single") hcluster <- function(x) hclust(x, method="single")
#         if(input$hfun=="Ward") hcluster <- function(x) hclust(x, method="ward")
#         if(input$hfun=="Complete") hcluster <- function(x) hclust(x, method="complete")
#         if(input$hfun=="Average") hcluster <- function(x) hclust(x, method="average")
#         if(input$hfun=="Mcquitty") hcluster <- function(x) hclust(x, method="mcquitty")
#         if(input$hfun=="Median") hcluster <- function(x) hclust(x, method="median")
#         if(input$hfun=="Centroid") hcluster <- function(x) hclust(x, method="centroid")
        #if(input$distfun=="Unsigned L1") distfunc1 <- function(x) as.dist(1-abs(x))
        #if(input$distfun=="Unsigned L2") distfunc1 <- function(x) as.dist(sqrt(1-x^2))
#         if(input$unsigned==T){
#             if(input$distfun=="L1") distfunc1 <- function(x) dist(abs(x), method="manhattan")
#             if(input$distfun=="L2") distfunc1 <- function(x) dist(abs(x))
#             if(input$distfun=="maximum") distfunc1 <- function(x) dist(abs(x), method="maximum")
#             if(input$distfun=="canberra") distfunc1 <- function(x) dist(abs(x), method="canberra")
#             if(input$distfun=="binary") distfunc1 <- function(x) dist(abs(x), method="binary")
#         } else{
#             if(input$distfun=="L1") distfunc1 <- function(x) dist(x, method="manhattan")
#             if(input$distfun=="L2") distfunc1 <- function(x) dist(x)
#             if(input$distfun=="maximum") distfunc1 <- function(x) dist(x, method="maximum")
#             if(input$distfun=="canberra") distfunc1 <- function(x) dist(x, method="canberra")
#             if(input$distfun=="binary") distfunc1 <- function(x) dist(x, method="binary")
#         }

        if(input$distfun=="Pearson") stop("Pearson metric can't be used in biclustering.")
        #print(N)
        
# test area      
#         M <- na.omit(csv[,c("sF","sMIN","DATASET")])
#         N <- aggregate(M, by=list(M$DATASET), FUN=mean)
#         d <- dist(N, method="euclidean")
#         tree <- hclust(d)
#         cut <- cutree(tree, k=3)
#         N$cluster <- cut
# test area end
        
        heatmap.2(t(scale(N)),
                  hclustfun = hcluster,
                  distfun = distfunc1,
                  #distfun <- function(x) daisy(x,metric="euclidean"),
                  #distfun = function(x) as.dist(1 - cor(x, use="pairwise.complete.obs")),
                  
                  #cellnote = mat_data,  # same data set for cell labels
                  #main = "Clustered Correlation Matrix", # heat map title
                  key=T, key.title = "color key", key.xlab = "within-row z-score",
                  #keysize=.8, 
                  #symkey=T, 
                  #symm=F, 
                  #symbreaks=T, 
                  #denscol="grey",
                  #notecol="black",      # change font color of cell labels to black
                  #density.info="none",  # turns off density plot inside color legend
                  trace="none",         # turns off trace lines inside the heat map
                  margins =c(12,8),     # widens margins around plot
                  col=my_palette,# use on color palette defined earlier
                  cexCol=.7,
                  scale="none",
                  labCol =  rownames(N)
                  
                  #breaks=col_breaks,    # enable color transition at specified limits
                  #dendrogram="both",     # only draw a row dendrogram
                  #Colv=T, Rowv=T
        )            # turn off column clustering
        
        rmatrix.plot <<- recordPlot()
        replayPlot(rmatrix.plot) # redraw
    })
    
}, height = npred_clust)


get_distfunc <- reactive({
    if(input$unsigned==T){
        if(input$distfun=="L1") distfunc1 <- function(x) dist(abs(x), method="manhattan")
        if(input$distfun=="L2") distfunc1 <- function(x) dist(abs(x))
        if(input$distfun=="Lp") distfunc1 <- function(x) dist(abs(x), method="minkowski", p=as.numeric(input$p))
        #if(input$distfun=="L4") distfunc1 <- function(x) dist(abs(x), method="minkowski", p=4)
        if(input$distfun=="Pearson") distfunc1 <- function(x) as.dist(1-abs(x))
        if(input$distfun=="maximum") distfunc1 <- function(x) dist(abs(x), method="maximum")
        if(input$distfun=="canberra") distfunc1 <- function(x) dist(abs(x), method="canberra")
        if(input$distfun=="binary") distfunc1 <- function(x) dist(abs(x), method="binary")
    } else{
        if(input$distfun=="L1") distfunc1 <- function(x) dist(x, method="manhattan")
        if(input$distfun=="L2") distfunc1 <- function(x) dist(x)
        if(input$distfun=="Lp") distfunc1 <- function(x) dist(x, method="minkowski", p=as.numeric(input$p))
        #if(input$distfun=="L4") distfunc1 <- function(x) dist(x, method="minkowski", p=4)
        if(input$distfun=="Pearson") distfunc1 <- function(x) as.dist(1-x)
        if(input$distfun=="maximum") distfunc1 <- function(x) dist(x, method="maximum")
        if(input$distfun=="canberra") distfunc1 <- function(x) dist(x, method="canberra")
        if(input$distfun=="binary") distfunc1 <- function(x) dist(x, method="binary")
    }
    distfunc1
})


get_hclust <- reactive({
    if(input$hfun=="Ward") hcluster <- function(x) hclust(x, method="ward")
    if(input$hfun=="Single") hcluster <- function(x) hclust(x, method="single")
    if(input$hfun=="Complete") hcluster <- function(x) hclust(x, method="complete")
    if(input$hfun=="Average") hcluster <- function(x) hclust(x, method="average")
    if(input$hfun=="Mcquitty") hcluster <- function(x) hclust(x, method="mcquitty")
    if(input$hfun=="Median") hcluster <- function(x) hclust(x, method="median")
    if(input$hfun=="Centroid") hcluster <- function(x) hclust(x, method="centroid")
    hcluster
})


get_cormatrix <- reactive({
    dat <- get_preds()
    nums <- sapply(dat, is.numeric)
    if(input$cortype=="Pearson")  M <- cor(dat[,nums], use="pairwise.complete.obs")
    if(input$cortype=="Spearman")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="spearman")
    if(input$cortype=="Kendall")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="kendall")
    #m2 <- mine(data.matrix(na.omit(dat[,nums])))
    if(input$cortype=="MIC (Maximal Information Coefficient)") {m2 <- get_mine(); M <- data.matrix(m2$MIC) }
    if(input$cortype=="MAS (Maximal Asymmetry Score)") {m2 <- get_mine(); M <-  data.matrix(m2$MAS) }
    if(input$cortype=="MEV (Maximum Edge Value)") {m2 <- get_mine(); M <- data.matrix(m2$MEV)}
    if(input$cortype=="MCN (Minimum Cell Number)") {m2 <- get_mine(); M <- data.matrix(m2$MCN) }
    if(input$cortype=="MIC-R2") {m2 <- get_mine(); M <- data.matrix(m2$MICR2)}
    M
})



output$corrs <- renderPlot({
    #if (is.null(input$action)) return()
    if(is.null(input$allvars)) return()
    #if(is.null(input$allvars) | is.null(input$subschk) | is.null(input$dependent)) return()
    if(input$allvars==F & is.null(input$predgroups)) return()
    if(is.null(input$cortype)) return()
    if(is.null(input$distfun)) return()
    if(is.null(input$hfun)) return()
    input$unsigned
    
    input$pcaButton
    isolate({
        dat <- get_preds()
        #     df <- filedata()
        #     
        #     if(input$allvars == TRUE & is.null(input$removed)){
        #         predicto <- names(df)
        #     } else if(input$allvars == TRUE & !is.null(input$removed)){
        #         predicto <- names(df)[-which(names(df) %in% c(input$removed))]
        #     } else if(input$allvars == FALSE){
        #         predicto <- c(input$independents, input$pcagroup)
        #         predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
        # #         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
        # #         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
        # #         if(input$demovars == T) {predicto <- c(predicto, demovars)}
        # #         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
        # #         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
        # #         if(input$bcells == T) {predicto <- c(predicto, bcells)}
        # #         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
        #         if(!is.null(input$removed)) {
        #             index <- which(predicto %in% input$removed)
        #             print(paste("hi ",index),sep="")
        #             if(any(index)==T){
        #                 predicto <- predicto[-index]
        #             } else { predicto <- predicto}
        #         }
        #     }
        #     #print(which(predicto %in% input$removed))
        #     print(predicto)
        #     
        #     if(input$subschk==F) {
        #         dat <-droplevels( df[,c(predicto)] )
        #         
        #     } else {
        #         dat <- droplevels(subset(
        #             df[,c(predicto)],  df[,input$subs] %in% input$subsvar
        #         ))
        #     }
        #     
        nums <- sapply(dat, is.numeric)
        
        
        
#         if(input$cortype=="Pearson")  M <- cor(dat[,nums], use="pairwise.complete.obs")
#         if(input$cortype=="Spearman")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="spearman")
#         if(input$cortype=="Kendall")  M <- cor(dat[,nums], use="pairwise.complete.obs", method="kendall")
#         #m2 <- mine(data.matrix(na.omit(dat[,nums])))
#         if(input$cortype=="MIC (Maximal Information Coefficient)") {m2 <- get_mine(); M <- data.matrix(m2$MIC) }
#         if(input$cortype=="MAS (Maximal Asymmetry Score)") {m2 <- get_mine(); M <-  data.matrix(m2$MAS) }
#         if(input$cortype=="MEV (Maximum Edge Value)") {m2 <- get_mine(); M <- data.matrix(m2$MEV)}
#         if(input$cortype=="MCN (Minimum Cell Number)") {m2 <- get_mine(); M <- data.matrix(m2$MCN) }
#         if(input$cortype=="MIC-R2") {m2 <- get_mine(); M <- data.matrix(m2$MICR2)}
        
        M <- get_cormatrix()
        distfunc1 <- get_distfunc()
        hcluster <- get_hclust()
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
#         if(input$distfun=="Unsigned") distfunc1 <- function(x) as.dist(1-abs(x))
#         #if(input$distfun=="Unsigned L2") distfunc1 <- function(x) as.dist(sqrt(1-x^2))
#         if(input$distfun=="Signed") distfunc1 <- function(x) as.dist(1-x)
#         #if(input$distfun=="Signed L2") distfunc1 <- function(x) as.dist(sign(1-x)*sqrt(1-x^2))
#         if(input$distfun=="L2") distfunc1 <- function(x) dist(x)
#         if(input$distfun=="L1") distfunc1 <- function(x) dist(x, method="manhattan")
#         
        
        
#         if(input$unsigned==T){
#             if(input$distfun=="L1") distfunc1 <- function(x) dist(abs(x), method="manhattan")
#             if(input$distfun=="L2") distfunc1 <- function(x) dist(abs(x))
#             if(input$distfun=="Pearson") distfunc1 <- function(x) as.dist(1-abs(x))
#             if(input$distfun=="maximum") distfunc1 <- function(x) dist(abs(x), method="maximum")
#             if(input$distfun=="canberra") distfunc1 <- function(x) dist(abs(x), method="canberra")
#             if(input$distfun=="binary") distfunc1 <- function(x) dist(abs(x), method="binary")
#         } else{
#             if(input$distfun=="L1") distfunc1 <- function(x) dist(x, method="manhattan")
#             if(input$distfun=="L2") distfunc1 <- function(x) dist(x)
#             if(input$distfun=="Pearson") distfunc1 <- function(x) as.dist(1-x)
#             if(input$distfun=="maximum") distfunc1 <- function(x) dist(x, method="maximum")
#             if(input$distfun=="canberra") distfunc1 <- function(x) dist(x, method="canberra")
#             if(input$distfun=="binary") distfunc1 <- function(x) dist(x, method="binary")
#         }
#         
        
#         if(input$hfun=="Ward") hcluster <- function(x) hclust(x, method="ward")
#         if(input$hfun=="Single") hcluster <- function(x) hclust(x, method="single")
#         if(input$hfun=="Complete") hcluster <- function(x) hclust(x, method="complete")
#         if(input$hfun=="Average") hcluster <- function(x) hclust(x, method="average")
#         if(input$hfun=="Mcquitty") hcluster <- function(x) hclust(x, method="mcquitty")
#         if(input$hfun=="Median") hcluster <- function(x) hclust(x, method="median")
#         if(input$hfun=="Centroid") hcluster <- function(x) hclust(x, method="centroid")
#         
        
        

        
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
    
}, height=npred_clust)
# output$idcorrs <- renderPlot({
#     #if (is.null(input$action)) return()
#     df <- filedata()
#     
#     if(input$allvars == TRUE & is.null(input$removed)){
#         predicto <- names(df)
#     } else if(input$allvars == TRUE & !is.null(input$removed)){
#         predicto <- names(df)[-which(names(df) %in% c(input$removed))]
#         predicto <- c(predicto)
#     } else if(input$allvars == FALSE){
#         predicto <- c(input$independents)
#         predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
# #         if(input$eigvars == T) {predicto <- c(predicto, eigvars)}
# #         if(input$phenovars == T) {predicto <- c(predicto, phenovars)}
# #         if(input$demovars == T) {predicto <- c(predicto, demovars)}
# #         if(input$t1vars == T) {predicto <- c(predicto, t1vars)}
# #         if(input$t3vars == T) {predicto <- c(predicto, t3vars)}
# #         if(input$bcells == T) {predicto <- c(predicto, bcells)}
# #         if(input$cytokine == T) {predicto <- c(predicto, cytokine)}
#         if(!is.null(input$removed)) {
#             index <- which(predicto %in% input$removed)
#             if(any(index)==T){
#                 predicto <- predicto[-index]
#             } else { predicto <- predicto}
#         }
#     }
#     #print(which(predicto %in% input$removed))
#     print(predicto)
#     
#     if(input$subschk==F) {
#         dat <- droplevels( df[,c(input$labcol, predicto)] )
#         
#     } else {
#         dat <-droplevels( subset(
#             df[,c(input$labcol, predicto)],  df[,input$subs] %in% input$subsvar
#         ))
#     }
#     
#     nums <- sapply(dat, is.numeric)
#     
#     #cor(na.omit(t(csv[,nums])), use="pairwise.complete.obs")
#     #cor(t(as.matrix(csv[-20,nums])), use="pairwise.complete.obs")
#     
#     
#     M <- cor(t(dat[,nums]), use="pairwise.complete.obs")
#     if(input$cortype=="Pearson")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs")
#     if(input$cortype=="Spearman")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs", method="spearman")
#     if(input$cortype=="Kendall")  M <- cor(t(dat[,nums]), use="pairwise.complete.obs", method="kendall")
#     
#     if(input$cortype=="MIC (Maximal Information Coefficient)"){
#         m1 <- mine(t(data.matrix(na.omit(dat[,nums]))))
#         M <- m1$MIC
#     }  
#     if(input$cortype=="MAS (Maximal Asymmetry Score)")  M <- m1$MAS
#     if(input$cortype=="MEV (Maximum Edge Value)")  M <- m1$MEV
#     if(input$cortype=="MCN (Minimum Cell Number)")  M <- m1$MCN
#     if(input$cortype=="MIC-R2")  M <- m1$MICR2
#     #M <- dat[,nums]
#     N <- data.matrix(M)
#     
#     rname.tmp <- dat[,input$labcol]
#     if(length(input$labcol)>=2) {
#         rownames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
#         colnames(N) <- apply(rname.tmp,1, function(x) paste(x,collapse=", "))
#     } else {
#         rownames(N) <- rname.tmp
#         colnames(N) <- rname.tmp
#     }
#     
#     my_palette <- colorRampPalette(c("blue", "white", "red"))(n = 299)
#     #col_breaks = c(seq(-1,-3,length=100),  # for red
#     #               seq(-.3,0.3,length=100),              # for yellow
#     #               seq(0.3,1,length=100))
#     #     corrplot(M, method="color", col=col(200),  
#     #              order="hclust", addgrid.col="grey", outline=T, hclust.method="average",
#     #              #addCoef.col = "black", addCoefasPercent = T, # Add coefficient of correlation
#     #              tl.col="black", tl.cex=30/nrow(M), tl.srt=45, #Text label color and rotation
#     #              # Combine with significance
#     #              #p.mat = p.mat, sig.level = 0.05, insig = "blank", 
#     #              # hide correlation coefficient on the principal diagonal
#     #              diag=FALSE 
#     #     )
#     if(input$distfun=="Unsigned") distfunc <- function(x) as.dist(1-abs(x))
#     #if(input$distfun=="Unsigned L2") distfunc <- function(x) as.dist(sqrt(1-x^2))
#     if(input$distfun=="Signed") distfunc <- function(x) as.dist(1-x)
#     if(input$distfun=="L2") distfunc <- function(x) dist(x)
#     if(input$distfun=="L1") distfunc <- function(x) dist(x, method="manhattan")
#     if(input$hfun=="Single") hcluster <- function(x) hclust(x, method="single")
#     if(input$hfun=="Ward") hcluster <- function(x) hclust(x, method="ward")
#     if(input$hfun=="Complete") hcluster <- function(x) hclust(x, method="complete")
#     if(input$hfun=="Average") hcluster <- function(x) hclust(x, method="average")
#     if(input$hfun=="Mcquitty") hcluster <- function(x) hclust(x, method="mcquitty")
#     if(input$hfun=="Median") hcluster <- function(x) hclust(x, method="median")
#     if(input$hfun=="Centroid") hcluster <- function(x) hclust(x, method="centroid")
#     #if(input$distfun=="Signed L2") distfunc <- function(x) as.dist(sign(1-x)*sqrt(1-x^2))
#     heatmap.2(N, distfun = distfunc, hclustfun = hcluster,
#               #cellnote = mat_data,  # same data set for cell labels
#               main = "Clustered Entities", # heat map title
#               key=T, key.title = "color key", key.xlab = "correlation",
#               #keysize=.8, 
#               symkey=F, symm=T, symbreaks=F, #denscol="grey",
#               #notecol="black",      # change font color of cell labels to black
#               #density.info="none",  # turns off density plot inside color legend
#               trace="none",         # turns off trace lines inside the heat map
#               margins =c(5,5),     # widens margins around plot
#               col=my_palette,       # use on color palette defined earlier
#               #breaks=col_breaks,    # enable color transition at specified limits
#               dendrogram="both",     # only draw a row dendrogram
#               Colv=T)            # turn off column clustering
#     
#     rmatrix.plot <<- recordPlot()
#     replayPlot(rmatrix.plot) # redraw
# })




output$plotnetwork <- renderPlot({
    if(is.null(input$allvars)) return()
    if(input$allvars==F & is.null(input$predgroups)) return()
    if(is.null(input$cortype)) return()
    #if(is.null(input$distfun)) return()
    #if(is.null(input$hfun)) return()
    input$unsigned
    if(is.null(input$netlayout)) return()
    input$pcaButton
    M <- get_cormatrix()
    
    isolate({
        #dat <- get_preds()
        #nums <- sapply(dat, is.numeric)
        
       
        #distfunc1 <- get_distfunc()
        #hcluster <- get_hclust()
       
        M2 <- M 
        
        #sigfilter <- !is.na(cellBHnote.full) #from 4-dynamic eigengene network
        
        thresh = input$corthresh
        M2[abs(M) < thresh] <- 0 
        #M2[!sigfilter[circle.indices$ix,circle.indices$ix]]<-0
        
        g = graph.adjacency(abs(M2),
                            mode = "undirected",
                            weighted = TRUE,
                            diag = FALSE,
                            add.colnames = NULL)
        
        metrics <- data.frame(
            #deg = degree(g),
            bet = betweenness(g),
            clos = closeness(g),
            eig = evcent(g),
            cor = graph.coreness(g)
        )
        
        all.edges = g[,edges=TRUE]
        all.edges.indices = as.numeric(all.edges[all.edges!=0])
        
        #EIGcor <- cor(dat[,nums], use="p")
        EIGcor <- M
        
        M2 <- M 
        posEdges = all.edges*(EIGcor>0)
        pos.edge.indices = as.numeric(posEdges[posEdges!=0])
        negEdges = all.edges*(EIGcor<0)
        neg.edge.indices = as.numeric(negEdges[negEdges!=0])
        
        E(g)$color <- ifelse(E(g) %in% neg.edge.indices, "blue","red")
        
#         radian.rescale <- function(x, start=0, direction=1) {
#             c.rotate <- function(x) (x + start) %% (2 * pi) * direction
#             c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
#         }
#         la <- layout.circle(g)
#         lab.locs <- radian.rescale(x=1:dim(EIGcor)[1], direction=-1, start=0)
        
        #if(input$netlayout=="layout.fruchterman.reingold") layoutvar <- layout.fruchterman.reingold
        if(input$netlayout=="layout.fruchterman.reingold"){
            layoutfun <- function(x) layout.fruchterman.reingold(x)
        }
        if(input$netlayout=="layout.mds"){
            layoutfun <- function(x) layout.mds(x)
        }
        if(input$netlayout=="layout.circle"){
            layoutfun <- function(x) layout.circle(x)
        }
        if(input$netlayout=="layout.sphere"){
            layoutfun <- function(x) layout.sphere(x)
        }
        if(input$netlayout=="layout.kamada.kawai"){
            layoutfun <- function(x) layout.kamada.kawai(x)
        }
        if(input$netlayout=="layout.reingold.tilford"){
            layoutfun <- function(x) layout.reingold.tilford(x)
        }
        if(input$netlayout=="layout.fruchterman.reingold.grid"){
            layoutfun <- function(x) layout.fruchterman.reingold.grid(x)
        }
        
        
        #print(input$predgroups)
        fillcolors <- rep("white", ncol(M))
        if(input$allvars==T){
            boxcolors <- rainbow( length(predgroupnames))
            for(i in 1: length(predgroupnames)){
                fillcolors[which(colnames(M) %in% unlist(predgroupnames[i]))] <- boxcolors[i]
            }
        } else {
            boxcolors <- rainbow(length(input$predgroups))
            for(i in 1: length(input$predgroups)){
                fillcolors[which(colnames(M) %in% unlist(predgroupnames[input$predgroups[i]]))] <- boxcolors[i]
            }
        }
        
        #predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
        #fillcolors[which(colnames(M) %in% input$dependent)] <- boxcolors[1]
        
        
#         for(i in 1: length(input$predgroups)){
#             fillcolors[which(colnames(M) %in% unlist(predgroupnames[input$predgroups[i]]))] <- boxcolors[i]
#         }
        #print(fillcolors)
        
        plot.igraph(g,
                    vertex.size = 10*(metrics$eig.vector/max(metrics$eig.vector))^(1/4),
                    vertex.size2 = 1000,
                    vertex.color = fillcolors,
                    vertex.frame.color = "black",
                    vertex.shape = "sphere",
                    #vertex.label = rownames(M2), 
                    vertex.label = names(M), 
                    vertex.label.font = 2, 
                    vertex.label.cex = .7,          
                    #vertex.label.dist = 1.25, 
                    #vertex.label.degree = lab.locs, 
                    vertex.label.color = "black",            
                    #edge.color="blue",
                    #edge.width = all.edges*t0adj
                    edge.width = 2* fisherz((E(g)$weight)), 
                    edge.arrow.size = 0.5,   
                    layout = layoutfun,
                    #layout = layout.fruchterman.reingold,
                    #layout=layout.mds,  
                    #layout=layout.circle,
                    #layout=layout.sphere, 
                    #layout=layout.reingold.tilford,
                    #layout=layout.fruchterman.reingold.grid,
                    #layout=layout.kamada.kawai,
                    rescale = T)
        network.plot <<- recordPlot()
        replayPlot(network.plot) # redraw
    })
    
}, height=screenheight)





output$circle <- renderPlot({
    if(is.null(input$allvars)) return()
    if(analysis() != "circleClustering") return()
    if(is.null(input$allvars)) return()
    if(input$allvars==F & is.null(input$predgroups)) return()
    if(is.null(input$cortype)) return()
    if(is.null(input$distfun)) return()
    if(input$distfun=="Lp" & is.null(input$p)) return()
    if(is.null(input$hfun)) return()
    if(is.null(input$dendheight)) return()
    if(is.null(input$labelheight)) return()
    input$unsigned
    input$pcaButton
    
    isolate({
        dat <- get_preds()
        nums <- sapply(dat, is.numeric)
        #nums <- sapply(csv, is.numeric)
        
        pca <- getpca()
        pcs <- pca$pca$x
        npc <- sum((cumsum(pca$pca$sdev)/sum(pca$pca$sdev))*100 < 95)
        
        #M <- get_cormatrix()
        distfunc1 <- get_distfunc()
        hcluster <- get_hclust()
        
       # d_pred <- dist(t(csv[,nums]))
        d_pred <- distfunc1(t(dat[,nums])) 
        hc_pred <- hcluster(d_pred)
        
        #iris_species <- rev(levels(iris[,5]))
        
       
        dend <- as.dendrogram(hc_pred)
        # order it the closest we can to the order of the observations:
        #dend <- rotate(dend, 1:150)
        
        # Color the branches based on the clusters:
        dend <- color_branches(dend, k=npc) #, groupLabels=iris_species)
        
        # Manually match the labels, as much as possible, to the real classification of the flowers:
       # labels_colors(dend) <-
    #        rainbow_hcl(3)[sort_levels_values(
     #           as.numeric(iris[,5])[order.dendrogram(dend)]
      #      )]
        
        # We shall add the flower type to the labels:
#         labels(dend) <- paste(as.character(colnames(dat[,nums]))[order.dendrogram(dend)],
#                               "(",labels(dend),")", 
#                               sep = "")
#         labels(dend) <- paste(as.character(dat[,input$dependent])[order.dendrogram(dend)],
#                               "(",labels(dend),")", 
#                               sep = "")
        #labels(dend) <- colnames(csv[,nums])
        labels(dend) <- as.character(colnames(dat[,nums]))[order.dendrogram(dend)]
        # We hang the dendrogram a bit:
    #    dend <- hang.dendrogram(dend,hang_height=.5)
        # reduce the size of the labels:
        # dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
        dend <- set(dend, "labels_cex", 1 )
        # And plot:
#         par(mar = c(3,3,3,7))
#         plot(dend, 
#              main = "Clustered Iris data set
#      (the labels give the true flower species)", 
#              horiz =  TRUE,  nodePar = list(cex = .007))
# #         legend("topleft", 
#                legend = rev(levels(csv[,"DATASET"])), 
#                fill = rainbow(5))
        
        par(mar = rep(5,4))
        circlize_dendrogram(dend, labels=TRUE, dend_track_height=input$dendheight, labels_track_height=input$labelheight)
       
        circos.plot <<- recordPlot()
        replayPlot(circos.plot) # redraw
    })
    
}, height=screenheight)














