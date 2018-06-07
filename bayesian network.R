# bayesian network

# Bayesian network
output$bnalgo <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Bayesian Network")){
        #items=names(df)
        #names(items)=items
        selectInput("bnalgo","Select BN algorithm:", c(
                                                        "hill-climbing (HC)"="hc", 
                                                        "Tabu Search"="tabu", 
                                                        "Grow-Shrink (GS)"="gs",
                                                       "Incremental Association (IAMB)"="iamb", 
                                                       "Fast Incremental Association (fIAMB)"="fast.iamb",
                                                       "Interleaved Incremental Association (Inter-IAMB)"="inter.iamb",
                                                       
                                                       "Max-Min Hill-Climbing"="mmhc",
                                                       "Restricted Maximization"="rsmax2",
                                                       "Max-Min Parents and Children"="mmpc",
                                                       "Hiton Parents and Children"="si.hiton.pc",
                                                       "Chow-Liu"="chow.liu",
                                                       "ARACNE"="aracne"),
                    
                    selected=input$bnalgo, selectize=T)
    }
    else return(NULL)
})
output$bnlayout <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    if (analysis() %in% c("Bayesian Network")){
        selectInput("bnlayout","Select BN layout:", c(
                                                      "fdp"="fdp" ,
                                                      "dot"="dot",
                                                      "neat"="neato", 
                                                      "circular"="twopi",
                                                      "perfect circle"="circo"
                                                      
        ),
        
        selected=input$bnlayout, selectize=T)
    }
    else return(NULL)
})

compute.bn <- reactive({
    df <- filedata()
    #print("Lets do bayes")
    # isolate({
    if(input$allvars == TRUE & is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$dependent))]
    } else if(input$allvars == TRUE & !is.null(input$removed)){
        predicto <- names(df)[-which(names(df) %in% c(input$removed, input$dependent))]
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
        if(!is.null(input$removed)) { index <- which(predicto %in% c(input$removed, input$dependent)) }
        if(is.null(input$removed)) { index <- which(predicto %in% c(input$dependent)) }
        if(any(index)==T){
            predicto <- predicto[-index]
        } else { predicto <- predicto}
        
    }
    #print(which(predicto %in% input$removed))
    #print(predicto)
    
    if(input$subschk==F) {
        dat <-  df[,c(input$dependent, predicto)] 
        toomanymissing <- which(sapply(dat, function(x) sum(is.na(x)))/nrow(dat) > na.pct)
        if(is.na(toomanymissing[1])){
            bndata <- droplevels(na.omit(dat))
        } else {
            bndata <- droplevels(na.omit(dat[,-toomanymissing]))
        }
        #bndata <- droplevels(na.omit(dat[,-toomanymissing]))
        #bndata <- droplevels(na.omit( df[,c(input$dependent, predicto)] ))
        
    } else {
        if(length(input$subs)>1){
            insubgroup <- as.logical(apply(sapply(df[,input$subs], function(x) { x %in% c(input$subsvar)}), 1, prod))
            df.tmp <- df[insubgroup, c(input$dependent, predicto)] 
        } else {
            #insubgroup <- df[,input$subs] %in% input$subsvar
            df.tmp <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% c(input$subsvar)  )
        }
        toomanymissing <- which(sapply(df.tmp, function(x) sum(is.na(x)))/nrow(df.tmp) > na.pct)
        if(is.na(toomanymissing[1])){
            bndata <- droplevels(na.omit(df.tmp))
        } else {
            bndata <- droplevels(na.omit(df.tmp[,-toomanymissing]))
        }
        #bndata <- droplevels(na.omit(df.tmp[,-toomanymissing]))
        
        #bndata <- droplevels( na.omit( df.tmp))
        #bndata <-droplevels( na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar )))
    }
    
    #print(bndata)
    
    if(input$bnalgo=="gs") alarm <- gs(bndata)
    if(input$bnalgo=="iamb") alarm <- iamb(bndata)
    if(input$bnalgo=="fast.iamb") alarm <- fast.iamb(bndata)
    if(input$bnalgo=="inter.iamb") alarm <- inter.iamb(bndata)
    if(input$bnalgo=="mmpc") alarm <- mmpc(bndata)
    if(input$bnalgo=="hc") alarm <- hc(bndata,restart=1, perturb=2)
    if(input$bnalgo=="tabu") alarm <- tabu(bndata)
    if(input$bnalgo=="mmhc") alarm <- mmhc(bndata)
    if(input$bnalgo=="rsmax2") alarm <- rsmax2(bndata)
    if(input$bnalgo=="si.hiton.pc") alarm <- si.hiton.pc(bndata)
    if(input$bnalgo=="aracne") {
        nums <- sapply(bndata,is.numeric)
        bndata <- bndata[,nums]
        alarm <- aracne(data.frame(bndata))
    } 
    if(input$bnalgo=="chow.liu"){
        nums <- sapply(bndata,is.numeric)
        bndata <- bndata[,nums]
        alarm <- chow.liu(bndata)
    } 
    
    return(list(data=bndata, bn=alarm))
})

output$bn <- renderPlot({
    #if (is.na(input$bnalgo)) return()
    #if(analysis() != "Bayesian Network") return()
    if(is.null(input$bnalgo)) stop("timing glitch. Starting calculation...")
    if(input$pcaButton==0) stop("Press RUN to initiate computation.")
    
    isolate({
        bn <- compute.bn()
        bndata <- bn$data
        alarm <- bn$bn
        
        fillcolors <- rep("white", ncol(bndata))
        if(input$allvars==T){
            boxcolors <- rainbow( length(predgroupnames) + 1)
            for(i in 1: length(predgroupnames)){
                fillcolors[which(names(bndata) %in% unlist(predgroupnames[i]))] <- boxcolors[i+1]
            }
        } else {
            boxcolors <- rainbow(length(input$predgroups) + 1)
            if(input$dependent %in% names(bndata)){
                fillcolors[which(names(bndata) %in% input$dependent)] <- boxcolors[1]
            }
            
            for(i in 1: length(input$predgroups)){
                fillcolors[which(names(bndata) %in% unlist(predgroupnames[input$predgroups[i]]))] <- boxcolors[i+1]
            }
        }
        
#         fillcolors <- rep("white", ncol(bndata))
#         boxcolors <- rainbow(length(input$predgroups)+1)
#         #predicto <- c(predicto, unlist(predgroupnames[input$predgroups]))
#         fillcolors[which(names(bndata) %in% input$dependent)] <- boxcolors[1]
#         for(i in 1: length(input$predgroups)){
#             fillcolors[which(names(bndata) %in% unlist(predgroupnames[input$predgroups[i]]))] <- boxcolors[i+1]
#         }
        
        #     fillcolors[which(names(bndata) %in% demovars)] <- "green"
        #     fillcolors[which(names(bndata) %in% phenovars)] <- "pink"
        #     fillcolors[which(names(bndata) %in% eigvars)] <- "cyan"
        #     fillcolors[which(names(bndata) %in% t1vars)] <- "orange"
        #     fillcolors[which(names(bndata) %in% t3vars)] <- "magenta"
        #     fillcolors[which(names(bndata) %in% bcells)] <- "turquoise"
        #     fillcolors[which(names(bndata) %in% cytokine)] <- "red"
        #     fillcolors[which(names(bndata) %in% input$dependent)] <- "yellow"
        
        
        
        highlight.opts <- list(nodes = names(bndata), col = "black", fill = fillcolors)
        bn.layout <- input$bnlayout
        graphviz.plot(alarm, highlight = highlight.opts, shape="rectangle", main="Bayesian Network", layout=bn.layout)
        #arcs = boot.strength(bndata, algorithm = input$bnalgo, R=10)
        #graphviz.plot(averaged.network(arcs, threshold=.35), highlight = highlight.opts, shape="rectangle", main="Averaged Bayesian Netowrk")
    })
    

}, height=screenheight)









