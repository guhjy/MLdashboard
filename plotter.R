library(ggplot2)
#library(lme4)
library(lmerTest)


output$dependent2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    selectInput("dependent2","Select variable to predict:", items, selected=dependent2.init, selectize=T)
    #}
    #else return(NULL)
})

# output$independent2 <- renderUI({
#     df <- filedata()
#     if (is.null(df)) return(NULL)
#     #if (analysis() %in% c("Modeling", "Bayesian Network")){
#     items=names(df)
#     names(items)=items
#     selectInput("independent2","Select predictor A:", items, selected=independent2.init, selectize=T)
#     #}
#     #else return(NULL)
# })

output$plottype <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    
    selectInput("plottype","Select plot type:", c("Box plot", "Line plot", "Spine plot"), selected=plottype.init, selectize=T)
    #}
    #else return(NULL)
})

output$grouping2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    #selectInput("grouping2","Select interaction variables B:", c("none",items), multiple=T, selected="Survivor", selectize=T)
    selectInput("grouping2","Select interacting variables:", c(items), multiple=T, selected=grouping2.init, selectize=T)
    #}
    #else return(NULL)
})

output$traces <- renderUI({
    df <- filedata()
    if(is.null(input$rptmeas)) return(NULL)
    if (is.null(df) | input$rptmeas==F) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    selectInput("traces","Select repeated measure ID:", c("none",items), selected=traces.init, selectize=T)
    #}
    #else return(NULL)
})

output$rptmeas <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    checkboxInput("rptmeas", "Use GLMM for repeated measures", value=rptmeas.init, width='100%')
})

output$controlling <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("controlling","Fixed effects:", c(items), multiple=TRUE, selected=controlling.init, selectize=T)
})

# plot points or not?
output$pointchk  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("pointchk", "Plot datapoints", value=pointchk.init, width='100%')
})

# plot points or not?
output$randomslope  <- renderUI({
    #df <- filedata()
    #if (is.null(df)) return(NULL)
    checkboxInput("randomslope", "Add random slopes", value=slopechk.init, width='100%')
})



get_subsetGLMM <- reactive({
    #if(is.null(input$subsGLMM)) return()
    #if(is.null(input$subsvarGLMM)) return()
    df <- filedata()
    if(input$subschkGLMM==F) {
        dat <-  df
#         toomanymissing <- which(sapply(dat, function(x) sum(is.na(x)))/nrow(dat) > na.pct)
#         if(is.na(toomanymissing[1])){
#             dat <- droplevels(na.omit(dat))
#         } else {
#             dat <- droplevels(na.omit(dat[,-toomanymissing]))
#        }
        
    } else {
        #dat <- droplevels( na.omit( subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )))
        # original dat <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )
        
        #dat <- subset( df[,c(input$dependent, predicto)],  df[,input$subs] %in% input$subsvar  )
        #insubgroup <- as.logical(apply(sapply(csv[,c("DATASET","Self_Win")], function(x) { x %in% subsvar[-1]}), 1, prod))
        if(length(input$subsGLMM)>1){
            insubgroup <- as.logical(apply(sapply(df[,input$subsGLMM], function(x) { x %in% input$subsvarGLMM}), 1, prod))
            dat <- droplevels(df[insubgroup,] )
        } else {
            #insubgroup <- df[,input$subs] %in% input$subsvar
            dat <- droplevels(subset( df,  df[,input$subsGLMM] %in% input$subsvarGLMM  ))
        }
        
        
#         toomanymissing <- which(sapply(dat, function(x) sum(is.na(x)))/nrow(dat) > na.pct)
#         if(is.na(toomanymissing[1])){
#             dat <- droplevels(na.omit(dat))
#         } else {
#             dat <- droplevels(na.omit(dat[,-toomanymissing]))
#         }
        #dat <- droplevels(na.omit(dat[,-toomanymissing]))
    }
    dat
})









output$plotter <- renderPlot({
    #df <- filedata()
    df <- get_subsetGLMM()
    #if (is.null(df) | is.null(input$rptmeas) | is.null(input$grouping2) | is.null(input$dependent2) | is.null(input$traces)) return(NULL)
    if(input$randomeffects=="") {
        indivtrace <- NULL
    } else {
        strsplit(input$randomeffects, ",")
        firstsplit <- unlist(strsplit(strsplit(input$randomeffects, ",")[[1]][1], "\\/|\\:"))  # split on / or :
        #firstsplit[length(firstsplit)]
        indivtrace <- firstsplit[length(firstsplit)]
    }
    # if(input$rptmeas==T){indivtrace <- if(input$traces=="none"){NULL} else{input$traces} }
    facetgroup <- if(length(input$grouping2)==1){NULL} else{input$grouping2[2]} 
    
    if (input$plottype=="Box plot"){
        #if(is.numeric(df[,input$grouping2[1]])) {df[,input$grouping2[1]] <- cut(df[,input$grouping2[1]],4)}
        if(length(input$grouping2)==1){
            if(is.numeric(df[,input$grouping2[1]])) {df[,input$grouping2[1]] <- cut(df[,input$grouping2[1]],4, labels=c("Q1","Q2","Q3","Q4"))}
            pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2)) 
            if(input$pointchk==T){
                p2 <- pl1  + geom_boxplot(notch=F) + geom_point(shape=16, position=position_jitter(width=.2,height=.5)) + theme_bw()
            } else{
                p2 <- pl1  + geom_boxplot(notch=F) + theme_bw()
            }
            p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
        } else { 
            if(is.numeric(df[,input$grouping2[1]])) {df[,input$grouping2[1]] <- cut(df[,input$grouping2[1]],4)}
            if(is.numeric(df[,input$grouping2[2]])) {df[,input$grouping2[2]] <- cut(df[,input$grouping2[2]],4)}
            pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2, fill=input$grouping2[1]))
            if(input$pointchk==T){
                p2 <- pl1   + geom_boxplot( notch=F)  + geom_point(shape=16, position=position_jitter(width=.2,height=.5)) + facet_grid(as.formula(paste(". ~", input$grouping2[2])), scales="free")  + theme_bw()  
            } else {
                p2 <- pl1   + geom_boxplot( notch=F)  + facet_grid(as.formula(paste(". ~", input$grouping2[2])), scales="free")  + theme_bw()  
            }
            
 
            p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
        }
        
    } else if(input$plottype=="Line plot"){
        if(length(input$grouping2)==1){
            #if(input$rptmeas==F){
            if(input$randomeffects==""){
                pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2))
                if(input$pointchk==T){
                    p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw()
                } else {
                    p2 <-  pl1 + geom_smooth() + theme_bw()
                }
                
                #p2 <- pl1 + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
            } else{ 
                pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2, group = indivtrace))
                if(class(df[,input$grouping2[1]])=="factor"){
                    if(input$pointchk==T){
                        p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                    } else {
                        p2 <- pl1 + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                    }
                   
                        
                } else{
                    print("not smooth")
                    if(input$pointchk==T){
                        p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + theme_bw() + stat_smooth(aes_string(group = 1))
                    } else {
                        p2 <- pl1 + geom_line(colour="grey") + theme_bw() + stat_smooth(aes_string(group = 1))
                    }
                    
                }
                
                p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        } else {
            if(is.numeric(df[,input$grouping2[2]])) {df[,input$grouping2[2]] <- cut(df[,input$grouping2[2]],4)}
            #if(input$rptmeas==F){
            if(input$randomeffects==""){
                pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2, fill=input$grouping2[2]))
                if(class(df[,input$grouping2[1]])=="factor"){
                    if(input$pointchk==T){
                        p2 <- pl1 +  geom_point(shape=16) + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    } else {
                        p2 <- pl1 + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    }
                    
                } else{
                    if(input$pointchk==T){
                        p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    } else {
                        p2 <-  pl1 + geom_smooth() + theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    }
                    
                }
                #pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2, fill=input$grouping2))
                #p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                #p2 <- pl1  + stat_smooth()  +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
            } else {
                pl1 <- ggplot(data = df, aes_string(x = input$grouping2[1], y = input$dependent2, group = indivtrace, fill=input$grouping2[2]))
                if(class(df[,input$grouping2[1]])=="factor"){
                    if(input$pointchk==T){
                        p2 <- pl1 +  geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    } else {
                        p2 <- pl1 + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    }
                    
                } else{
                    if(input$pointchk==T){
                        p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1))  +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    } else {
                        p2 <- pl1 + geom_line(colour="grey") + stat_smooth(aes_string(group = 1))  +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2[2])))
                    }
                    
                }
                p2 + scale_fill_hue()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))
            }
        }
       

    } else if(input$plottype=="Spine plot"){
        if(is.numeric(df[,input$grouping2[1]])) {df[,input$grouping2[1]] <- cut(df[,input$grouping2[1]],4)}
        if(is.numeric(df[,input$dependent2])) {df[,input$dependent2] <- cut(df[,input$dependent2],4)}
#         pl1 <- ggplot(data=df, aes_string(x = input$independent2, y = input$dependent2, fill=input$grouping2, color=input$grouping2)) 
#         p2 <- pl1 + geom_bar(stat="identity", position=position_dodge()) 
       print( spineplot(factor(df[,input$dependent2])~factor(df[,input$grouping2[1]]), xlab=input$grouping2[1], ylab=input$dependent2, col=rainbow(length(levels(df[,input$dependent2]))))  )
    }
    
    

  
}, height=screenheight)


create_formula <- function(){
    randeff <- input$randomeffects
    Y <- input$dependent2
    if(length(input$controlling)==0){
        X <- paste(input$grouping2[1], sep="")
    } else { X <- paste(input$grouping2[1], paste(input$controlling, collapse="+"), sep="+") }
    if(randeff==""){
        if(is.null(input$grouping2)){
            fmla <- as.formula(paste(Y, "~", X, sep=" "))
        }
        if(!is.null(input$grouping2) & !is.null(input$controlling)){
            #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, sep=" "))
            fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+", paste(input$controlling, collapse="+"),  sep=" "))
        }
        if(!is.null(input$grouping2) & is.null(input$controlling)){
            #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, sep=" "))
            fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"),  sep=" "))
        }
    } else {
        #randeff <- "DATASET/sTEAMS, RECORD"
        effectslist <- strsplit(randeff, ",")
        
        if(input$randomslope == T){
            make_slopeintercept <- function(x) paste("(1+", paste(input$grouping2,collapse="+"),  "|", x, ")", sep="")
            slopesinterc <- sapply(effectslist, make_slopeintercept)
            mixedeffect <- paste(slopesinterc, collapse="+")
        } else {
            make_intercept <- function(x) paste("(1|", x, ")", sep="")
            intercepts <- sapply(effectslist, make_intercept)
            mixedeffect <- paste(intercepts, collapse="+")
        }

        
        
        if(is.null(input$grouping2)){
            fmla <- as.formula(paste(Y, "~",
                                     X, "+", mixedeffect, sep=" "))
        }
        if(!is.null(input$grouping2) & !is.null(input$controlling)){
            #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, "+ (1|", input$traces, ")", sep=" "))
            fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+", paste(input$controlling, collapse="+"), "+", mixedeffect, sep=" "))
        }
        if(!is.null(input$grouping2) & is.null(input$controlling)){
            #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, "+ (1|", input$traces, ")", sep=" "))
            fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+", mixedeffect, sep=" "))
        }
    }
    fmla
}



# create_formula <- function(){
#     rptm <- input$rptmeas
#     #grp <- input$grouping2
#     Y <- input$dependent2
#     if(length(input$controlling)==0){
#         X <- paste(input$grouping2[1], sep="")
#     } else { X <- paste(input$grouping2[1], paste(input$controlling, collapse="+"), sep="+") }
#     
#     
#     if(rptm==T & is.null(input$grouping2)){
#         fmla <- as.formula(paste(Y, "~",
#                                  X, "+ (1|", input$traces, ")", sep=" "))
#     }
#     
#     if(rptm==F & is.null(input$grouping2)){
#         fmla <- as.formula(paste(Y, "~", X, sep=" "))
#     }
#     
#     if(rptm==T & !is.null(input$grouping2) & !is.null(input$controlling)){
#         #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, "+ (1|", input$traces, ")", sep=" "))
#         fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+", paste(input$controlling, collapse="+"), "+ (1|", input$traces, ")", sep=" "))
#     }
#     
#     if(rptm==T & !is.null(input$grouping2) & is.null(input$controlling)){
#         #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, "+ (1|", input$traces, ")", sep=" "))
#         fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+ (1|", input$traces, ")", sep=" "))
#     }
#     
#     if(rptm==F & !is.null(input$grouping2) & !is.null(input$controlling)){
#         #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, sep=" "))
#         fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "+", paste(input$controlling, collapse="+"),  sep=" "))
#     }
#     if(rptm==F & !is.null(input$grouping2) & is.null(input$controlling)){
#         #fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, sep=" "))
#         fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"),  sep=" "))
#     }
#     fmla
# }

output$pairsummary <- renderPrint({
    #df <- filedata()
    df <- get_subsetGLMM()
    if (is.null(df)) return(NULL)
    
    #fmla <- as.formula("MELDXI ~ Platelets")
    fmla <- create_formula()
    #print(formula)
    #if(class(df[,input$dependent2])=="numeric" & input$rptmeas==T){
    if(class(df[,input$dependent2])=="numeric" & input$randomeffects!=""){
        mod <- do.call(lmer, list(formula=fmla, data=df))
        #mod <- lmer(formula=fmla, data=df, REML=T)
        #mod <- lmer(fmla, data=df)
        print("LINEAR MIXED EFFECT-MODEL")
        print(anova(mod))
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print(".")
#         print("SUMMARY BELOW")
        #print(summary(mod))
        coefs <- data.frame(coef(summary(mod)))
        # use normal distribution to approximate p-value
        #coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
        print(coefs)
    }
    #if(class(df[,input$dependent2])=="numeric" & input$rptmeas==F){
    if(class(df[,input$dependent2])=="numeric" & input$randomeffects==""){
        mod <- lm(fmla, data=df)
        print("LINEAR MODEL")
        print(anova(mod))
        print(summary(mod))
#         if(class(df[,input$grouping2[1]])=="factor"){
#             kw <- kruskal.test(df[,input$dependent2],df[,input$grouping2[1]])
#             posthoc <- pairwise.t.test(df[,input$dependent2], df[,input$grouping2[1]], paired=F, p.adjust="BH")
#             pwt <- pairwise.wilcox.test(df[,input$dependent2], df[,input$grouping2[1]], p.adjust.method="BH", paired=F, exact=F, correct=T)
#             bt <- bartlett.test(df[,input$dependent2],df[,input$grouping2[1]], data=df)
#             ft <- fligner.test(df[,input$dependent2],df[,input$grouping2[1]], data=df)
#             print(kw)
#             print(posthoc)
#             print(pwt)
#             print(bt)
#             print(ft)
#        }
    }
    
    if(class(df[,input$dependent2])=="factor" & class(df[,input$grouping2[1]])=="numeric" & input$randomeffects!=""){
    #if(class(df[,input$dependent2])=="factor" & class(df[,input$grouping2[1]])=="numeric" & input$rptmeas==T){
        mod <- do.call(glmer, list(formula=fmla, data=df, family=binomial(link="logit")))
        #mod <- glmer(fmla, data=df, family=binomial(link="logit"), REML=F)
        #mod <- glmer(fmla, data=df, family=binomial(link="logit"))
        print("GENERALIZED LINEAR MIXED-EFFECT MODEL")
        print(anova(mod))
        print("SUMMARY BELOW")
        #print(summary(mod))
        coefs <- data.frame(coef(summary(mod)))
        # use normal distribution to approximate p-value
        #coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
        print(coefs)
    }
    if(class(df[,input$dependent2])=="factor" & class(df[,input$grouping2[1]])=="numeric" & input$randomeffects==""){
    #if(class(df[,input$dependent2])=="factor" & class(df[,input$grouping2[1]])=="numeric" & input$rptmeas==F){
        mod <- glm(fmla, data=df, family=binomial(link="logit"))
        print("GENERALIZED LINEAR MODEL")
        print(anova(mod))
        print(summary(mod))
    }
    
    if(class(df[,input$dependent2])=="factor" & class(df[,input$grouping2[1]])=="factor" & length(input$grouping2)==1){
        xi2 <- chisq.test(df[,input$dependent2],df[,input$grouping2[1]])
        print(xi2)
        fish <- fisher.test(df[,input$dependent2],df[,input$grouping2[1]])
        print(fish)
    }
    
})

