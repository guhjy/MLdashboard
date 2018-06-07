library(ggplot2)
#library(lme4)
library(lmerTest)

output$dependent2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    selectInput("dependent2","Select response variable Y:", items, selected="MELDXI", selectize=T)
    #}
    #else return(NULL)
})

output$independent2 <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    selectInput("independent2","Select predictor A:", items, selected="Timepoint", selectize=T)
    #}
    #else return(NULL)
})

output$plottype <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){

    selectInput("plottype","Select plot type:", c("Box plot", "Line plot", "Spine plot"), selected="Box plot", selectize=T)
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
    selectInput("grouping2","Select interaction variables B:", c(items), multiple=T, selected="Survivor", selectize=T)
    #}
    #else return(NULL)
})

output$traces <- renderUI({
    df <- filedata()
    if (is.null(df) | input$rptmeas==F) return(NULL)
    #if (analysis() %in% c("Modeling", "Bayesian Network")){
    items=names(df)
    names(items)=items
    selectInput("traces","Select repeated measure ID:", c("none",items), selected="PatientID", selectize=T)
    #}
    #else return(NULL)
})

output$rptmeas <- renderUI({
    df <- filedata()
    if (is.null(df) ) return(NULL)
    checkboxInput("rptmeas", "Use GLMM for repeated measures", value=T, width='100%')
})

output$controlling <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)
    names(items)=items
    selectInput("controlling","Control for additional factors C:", c(items), multiple=TRUE, selected=NULL, selectize=T)
})




output$plotter <- renderPlot({
    df <- filedata()
    if (is.null(df) | is.null(input$rptmeas) | is.null(input$grouping2) | is.null(input$dependent2) | is.null(input$independent2) | is.null(input$traces)) return(NULL)
    indivtrace <- if(input$traces=="none"){NULL} else{input$traces} 
    facetgroup <- if(is.null(input$grouping2)){NULL} else{input$grouping[1]} 

    
    if (input$plottype=="Box plot"){
        if(is.null(input$grouping2)){
            pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2)) 
            p2 <- pl1  + geom_boxplot(notch=F) + geom_point(shape=16, position=position_jitter(width=.2,height=.5)) + theme_bw()
            p2 + scale_fill_brewer(palette="Spectral")
        } else { 
            pl1 <- ggplot(data = df, aes_string(x = input$grouping2, y = input$dependent2, fill=input$grouping2))
            p2 <- pl1   + geom_boxplot( notch=F) + geom_jitter(width=.2) + facet_grid(as.formula(paste(". ~", input$independent2)), scales="free")  + theme_bw()
            p2 + scale_fill_brewer(palette="Spectral")
        }
        
    } else if(input$plottype=="Line plot"){
        if(is.null(input$grouping2)){
            if(input$rptmeas==F){
                pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2))
                p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw()
                #p2 <- pl1 + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                p2 + scale_fill_brewer(palette="Spectral")
            } else{ 
                pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2, group = indivtrace))
                if(class(df[,input$independent2])=="factor"){
                    p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                        
                } else{
                    p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + geom_smooth() + theme_bw()
                        #stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw()
                       # geom_smooth() + theme_bw()
                }
                
                p2 + scale_fill_brewer(palette="Spectral")
            }
        } else {
            if(input$rptmeas==F){
                pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2, fill=input$grouping2))
                if(class(df[,input$independent2])=="factor"){
                    p2 <- pl1 +  geom_point(shape=16) + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                } else{
                    p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                }
                #pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2, fill=input$grouping2))
                #p2 <-  pl1 +  geom_point(shape=16) + geom_smooth() + theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                #p2 <- pl1  + stat_smooth()  +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                p2 + scale_fill_brewer(palette="Spectral")
            } else {
                pl1 <- ggplot(data = df, aes_string(x = input$independent2, y = input$dependent2, group = indivtrace, fill=input$grouping2))
                if(class(df[,input$independent2])=="factor"){
                    p2 <- pl1 +  geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1)) + stat_summary(aes(group = 1), geom = "point", fun.y = mean, shape = 16, size = 3) +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                } else{
                    p2 <- pl1 + geom_point(shape=16) + geom_line(colour="grey") + stat_smooth(aes_string(group = 1))  +  theme_bw() + facet_grid(as.formula(paste(". ~", input$grouping2)))
                }
                p2 + scale_fill_brewer(palette="Spectral")
            }
        }
       

    } else if(input$plottype=="Spine plot"){
#         pl1 <- ggplot(data=df, aes_string(x = input$independent2, y = input$dependent2, fill=input$grouping2, color=input$grouping2)) 
#         p2 <- pl1 + geom_bar(stat="identity", position=position_dodge()) 
       print( spineplot(factor(df[,input$dependent2])~factor(df[,input$independent2]), xlab=input$independent2, ylab=input$dependent2, col=rainbow(length(levels(df[,input$dependent2]))))  )
    }
    
    

  
})




create_formula <- function(){
    rptm <- input$rptmeas
    grp <- input$grouping2
    Y <- input$dependent2
    if(is.null(input$controlling)){
        X <- paste(input$independent2, sep="")
    } else { X <- paste(input$independent2, paste(input$controlling, collapse="+"), sep="+") }
    if(rptm==T & is.null(input$grouping2)){
        fmla <- as.formula(paste(Y, "~",
                                 X, "+ (1|", input$traces, ")", sep=" "))
    }
    if(rptm==F & is.null(input$grouping2)){
        fmla <- as.formula(paste(Y, "~", X, sep=" "))
    }
    if(rptm==T & !is.null(input$grouping2)){
        fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, "+ (1|", input$traces, ")", sep=" "))
    }
    if(rptm==F & !is.null(input$grouping2)){
        fmla <- as.formula(paste(Y, "~", paste(input$grouping2, collapse="*"), "*", X, sep=" "))
    }
    fmla
}

output$pairsummary <- renderPrint({
    df <- filedata()
    if (is.null(df)) return(NULL)
    
    fmla <- as.formula("MELDXI ~ Platelets")
    fmla <- create_formula()
    #print(formula)
    if(class(df[,input$dependent2])=="numeric" & input$rptmeas==T){
        mod <- do.call(lmer, list(formula=fmla, data=df))
        #mod <- lmer(fmla, data=df)
        print("LINEAR MIXED EFFECT-MODEL")
        print(anova(mod))
        print("SUMMARY BELOW")
        print(summary(mod))
    }
    if(class(df[,input$dependent2])=="numeric" & input$rptmeas==F){
        mod <- lm(fmla, data=df)
        print("LINEAR MODEL")
        print(anova(mod))
        print(summary(mod))
        if(class(df[,input$independent2])=="factor"){
            kw <- kruskal.test(df[,input$dependent2],df[,input$independent2])
            posthoc <- pairwise.t.test(df[,input$dependent2], df[,input$independent2], paired=F, p.adjust="BH")
            pwt <- pairwise.wilcox.test(df[,input$dependent2], df[,input$independent2], p.adjust.method="BH", paired=F, exact=F, correct=T)
            bt <- bartlett.test(df[,input$dependent2],df[,input$independent2], data=df)
            ft <- fligner.test(df[,input$dependent2],df[,input$independent2], data=df)
        }
        print(kw)
        print(posthoc)
        print(pwt)
        print(bt)
        print(ft)
    }
    
    if(class(df[,input$dependent2])=="factor" & class(df[,input$independent2])=="numeric" & input$rptmeas==T){
        mod <- do.call(glmer, list(formula=fmla, data=df, family=binomial(link="logit")))
        #mod <- glmer(fmla, data=df, family=binomial(link="logit"))
        print("GENERALIZED LINEAR MIXED-EFFECT MODEL")
        print(anova(mod))
        print("SUMMARY BELOW")
        print(summary(mod))
    }
    if(class(df[,input$dependent2])=="factor" & class(df[,input$independent2])=="numeric" & input$rptmeas==F){
        mod <- glm(fmla, data=df, family=binomial(link="logit"))
        print("GENERALIZED LINEAR MODEL")
        print(anova(mod))
        print(summary(mod))
    }
    
    if(class(df[,input$dependent2])=="factor" & class(df[,input$independent2])=="factor" ){
        xi2 <- chisq.test(df[,input$dependent2],df[,input$independent2])
        print(xi2)
        fish <- fisher.test(df[,input$dependent2],df[,input$independent2])
        print(fish)
    }
    
})

