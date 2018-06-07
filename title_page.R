# title page for HiD

title_page <-tabPanel("About",
    
    mainPanel(
    h1("Hi-D MultiLevel Modeler:", align="center"),
    h2("An integrative model of immune biology and organ dysfunction", align = "center"),
    h4("Nicholas Wisniewski", align = "center"),
    h6("University of California, Los Angeles", align = "center"),
    hr(),
    h4("Description"),
    p("This application provides an interface for exploring the systems genetics of multiple organ dysfunction associated with MCSD surgery in advanced heart failure patients. Select the tabs across the top in order to initiate the associated computations. Each tab will automatically bring up the control panel relevant to that tab."),
    hr(),
    h4("Dataset"),
    p("The dataset includes 22 consecutive heart failure patients undergoing MCS surgery, sampled at 5 timepoints: day -1 preoperative, and days 1, 3, 5, and 8 postoperative. Phenotyping was performed using 12 clinical parameters (Bilirubin, Creatinine, Glucose, etc), 2 organ dysfunction scoring systems (MELD-XI and SOFA), and survival outcomes. Eigengenes were computed from whole-genome mRNA expression in peripheral blood mononuclear cells, using weighted gene co-expression network analysis (WGCNA). Eigengenes were named according to biological function using gene ontology, pathway, and transcription factor binding site enrichment analyses. Additionally, immunophenotyping panels T1 (innate), T3 (T cell), T7 (B cell) and Luminex cytokines were measured during the same period. "),
    hr(),
    h4("Related article"),
    a("N. Wisniewski, G. Bondar, C. Rau, J. Chittoor, E. Chang, A. Esmaeili, M. Deng. An integrative model of leukocyte genomics and organ dysfunction in heart failure patients requiring mechanical circulatory support. bioRxiv doi: 10.1101/024646, 2015. ", href="http://biorxiv.org/content/early/2015/08/14/024646"),
    hr(),
    tags$div(
        HTML("<strong>Copyright &copy 2016 The Regents of the University of California </strong>"),
        HTML("<br>"),
        HTML("<strong>All Rights Reserved </strong>"),
        HTML("<br>"),
        HTML("<strong>Created by Nicholas Wisniewski </strong>")
    )
    #h4("Dataset"),
    #p("The dataset includes box scores"),
    #hr(),
    #h4("Full article"),
    #a("N. Wisniewski, G. Bondar, C. Rau, J. Chittoor, E. Chang, A. Esmaeili, M. Deng. An integrative model of leukocyte genomics and organ dysfunction in heart failure patients requiring mechanical circulatory support. bioRxiv doi: 10.1101/024646, 2015. ", href="http://biorxiv.org/content/early/2015/08/14/024646")
)
)