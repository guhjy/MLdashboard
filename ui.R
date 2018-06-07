library(markdown)
library(shinythemes)
library(pastecs)

source("title_page.R", local=TRUE)
source("ui panels.R", local=TRUE)
#source("import_panel.R", local=TRUE)
#source("save_panel.R", local=TRUE)
#source("unsupervised_panel.R", local=TRUE)

shinyUI(navbarPage("Hi-D MultiLevel Modeler", inverse=T, theme = shinytheme("flatly"),
                   whole_panel,
                   plots_panel,
                   navbarMenu("Data", import_panel, pred_panel
                              #save_panel
                              ),
                   title_page
))

