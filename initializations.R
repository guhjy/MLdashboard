# Initializations

# screen height
disp29inch <- 1300
disp15inch <- 700
screenheight <- disp15inch

# General
na.pct <- .5  # percentage of missing data allowed in a column
subschk.init <- F  # take subset
subs.init <- "Timepoint"  # what's the subsetting variable
subsvar.init <- c(levels(csv$Timepoint)[c(1)])  # what are the levels of the subsetting variables

dependent.init <- "Survivor"  # what to predict
allvars.init <- F  # use all predictors
#predgroups.init <- uniquegroupnames[c(15,19, 8, 11)]  # use groups of predictors
predgroups.init <- c("clinical",  "eigengene", "cytokine" )
removed.init <- NULL  # remove predictors
independents.init <- NULL  # add predictors

# Clustering
aggbyvars.init <- c("Timepoint","Survivor")

# Machine Learning
trainmethod <- "LOOCV" # or "LOOCV" , what kind of cross validation for optimization
resamplechk.init <- TRUE

# PCA
which2pcs.init <- 1:2
whichpcs.init <- 1:2
alpha.init <- .6  # transparency of the points on the PCA plot

# GLM
subschkGLMM.init <- F  # take subset
subsGLMM.init <- "Timepoint"  # what's the subsetting variable
subsvarGLMM.init <- c(levels(csv$Timepoint)[c(2:5)])  # what are the levels of the subsetting variables
slopechk.init <- F

randomeffects.init <- c("PatientID")
dependent2.init <- "MELDXI"  # what to predict
grouping2.init <- c("Timepoint", "Survivor")  # interacting variables
controlling.init <-  c("Age", "Sex", "Ischemic")  # controlling variables
rptmeas.init <- T  # repeated measures?
traces.init <- "PatientID"  # what's the repeated measures variable
plottype.init <- "Line plot"  # what's the plot type
pointchk.init <- T  # show points on the plot? (slow for big data)