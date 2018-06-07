

methods.master = rbind(
    #c("lm", "Linear Regression"),
    c("glm","GLM (standard)"),
    c("pls", "GLM (PLS)"),
    #c("LogitBoost", "Logistic (boosted)"),
    c("glmnet", "GLM (elastic net)"),
    #c('bayesglm',"Logistic (Bayesian)"), 
    #c('logicBag',"Logistic (bagged)"),
    c("rpart", "Decision Tree"),
    #c("treebag", "Decision Tree (bagged)"),
    c("gbm", "Decision Tree (boosted)"),
    #c("ada", "Decision Tree (boosted)"),
    #c("AdaBag","Decision Tree (bagged boosted)"),
    c("rf", "Random Forest"),
    #c('RRFglobal', "Regularized Random Forest"),
    #c('ORFsvm' ,"Oblique Random Forest (SVM)"),
    #c('rFerns',"Random Ferns"),
    #c('C5.0' ,"C50"),
    #c('OneR' ,"Single Rule Classification"),
    #c('GFS.GCCL' ,"Fuzzy Rules Using Genetic Cooperative-Competitive Learning"),
    #c('SLAVE' ,"Fuzzy Rules Using the Structural Learning Algorithm on Vague Environment"),
    #c('xgbLinear',"eXtreme Gradient Boosting (Linear)"),
    #c('xgbTree' ,"eXtreme Gradient Boosting (CART)"),
    c('lda' ,"LDA"),
    #c('qda' ,"Quadratic Discriminant Analysis"),
    #c('pda' ,"L1 Penalized Discriminant Analysis"),
    #c('hdda',"High Dimensional Discriminant Analysis"),
    #c('amdai',"Adaptive Mixture Discriminant Analysis"),
    #c('bstSm',"Smoothing Spline (boosted)"),
    c("svmLinear", "SVM (linear)"),
    #c("svmPoly", "SVM (polynomial)"),
    c("svmRadial", "SVM (radial)"),
    #c('svmSpectrumString' ,"SVM (Spectrum String Kernel)"),
    #c('lssvmLinear' ,"Least Squares SVM (linear)"),
    #c("lssvmPoly","Least Squares SVM (polynomial)"),
    #c('lssvmRadial' ,"Least Squares SVM (radial)"),
    #c('rvmLinear',"Relevance Vector Machine (linear)"),
    #c('rvmPoly' ,"Relevance Vector Machine (polynomial)"),
    #c('rvmRadial',"Relevance Vector Machine (radial)"),   
    #c('vbmpRadial',"Variational Bayesian Probit Regression"),
    #c('brnn' ,"Bayesian Regularized Neural Network"),
    c("nnet","Neural Network"),
    #c('avNNet' ,"Model Averaged Neural Network"),
    #c('mlpWeightDecay' ,"MultiLayer Perceptron"),
    #c('rbf',"Radial Basis Function Network"),
    #c('gaussprLinear' ,"Gaussian Process (linear)"),
    #c('gaussprPoly',"Gaussian Process (polynomial)"),
    #c('gaussprRadial' ,"Gaussian Process (radial)"),
    
    c("xyf", "Self Organizing Map"),
    c("knn","k-Nearest Neighbors")
    #c("nb", "Naive Bayes")
)

methods.master  # should write this out to .csv
methods <- methods.master[,1]
modelnames <- methods.master[,2]
