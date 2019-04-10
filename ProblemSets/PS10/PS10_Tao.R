library(rpart)
library(e1071)
library(kknn)
library(nnet)
library(mlr)
library(tidyverse)
library(magrittr)

set.seed(100)

income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names)
#   age: continuous.
#   workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked.
#   fnlwgt: continuous.
#   education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
#   education-num: continuous.
#   marital-status: Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse.
#   occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
#   relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
#   race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
#   sex: Female, Male.
#   capital-gain: continuous.
#   capital-loss: continuous.
#   hours-per-week: continuous.
#   native-country: United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands.

######################
# Clean up the data
######################
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL
# Make sure continuous variables are coded as such
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)
# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]

#5 
# classification task
traintask <- makeClassifTask(data = income.train,target = "high.earner")
testtask <- makeClassifTask(data = income.test, target = "high.earner")

#The 3-fold cross-validation strategy
set.cv <- makeResampleDesc("CV",iters = 3L)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)
#The tuning strategy
tuneMethod <- makeTuneControlRandom(maxit=10L)

######################
#Learners
######################

# trees
trees <- makeLearner("classif.rpart", predict.type = "response")

# Logistic regression
logit <- makeLearner("classif.glmnet",predict.type = "response")

# Neural network: classif.nnet
neural <- makeLearner("classif.nnet", predict.type = "response")

# Naive Bayes: classif.naiveBayes
Naive.Bayes <- makeLearner("classif.naiveBayes", predict.type = "response")

# kNN: classif.kknn
knnlearner <- makeLearner("classif.kknn", predict.type = "response")

# SVM: classif.svm
svm <- makeLearner("classif.svm", predict.type = "response")

######################
#6 hyperparameters
######################

# tree model
tree_model <- makeParamSet( makeIntegerParam("minsplit",lower=10,upper=50),makeIntegerParam(
  "minbucket",lower=5,upper=50),makeNumericParam("cp", lower=0.001,upper=0.2))

# Logit model
logit_model <- makeParamSet(makeNumericParam("lambda",lower=0,upper=3),
                            makeNumericParam("alpha",lower=0,upper=1))
#Neural network model
neural_networl_model <- makeParamSet(makeIntegerParam("size" ,lower=1,upper=10),
                                     makeNumericParam("decay",lower=0.1,upper=0.5), makeIntegerParam("maxit",lower=1000,upper=1000))
#kNN
knn <- makeParamSet(makeIntegerParam("k",lower=1,upper=30))

#SVM
SVM <- makeParamSet(makeDiscreteParam("kernel", values = "radial"),
                    makeDiscreteParam("cost", values = 2^c(-2,-1,0, 1,2,10)),
                    makeDiscreteParam("gamma", values = 2^c(-2,-1,0, 1,2,10)))

######################
#7 TUNE THE MODELS 
######################

#tree model
tune_trees <- tuneParams(learner = trees,task = traintask,
                         resamping = set.cv,
                         par.set = tree_model, 
                         control = tuneMethod, 
                         measures=list(f1, gmean))
#Logit
tune_logit <- tuneParams(learner = logit,
                         task = traintask,
                         resampling = set.cv,
                         measures=list(f1, gmean),      
                         par.set = logit_model,
                         control = tuneMethod,
                         show.info = TRUE)



#Neural Network
tune_neural <- tuneParams(learner = neural,
                          task = traintask,
                          resampling = set.cv,
                          measures=list(f1, gmean),      
                          par.set = neural_networl_model,
                          control = tuneMethod,
                          show.info = TRUE)


#KNN
tune_KNN <- tuneParams(learner = knnlearner,
                       task = traintask,
                       resampling = set.cv,
                       measures=list(f1, gmean),      
                       par.set = knn,
                       control = tuneMethod,
                       show.info = TRUE)



#SVM
tune_SVM <- tuneParams(learner = svm,
                       task = traintask,
                       resampling = set.cv,
                       measures=list(f1, gmean),      
                       par.set = SVM,
                       control = tuneMethod,
                       show.info = TRUE)

#the optimal tuning parameters
parameters_tree <- setHyperPars(learner=trees, par.vals = tune_trees$x)
parameters_logit <- setHyperPars(learner=logit, par.vals = tune_logit$x)
parameters_neural <- setHyperPars(learner=neural, par.vals = tune_neural$x)
parameters_knn <- setHyperPars(learner=knnlearner, par.vals = tune_KNN$x)
parameters_svm <- setHyperPars(learner=svm, par.vals = tune_SVM$x)

# Verify performance
resample(parameters_tree,traintask,resampleStrat,measures=list(f1, gmean))
resample(parameters_logit,traintask,resampleStrat,measures=list(f1, gmean))
resample(parameters_neural,traintask,resampleStrat,measures=list(f1, gmean))
resample(parameters_knn,traintask,resampleStrat,measures=list(f1, gmean))
resample(parameters_svm,traintask,resampleStrat,measures=list(f1, gmean))

# Train the final model
finalmodel_tree <- train(learner = parameters_tree, task = traintask)
finalmodel_logit <- train(learner = parameters_logit, task = traintask)
finalmodel_neural <- train(learner = parameters_neural, task = traintask)
finalmodel_knn <- train(learner = parameters_knn, task = traintask)
finalmodel_svm <- train(learner = parameters_svm, task = traintask)

# Predict in test set
prediction_tree <- predict(finalmodel_tree, newdata = income.test)
prediction_logit <- predict(finalmodel_logit, newdata = income.test)
prediction_neural <- predict(finalmodel_neural, newdata = income.test)
prediction_knn <- predict(finalmodel_knn, newdata = income.test)
prediction_svm <- predict(finalmodel_svm, newdata = income.test) 

# print table
print(tune_trees)
print(tune_logit)
print(tune_neural)
print(tune_KNN)
print(tune_SVM)
