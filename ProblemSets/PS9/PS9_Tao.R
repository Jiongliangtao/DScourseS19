library(mlr)
library(glmnet)
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula <- as.formula(lmedv ~ .^3 + 
                        poly(crim, 6) + 
                        poly(zn, 6) + 
                        poly(indus , 6) + 
                        poly(nox, 6) + 
                        poly(rm, 6) + 
                        poly(age, 6) + 
                        poly(dis, 6) + 
                        poly(rad, 6) + 
                        poly(tax, 6) + 
                        poly(ptratio , 6) + 
                        poly(b, 6) + 
                        poly(lstat , 6))
mod_matrix <- data.frame(model.matrix(formula, housing))
#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix[, 1] = housing$lmedv
colnames(mod_matrix)[1] = "lmedv" #make sure to rename it otherwise MLR won’t find it
head(mod_matrix) #just make sure everything is hunky-dory

# Break up the data:
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train ,]
housing.test <- mod_matrix[test, ]

dim(housing.train)

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 6 folds
print(sampleResults$aggr)




#LASSO


# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)
finalModel
# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))

out.of.sample.rmse <- sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(out.of.sample.rmse)


# Ridge


# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)
finalModel
# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))

out.of.sample.rmse2 <- sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(out.of.sample.rmse2)




# Elastic Net


# Search over penalty parameter lambda and force elastic net parameter to be weighted
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)
finalModel
# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))

out.of.sample.rmse3 <- sqrt(mean((prediction$data$truth-prediction$data$response)^2))
print(out.of.sample.rmse3)