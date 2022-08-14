####  Data Science Master Seminar SS21               
####  An Introduction to Extreme Gradient Boosting   
library(dplyr)
library(Matrix)
library(xgboost)
library(caret)
library(ggplot2)
library(Ckmeans.1d.dp)
library(C50)
library(randomForest)
library(gbm)
library(tibble)
library(doParallel)
library(rbenchmark)
library(htmlTable)

### Import Dataset and Preprocessing
C2C_data <- read.csv("cell2celltrain.csv") # only use this part of the dataset, since test data has no labels
C2C_data[sapply(C2C_data, is.character)] <- lapply(C2C_data[sapply(C2C_data, is.character)], as.factor)
C2C_data$CustomerID <- NULL

# Descriptiv Statistics
summary(C2C_data)

na_in_row <- apply(C2C_data, 1, function(x) sum(is.na(x)))
sum(na_in_row >= 1)  

# Splitting in Training and Test Data
set.seed(123)
index <- sample(1:nrow(C2C_data), round(nrow(C2C_data) * 0.7))
train <- C2C_data[index, ]
test  <- C2C_data[-index, ]

# One-hot-encoding
options(na.action='na.pass')

X_train <- sparse.model.matrix(Churn ~ . - 1, data = train)
Y_train <- as.numeric(train$Churn == "Yes")

X_test <- sparse.model.matrix(Churn ~ . - 1, data = test)
Y_test <- as.numeric(test$Churn == "Yes")

### Basic xgboost model
# training
set.seed(123)
xgb_basic <- xgboost(data = X_train, label = Y_train, nrounds = 1000, objective = "binary:logistic")

# testing
xgbpred <- predict (xgb_basic, X_test)
xgbpred <- ifelse (xgbpred > 0.5,1,0)

# evaluation
xgb_basic_results <- confusionMatrix(table(xgbpred, Y_test), positive = "1")
#-> Accuracy: 70.17%


### Hyper parameter tuning

## Choose Learning rate
# create grid search
hyper_grid1 <- expand.grid(
  eta = c(0.3, 0.1, 0.05, 0.01),
  logloss = NA,
  trees = NA)

# execute grid search
for(i in seq_len(nrow(hyper_grid1))) {
  set.seed(123)  # for reproducibility
      xgb <- xgb.cv(
      data = X_train,
      label = Y_train,
      objective = "binary:logistic",
      nrounds = 1000, 
      early_stopping_rounds = 5,
      eta = hyper_grid1$eta[i], 
      nfold = 5,
      nthread = 6)

  # add logloss and trees to results
  hyper_grid1$logloss[i]  <- min(xgb$evaluation_log$test_logloss_mean)
  hyper_grid1$trees[i] <- xgb$best_iteration
}

# results
arrange(hyper_grid1, logloss)
#-> eta = 0.05 provides best results

## Choose tree specific hyperparameters
hyper_grid2 <- expand.grid(
  eta = 0.05,
  max_depth = c(3:10), 
  min_child_weight = c(1:10),
  logloss = NA,
  trees = NA)

for(i in seq_len(nrow(hyper_grid2))) {
  
  set.seed(123)  
  xgb <- xgb.cv(
    data = X_train,
    label = Y_train,
    objective = "binary:logistic",
    nrounds = 1000,
    early_stopping_rounds = 5,
    eta = hyper_grid2$eta[i], 
    max.depth = hyper_grid2$max_depth[i],
    min_child_weight = hyper_grid2$min_child_weight[i],
    nfold = 5,
    nthread = 6
  )
  
  hyper_grid2$logloss[i]  <- min(xgb$evaluation_log$test_logloss_mean)
  hyper_grid2$trees[i] <- xgb$best_iteration
}

head(arrange(hyper_grid2, logloss))
#-> max_depth = 4 & min_child_weight = 5

## Explore stochastic GBM attributes
hyper_grid3 <- expand.grid(
  eta = 0.05,
  max_depth = 4, 
  min_child_weight = 5,
  subsample = c(0.5, 0.75, 1), 
  colsample_bytree = c(0.5, 0.75, 1),
  logloss = NA,
  trees = NA)

for(i in seq_len(nrow(hyper_grid3))) {
  
  set.seed(123)  
  xgb <- xgb.cv(
    data = X_train,
    label = Y_train,
    objective = "binary:logistic",
    nrounds = 1000,
    early_stopping_rounds = 5,
    eta = hyper_grid3$eta[i], 
    max.depth = hyper_grid3$max_depth[i],
    min_child_weight = hyper_grid3$min_child_weight[i],
    subsample = hyper_grid3$subsample[i],
    colsample_bytree = hyper_grid3$colsample_bytree[i],
    nfold = 5,
    nthread = 6
  )
  
  hyper_grid3$logloss[i]  <- min(xgb$evaluation_log$test_logloss_mean)
  hyper_grid3$trees[i] <- xgb$best_iteration
}

head(arrange(hyper_grid3, logloss))
#-> subsample = 0.75, colsample_bytree = 0.75

## Overfitting parameters with random search
# choose hyperparameter sets with randomized values for gamma, lambda and alpha
parameters_list = list()
set.seed(23)
for (i in 1:50){
  parameters <- data.frame(eta = 0.05,
                max_depth = 4, 
                min_child_weight = 5,
                subsample = 0.75, 
                colsample_bytree = 0.75,
                gamma = sample(c(0, 1, 10, 100), 1),
                lambda = sample(c(0, 0.1, 1, 10, 100, 1000), 1),
                alpha = sample(c(0, 0.1, 1, 10, 100, 1000), 1),
                logloss = NA,
                trees = NA
  )
    parameters_list[[i]] <- parameters
}

# create object that contains all randomly created hyperparameters
hyper_grid4 = do.call(rbind, parameters_list)

for(i in seq_len(nrow(hyper_grid4))) {
  
  set.seed(123)  
  xgb <- xgb.cv(
    data = X_train,
    label = Y_train,
    objective = "binary:logistic",
    nrounds = 1000,
    early_stopping_rounds = 5,
    eta = hyper_grid4$eta[i], 
    max.depth = hyper_grid4$max_depth[i],
    min_child_weight = hyper_grid4$min_child_weight[i],
    subsample = hyper_grid4$subsample[i],
    colsample_bytree = hyper_grid4$colsample_bytree[i],
    gamma = hyper_grid4$gamma[i],
    lambda = hyper_grid4$lambda[i],
    alpha = hyper_grid4$alpha[i],
    nfold = 5,
    nthread = 6)
  
  hyper_grid4$logloss[i]  <- min(xgb$evaluation_log$test_logloss_mean)
  hyper_grid4$trees[i] <- xgb$best_iteration
}

head(arrange(hyper_grid4, logloss))
#-> gamma = 1, lambda = 0.1, alpha = 10, trees = 326

## Final XGBoost Model
# training
set.seed(123)
xgb_final <- xgboost(
  data = X_train,
  label = Y_train,
  objective = "binary:logistic",
  nrounds = 326,
  eta = 0.05, 
  max.depth = 4,
  min_child_weight = 5,
  subsample = 0.75,
  colsample_bytree = 0.75,
  gamma = 1,
  lambda = 0.1,
  alpha = 10
)
# testing
xgb_final_pred <- predict (xgb_final, X_test)
xgb_final_pred <- ifelse (xgb_final_pred > 0.5,1,0)

# evaluation
xgb_final_results <- confusionMatrix (table(xgb_final_pred, Y_test), positive = "1")
#-> Accuracy 72.26%

### Feature Importance
importance <- xgb.importance(feature_names = colnames(X_test), 
                             model = xgb_final)
head(importance)

tiff("importance.tiff", units="in", width=6, height=3, res=400)

xgb.ggplot.importance(importance, top_n = 10)
          
dev.off()            

### Performance comparison to other models
## C5.0 

control <- trainControl(method = "cv", number = 5, verboseIter= T)
set.seed(123)
C5.0 <- train(Churn ~., data  = train, trControl=control, method="C5.0", tuneGrid = data.frame(winnow = c(TRUE,FALSE), trials=1, model="tree"), na.action = na.pass)

C5.0_pred <- predict(C5.0, test, na.action = na.pass)

#confusion matrix
C5.0_results <- confusionMatrix (table(C5.0_pred, test$Churn), positive = "Yes")

## Random Forests
# Preprocessing (rf cannot deal with missing values)
train_clean <- na.omit(train)
test_clean <- na.omit(test)

# set up additional threads
registerDoParallel(cores = 6)

# tuning with caret
control <- trainControl(method='cv', 
                        number=5,
                        search = 'random',
                        verboseIter = T)

set.seed(123)
rf_random <- train(Churn ~ .,
                   data = train_clean,
                   method = 'rf',
                   tuneLength  = 15, 
                   trControl = control,
                   verbose = T)
print(rf_random)
#-> best mtry = 229 (categorical data is automatically encoded as dummies, thus higher mtry values)

stopImplicitCluster()

# Testing & Evaluation
rf_pred <- predict(rf_random, newdata = test_clean)
rf_results <- confusionMatrix(table(rf_pred, test_clean$Churn), positive = "Yes")

## GBM
# tuning with caret
control <- trainControl(method='cv', 
                        number=5, 
                        verboseIter= T)
# create grid search
set.seed(123)
hyper_grid_gbm <- expand.grid(n.trees = 2:6*100,
                      interaction.depth = 3:10,
                      shrinkage = c(0.3, 0.2, 0.1, 0.05),
                      n.minobsinnode = 1:10)
hyper_grid_gbm <- hyper_grid_gbm[sample(1:nrow(hyper_grid_gbm), 50),]

gbm_random <- train(x = train_clean[, -1], y = train_clean$Churn, 
                 method = "gbm", 
                 trControl = control, 
                 tuneGrid = hyper_grid_gbm)

gbm_random$bestTune
# -> n.trees = 400, interaction.depth = 5, shrinkage = 0.05, n.minobsinnode = 8

# Testing & Evaluation
gbm_pred <- predict(gbm_random, test_clean)
gbm_results <- confusionMatrix(table(gbm_pred, test_clean$Churn), positive = "Yes")

## Comparing Performance
results_compared <- list(C5.0 = C5.0_results,
                         gbm = gbm_results,
                         rf = rf_results,
                         xgb = xgb_final_results)

results_compared <- sapply(results_compared, function(x){cbind(x$overall[1:2], x$byClass[5:6])})

results_compared <- results_compared %>%
  t() %>%
  as.data.frame(row.names = F) %>%
  mutate(Model = c("C5.0", "gbm", "rf", "xgb")) %>%
  relocate(Model) 

colnames(results_compared) <- c("Model", "Accuracy", "Kappa", "Precision", "Recall")

# Visualizing Performance
benchmark <- sum(C2C_data$Churn == "No") / nrow(C2C_data)

tiff("accuracies.tiff", units="in", width=5, height=3, res=400)

results_compared %>% 
  ggplot() +
  geom_bar(aes(x = Model, y = Accuracy, fill = Model), stat="identity", width = 0.2) +
  geom_hline(yintercept = benchmark, col = "black") +
  scale_y_continuous(limits=c(0.6,0.75) , oob = scales::oob_keep) +
  ggtitle("Model accuracies compared to the benchmark") +
  theme(plot.title = element_text(hjust = 0.5))

dev.off()

# Table output
results_compared %>% 
  round(digit = 3) %>%
  addHtmlTableStyle(css.cell = c("width: 100;")) %>%
  htmlTable()

### Compare running time 
## mearuntime of different models (for 1 thread)
set.seed(123)
control <- trainControl(method = "none")
runtime_compared <- benchmark(
"C5.0" = {
  C5.0 <- train(Churn ~., data  = train, 
                trControl=control, 
                method="C5.0", 
                tuneGrid = data.frame(winnow = T, trials=1, model="tree"),
                na.action = na.pass,
                verbose = F)
},
"rf" = {
  train(Churn ~ .,
        data = train_clean,
        method = 'rf',
        trControl = control,
        verbose = F,
        tuneGrid = data.frame(mtry = 227))
},
"gbm" = {
  train(x = train_clean[, -1], y = train_clean$Churn, 
        method = "gbm", 
        trControl = control,
        distribution = "bernoulli",
        verbose = F,
        tuneGrid = data.frame(n.trees = 400, interaction.depth = 5, 
                              shrinkage = 0.05, n.minobsinnode = 8))
},
"xgboost" = {
  xgboost(
    data = X_train,
    label = Y_train,
    objective = "binary:logistic",
    nrounds = 326,
    eta = 0.05, 
    max.depth = 4,
    min_child_weight = 5,
    subsample = 0.75,
    colsample_bytree = 0.75,
    gamma = 1,
    lambda = 0.1,
    alpha = 10,
    nthread = 1,
    verbose = F)
},
replications = 1,
columns = c("test", "elapsed",
            "relative", "user.self", "sys.self"))
names(runtime_compared)[names(runtime_compared) == "test"] <- "Model"

runtime_compared <- arrange(runtime_compared, by = desc(elapsed))

# output as HTML table
runtime_compared[,2:5] <- round(runtime_compared[,2:5], 2)
run.time %>% 
  addHtmlTableStyle(css.cell = c("width: 100;")) %>%
  htmlTable(rnames = F)

### Runtime of xgboost for different threads 
# measuring runtimes
parallel_runtime <- c()
for (i in 1:10) {
start.time<-proc.time()
set.seed(123)
xgboost(
  data = X_train,
  label = Y_train,
  objective = "binary:logistic",
  nrounds = 326,
  eta = 0.05, 
  max.depth = 4,
  min_child_weight = 5,
  subsample = 0.75,
  colsample_bytree = 0.75,
  gamma = 1,
  lambda = 0.1,
  alpha = 10,
  nthread = i,
  verbose = F)

stop.time <- proc.time()

run.time <- stop.time - start.time
parallel_runtime <- rbind(parallel_runtime, run.time)
}

# visualize
tiff("test.tiff", units="in", width=5, height=3, res=400)

parallel_runtime %>% as.data.frame() %>% mutate(threads = c(1:10)) %>%
  ggplot(aes(x = threads, y = elapsed)) +
  geom_bar(stat = "identity", width = 0.3) +
  scale_x_continuous(breaks = c(1:10)) +
  
  ggtitle("xgboost runtimes on multiple threads") +
  theme(plot.title = element_text(hjust = 0.5)) 
  
dev.off()
