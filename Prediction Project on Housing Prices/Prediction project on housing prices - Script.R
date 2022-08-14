#### Predictive analytics project R-script

library(readr)
library(dplyr)
library(caret)
library(randomForest)
library(xgboost)
library(doParallel)
library(tidyr)
library(stargazer)
library(broom)
library(Matrix)

### Import data
data <- read_csv("data.csv")
data <- as.data.frame(data)
data <- subset(data, data$price != 0)

# Missing values
sum(is.na(data))

# Some feature engineering 
data$age <- 0
for (row in (1:nrow(data))) {
    data[row, "age"] <- 2014 - as.numeric(data[row, "yr_built"])
}

data$renovated <- as.numeric(data$yr_renovated != 0)
data$condition <- as.factor(data$condition)
data$view <- as.factor(data$view)
data <- data %>% select(price, bathrooms, bedrooms, condition, floors, sqft_living, sqft_lot, 
                        waterfront, view, age, renovated, statezip)


# Splitting
set.seed(123)
training.samples <- data$price %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

### Regression
# Graphical exploration
continous <- train.data %>% select(price, sqft_living, sqft_lot, age) 

ggplot(gather(continous), aes(value)) + 
  geom_histogram(bins = 25) + 
  facet_wrap(~key, scales = 'free_x')

continous <- train.data %>% mutate(log_price = log(price), log_living = log(sqft_living), log_lot= log(sqft_lot)) %>% select(age, log_price, log_living, log_lot) 

ggplot(gather(continous), aes(value)) + 
  geom_histogram(bins = 25) + 
  facet_wrap(~key, scales = 'free_x')

# WIP: small issue with the x scale
discrete <- train.data %>% select(bathrooms, bedrooms, condition, floors, waterfront, view, renovated) 

ggplot(gather(discrete), aes(value)) + 
  geom_bar() + 
  facet_wrap(~key, scales = 'free_x') 

# Scatter plots
scatter_df <- train.data %>% mutate(log_living = log(sqft_living), log_lot= log(sqft_lot))

for(i in 2:ncol(scatter_df)) {
  colname = colnames(scatter_df)[i]
  print(ggplot(scatter_df, aes(x = scatter_df[ , i], y = log(price))) +
    geom_point() + xlab(colname))
}

# Full model fitting
reg <- lm(log(price) ~ bathrooms + bedrooms + condition + floors + log(sqft_living) + log(sqft_lot) + waterfront + view + age + renovated + statezip, data = train.data)
summary(reg)
stargazer(reg, type = "html", out = "first_reg.html",  single.row = TRUE, omit = "statezip", omit.labels = "state code dummies?")


# Model diagnostics
par(mfrow=c(2,2))
plot(reg)

# outlier removal
Q <- quantile(log(train.data$price), probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(log(train.data$price))
up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range
train.data_clean <- subset(train.data, log(train.data$price) > low & log(train.data$price) < up)

hist(log(train.data_clean$price))

# 3643 --> 3577

# Run regression again
reg2 <- lm(log(price) ~ bathrooms + bedrooms + condition + floors + log(sqft_living) + log(sqft_lot) + waterfront + view + age +  renovated + statezip, data = train.data_clean)
summary(reg2)
stargazer(reg, reg2, type = "html", out = "second_reg.html",  single.row = TRUE, omit = "statezip", omit.labels = "state code dummies?")

par(mfrow=c(2,2))
plot(reg2)

train.data_clean <- train.data_clean[!(rownames(train.data_clean) == "4350"),]


## Identify potential top models
# Stepwise
reg3 <- lm(log(price) ~ bathrooms + bedrooms + condition + floors + log(sqft_living) + log(sqft_lot) + waterfront + view + age +  renovated + statezip, data = train.data_clean)
best_step <- step(reg3)
best_step <- "log(price) ~ bathrooms + bedrooms + condition + floors + waterfront + log(sqft_living) + log(sqft_lot) + view + statezip"

# best subset
predictors <- c("bedrooms","bathrooms","log(sqft_living)","log(sqft_lot)", "floors","waterfront", "view", "condition", "age", "renovated", "statezip")

out <- unlist(lapply(1:5, function(n) {
  combinations <- t(combn(predictors,n))
  formulas <- apply(combinations, 1, 
                    function(row) paste0("log(price) ~ ", paste0(row, collapse = "+")))}))

mods = lapply(out, function(frml) lm(frml, data=train.data_clean))

tmp = bind_rows(lapply(out, function(frml) {
  a = glance(lm(frml, data=train.data_clean))
  a$frml = frml
  return(a)
}))

tmp <- tmp %>% arrange(AIC)
head(tmp)

best_aic <- tmp$frml[1]


## Evaluate potential models
fitControl <- trainControl(method = "cv",
                           number = 5)

set.seed(123)


best_step_reg <- train(as.formula(best_step), data = train.data_clean, 
                       method = "lm", 
                       trControl = fitControl)

best_aic_reg <- train(as.formula(best_aic), data = train.data_clean, 
                 method = "lm", 
                 trControl = fitControl)

# select best step as final model
reg4 <- lm(best_step, data = train.data_clean)
summary(reg4)
stargazer(reg, reg2, reg4, type = "html", out = "final model.html",  single.row = TRUE, omit = "statezip", omit.labels = "state code dummies?")



## random forest
set.seed(123)
cl <- makePSOCKcluster(10)
registerDoParallel(cl)

control <- trainControl(method="repeatedcv", number=5, search="grid")
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(price~., data=train.data, method="rf", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
# --> best mtry = 11

stopImplicitCluster()


## boosting
dummy1 <- dummyVars(" ~ .", data=train.data)
newdata1 <- data.frame(predict(dummy1, newdata = train.data))
newdata1 <- newdata1[ , order(names(newdata1))]
X_train <- sparse.model.matrix(price ~ . - 1, data = newdata1)

dummy2 <- dummyVars(" ~ .", data=test.data)
newdata2 <- data.frame(predict(dummy2, newdata = test.data))

setdiff(train.data$statezip, test.data$statezip)
newdata2$statezipWA.98051 <- 0
newdata2$statezipWA.98068 <- 0
newdata2$statezipWA.98050 <- 0
newdata2$statezipWA.98354 <- 0
newdata2 <- newdata2[ , order(names(newdata2))]
X_test <- sparse.model.matrix(price ~ . - 1, data = newdata2)


# choose hyperparameter sets with randomized values 
parameters_list = list()
set.seed(123)
for (i in 1:1000){
  parameters <- data.frame(eta = runif(1, .01, .3),
                           max_depth = sample(c(2, 3, 4, 5), 1), 
                           min_child_weight = sample(0:10, 1),
                           subsample = runif(1, .7, 1),
                           colsample_bytree = runif(1, .6, 1),
                           gamma = sample(c(0, 1, 10, 100, 1000), 1),
                           lambda = sample(c(0, 0.1, 1, 10, 100, 1000), 1),
                           alpha = sample(c(0, 0.1, 1, 10, 100, 1000), 1),
                           rmse = NA,
                           trees = NA
  )
  parameters_list[[i]] <- parameters
}

# create object that contains all randomly created hyperparameters
hyper_grid = do.call(rbind, parameters_list)

for(i in seq_len(nrow(hyper_grid))) {
  set.seed(123)  
  xgb <- xgb.cv(
    data = X_train,
    label = train.data$price,
    nrounds = 500,
    early_stopping_rounds = 10,
    eta = hyper_grid$eta[i], 
    max.depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i],
    gamma = hyper_grid$gamma[i],
    lambda = hyper_grid$lambda[i],
    alpha = hyper_grid$alpha[i],
    nfold = 5,
    nthread = 24,
    verbose =F)
  
  hyper_grid$rmse[i]  <- min(xgb$evaluation_log$test_rmse_mean)
  hyper_grid$trees[i] <- xgb$best_iteration
}

head(arrange(hyper_grid, rmse))
best_tune = 173

xgb_final <- xgboost(
  data = X_train,
  label = train.data$price,
  nrounds = hyper_grid$trees[best_tune],
  eta = hyper_grid$eta[best_tune], 
  max.depth = hyper_grid$max_depth[best_tune],
  min_child_weight = hyper_grid$min_child_weight[best_tune],
  subsample = hyper_grid$subsample[best_tune],
  colsample_bytree = hyper_grid$colsample_bytree[best_tune],
  gamma = hyper_grid$gamma[best_tune],
  lambda = hyper_grid$lambda[best_tune],
  alpha = hyper_grid$alpha[best_tune]
)

## testing
reg.pred <- predict(reg4, newdata = test.data)
reg.res <- test.data$price - exp(reg.pred)
reg.rmse <- sqrt(mean((reg.res)^2))
reg.mae <- mean(abs(reg.res))
plot(exp(reg.pred), reg.res)

rf_final <- randomForest(price ~., data = train.data, mtry = 11)
rf.pred <- predict(rf_final, test.data)
rf.res <- test.data$price - rf.pred
rf.rmse <- sqrt(mean((rf.res)^2))
rf.mae <- mean(abs(rf.res))
plot(rf.pred, rf.res)

xgb_final_pred <- predict (xgb_final, X_test)
xgb.res <- test.data$price - xgb_final_pred
xgb.rmse <- sqrt(mean((xgb.res)^2))
xgb.mae <- mean(abs(xgb.res))
plot(xgb_final_pred, xgb.res)

## Performance Evaluation
model_rmse <- data.frame(c("Regression", "Random Forest", "Boosting"),c(reg.rmse, rf.rmse, xgb.rmse))
colnames(model_rmse) <- c("Model", "RMSE")
model_rmse$Model <- factor(model_rmse$Model, levels=unique(model_rmse$Model))

model_mae <- data.frame(c("Regression", "Random Forest", "Boosting"),c(reg.mae, rf.mae, xgb.mae))
colnames(model_mae) <- c("Model", "MAE")
model_mae$Model <- factor(model_mae$Model, levels=unique(model_mae$Model))

# Visualization
model_rmse %>% 
  ggplot() +
  geom_bar(aes(x = Model, y = RMSE, fill = Model), stat="identity", width = 0.2) +
  scale_y_continuous(oob = scales::oob_keep) +
  scale_x_discrete() +
  ggtitle("Model accuracies") +
  theme(plot.title = element_text(hjust = 0.5))

model_mae %>% 
  ggplot() +
  geom_bar(aes(x = Model, y = MAE, fill = Model), stat="identity", width = 0.2) +
  scale_y_continuous( oob = scales::oob_keep) +
  scale_x_discrete() +
  ggtitle("Model accuracies") +
  theme(plot.title = element_text(hjust = 0.5))

# Fit final model on all data
Q <- quantile(log(data$price), probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(log(data$price))
up <-  Q[2]+1.5*iqr # Upper Range  
low <- Q[1]-1.5*iqr # Lower Range
all.data <- subset(data, log(data$price) > low & log(data$price) < up)
all.data <- all.data[!(rownames(all.data) == "4350"),]

reg5 <- lm(log(price) ~ bathrooms + bedrooms + condition + floors + waterfront + log(sqft_living) + log(sqft_lot) + view + statezip, data = all.data)
summary(reg5)
stargazer(reg5, type = "html", out = "final_reg_all.html", report = "vct*", single.row = TRUE)

par(mfrow=c(2,2))
plot(reg5)