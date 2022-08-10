# R script corresponding to term paper "Evaluating the accuracy of cash flow forecasting methods". 
# The structure of this script is  based on that of the term paper.

#### 2. Data describtion and transformation
# Import datasets
data <- read.csv(file = "54datasets3.csv", header = T, sep = ",", dec = ".")
dataset <- split.data.frame(`data`, `data`$Company)
dataset3 <- dataset$`3`
dataset3[,1] <- as.Date(dataset3[,1])
dataset3[,4:6] <- lapply(dataset3[,4:6], factor)
dataset8 <- dataset$`8`
dataset8[,1] <- as.Date(dataset8[,1])
dataset8[,4:6] <- lapply(dataset8[,4:6], factor)

# Visualization of time series
library(ggplot2)
cf_plot3 <- ggplot() + 
  geom_line(data=dataset3, aes(x = Date,y = NetCF)) + 
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=22),
        legend.text=element_text(size=18),
        legend.title=element_text(size=22))

cf_plot8 <- ggplot() + 
  geom_line(data=dataset8, aes(x = Date,y = NetCF)) +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=22),
        legend.text=element_text(size=18),
        legend.title=element_text(size=22))

# Data summary
library(psych)
library(stargazer)
desc3 <- describe(dataset3$NetCF)
desc8 <- describe(dataset8$NetCF)
null3 <- sum(dataset3$NetCF==0) / length(dataset3$NetCF)*100
null8 <- sum(dataset8$NetCF==0) / length(dataset8$NetCF)*100
datadesc <- rbind(desc3[c(2,3,4,8,9,11,12)], desc8[c(2,3,4,8,9,11,12)])
datadesc <- cbind(datadesc, c(null3, null8))
datadesc <- round(datadesc, digits=2)
dimnames(datadesc) <- list(c("dataset1", "dataset2"), 
                           c("Length", "Mean", "Std", "Min", "Max", "Skewness", "Kurtosis", "Null %"))
stargazer(datadesc, summary=FALSE, type="html", digits=2, out = "datadesc")

# Shapiro-Walk test 1
shapiro.test(dataset3$NetCF)
shapiro.test(dataset8$NetCF)

# Outlier correction
outliersZ <- function(datasetcomp, CF, zCutOff) {
  stdev <- sd(CF)
  absZ <- abs(CF - mean(CF)) / stdev
  datasetcomp[-which(absZ > zCutOff),] 
}

d3.corZ <- outliersZ(dataset3, dataset3$NetCF, 5)
d8.corZ <- outliersZ(dataset8, dataset8$NetCF, 5)

# Box Cox transformation and implementation of inverse (for later use)
library(geoR)
boxtrans <- function(CFs) {
  CF.box <- boxcoxfit(CFs, lambda = seq(-4,4,0.1), lambda2 = TRUE)
  lambda <- CF.box$lambda[1]
  lambda2 <- CF.box$lambda[2]
  if(lambda == 0){CF.norm <- log(CFs + lambda2)}
  if(lambda != 0){CF.norm <- ((CFs + lambda2) ^ lambda - 1) / lambda}
  output <- list(CF.norm, lambda, lambda2)
}

boxtrans3 <- boxtrans(d3.corZ$NetCF)
d3.cor <- d3.corZ
d3.cor$NetCF <- boxtrans3[[1]]

boxtrans8 <- boxtrans(d8.corZ$NetCF)
d8.cor <- d8.corZ
d8.cor$NetCF <- boxtrans8[[1]]

boxcoxinv <- function(transCF, lambda, lambda2) {
  if(lambda == 0){y <- (exp(transCF) - lambda2)}
  if(lambda != 0){y <- ((lambda*transCF+1) ^ (1/lambda) - lambda2)}
  return(y)
}

# Shapiro-Walk test 2
shapiro.test(d3.cor$NetCF)
shapiro.test(d8.cor$NetCF)

# Datasets formatted as timeseries
tsd3.cor <- ts(d3.cor) 
tsd8.cor <- ts(d8.cor)

#### 3. Forecasting Methods and Implementation
# ARIMA model parameter selection
library(forecast)
auto.arima(tsd3.cor[,3])
auto.arima(tsd8.cor[,3])

# Regression model parameter selection
lm3.1 <- lm(NetCF ~ Holiday + DayWeek + DayMonth -1, data = tsd3.cor)
lm3.2 <- lm(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth 
            + DayWeek*DayMonth -1, data = tsd3.cor)
step(lm3.2)

lm8.1 <- lm(NetCF ~ Holiday + DayWeek + DayMonth -1, data = tsd8.cor)
lm8.2 <- lm(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth 
            + DayWeek*DayMonth -1, data = tsd8.cor)
step(lm8.2)

# Random forest parameter selection
library(caret)
cv.10folds.1 <- createFolds(tsd3.cor[,3], k=10)
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10folds.1)
cv.10folds.2 <- createFolds(tsd8.cor[,3], k=10)
ctrl.2 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10folds.2)

rf3.cv.1 <- train(NetCF ~ . - Date - Company, data=tsd3.cor, method = "rf", tuneLength =5, ntree = 1000, trControl = ctrl.1)
rf3.cv.2 <- train(NetCF ~ . - Date - Company, data=tsd3.cor, method = "rf", tuneLength =5, ntree = 750, trControl = ctrl.1)
rf3.cv.3 <- train(NetCF ~ . - Date - Company, data=tsd3.cor, method = "rf", tuneLength =5, ntree = 500, trControl = ctrl.1)

rf8.cv.1 <- train(NetCF ~ . - Date - Company, data=tsd8.cor, method = "rf", tuneLength =5, ntree = 1000, trControl = ctrl.2)
rf8.cv.2 <- train(NetCF ~ . - Date - Company, data=tsd8.cor, method = "rf", tuneLength =5, ntree = 750, trControl = ctrl.2)
rf8.cv.3 <- train(NetCF ~ . - Date - Company, data=tsd8.cor, method = "rf", tuneLength =5, ntree = 500, trControl = ctrl.2)

#### 4. Evaluation of Accuracy and Discussion
### Exemplary forecast
d3_train <- window(tsd3.cor, end = (length(tsd3.cor[,3])-50))
d3_test  <- window(tsd3.cor, start = length(tsd3.cor[,3])-49)
d8_train <- window(tsd8.cor, end = length(tsd8.cor[,3])-50)
d8_test  <- window(tsd8.cor, start = length(tsd8.cor[,3])-49)

ar_model3 <- Arima(d3_train[,3], order=c(1,0,0), include.mean=TRUE)
ar_fc3 <- predict(ar_model3, n.ahead=50)
ar_fc3 <- boxcoxinv(ar_fc3$pred, boxtrans3[[2]], boxtrans3[[3]])
ar_model8 <- Arima(d8_train[,3], order=c(2,1,2), include.mean=FALSE)
ar_fc8 <- predict(ar_model8, n.ahead=50)
ar_fc8 <- boxcoxinv(ar_fc8$pred, boxtrans8[[2]], boxtrans8[[3]])

reg_model3 <- lm(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth 
                 + DayWeek*DayMonth -1, data = d3_train)
reg_fc3 <- predict(reg_model3, d3_test)
reg_fc3 <- boxcoxinv(reg_fc3, boxtrans3[[2]], boxtrans3[[3]])
reg_model8 <- lm(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth -1, 
                 data = d8_train)
reg_fc8 <- predict(reg_model8, d8_test)
reg_fc8 <- boxcoxinv(reg_fc8, boxtrans8[[2]], boxtrans8[[3]])

library(randomForest)
rf_model3 <- randomForest(NetCF ~.- Date - Company, data= d3_train, ntree=750, mtry=2)
rf_fc3 <- predict(rf_model3, d3_test)
rf_fc3 <- boxcoxinv(rf_fc3, boxtrans3[[2]], boxtrans3[[3]])
rf_model8 <- randomForest(NetCF ~.- Date - Company, data= d8_train, ntree=750, mtry=2)
rf_fc8 <- predict(rf_model8, d8_test)
rf_fc8 <- boxcoxinv(rf_fc8, boxtrans8[[2]], boxtrans8[[3]])

# Visualization of the forecasts
fc.plot3 <- ggplot() + 
  geom_line(data=d3.corZ[1182:1242,], aes(x = Date, y = NetCF, col = "C1")) +
  geom_line(aes(x = d3.corZ[1193:1242,1], y = ar_fc3, col = "C2")) +
  geom_line(aes(x = d3.corZ[1193:1242,1], y = reg_fc3, col = "C3")) +
  geom_line(aes(x = d3.corZ[1193:1242,1], y = rf_fc3, col = "C4")) +
  scale_color_manual(
    values = c(C1 = "black", C2 = "red", C3 = "blue", C4 = "forestgreen"),
    name="Forecast", 
    labels = c("Real CFs", "ARIMA", "Regression", "Random Forest")) +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        legend.text = element_text(size=18),
        legend.title = element_text(size=22))  

fc.plot8 <- ggplot() + 
  geom_line(data=d8.corZ[948:1008,], aes(x = Date, y = NetCF, col = "C1")) +
  geom_line(aes(x = d8.corZ[959:1008,1], y = ar_fc8, col = "C2")) +
  geom_line(aes(x = d8.corZ[959:1008,1], y = reg_fc8, col = "C3")) +
  geom_line(aes(x = d8.corZ[959:1008,1], y = rf_fc8, col = "C4")) +
  scale_color_manual(
    values = c(C1 = "black", C2 = "red", C3 = "blue", C4 = "forestgreen"),
    name="Forecast", 
    labels = c("Real CFs", "ARIMA", "Regression", "Random Forest")) +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=22),
        legend.text = element_text(size=18),
        legend.title = element_text(size=22))

### Time series cross validation
initWindow <- 36
horizon <- 50
set.seed(999)

## tsCV based on forecast package
# Naive estimator
naive3 <- tsCV(tsd3.cor[,3], naive, h=horizon)
naive3.rmse <- sqrt(colMeans(naive3^2, na.rm =TRUE))
naive3.mae <- colMeans(abs(naive3), na.rm =TRUE)

naive8 <- tsCV(tsd8.cor[,3], naive, h=horizon)
naive8.rmse <- sqrt(colMeans(naive8^2, na.rm =TRUE))
naive8.mae <- colMeans(abs(naive8), na.rm =TRUE)

# ARIMA model
fun.ar3 <- function(x, h){forecast(Arima(x, order=c(1,0,0), include.mean=TRUE), h=h)}
ar3.res <- tsCV(tsd3.cor[,3], fun.ar3, h=horizon, initial = initWindow)
ar3.rmse <- sqrt(colMeans(ar3.res^2, na.rm =TRUE))
ar3.mae <- colMeans(abs(ar3.res), na.rm =TRUE)

fun.ar8 <- function(x, h){forecast(Arima(x, order=c(2,1,2), include.mean=FALSE), h=h)}
ar8.res <- tsCV(tsd8.cor[,3], fun.ar8, h=horizon, initial = initWindow)
ar8.rmse <- sqrt(colMeans(ar8.res^2, na.rm =TRUE))
ar8.mae <- colMeans(abs(ar8.res), na.rm =TRUE)

## tsCV based on forecast package (deduced from https://rpubs.com/crossxwill/time-series-cv)
forecast_horizons = c(1:horizon)
library(doParallel)
registerDoParallel(cores=8)

# Regression model
reg3.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(reg3.acc) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd3.cor[,3])-initWindow+1):(length(tsd3.cor[,3])-initWindow-length(forecast_horizons)+2)) 
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl = trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i],
                      fixedWindow = F, allowParallel = T, seeds=seeds, savePredictions = T) 
  train.reg3 <- train(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth 
                      + DayWeek*DayMonth -1, data=tsd3.cor, method="lm", trControl = ctrl)
  reg3.acc[1,i] <- sqrt(mean((train.reg3$pred$pred-train.reg3$pred$obs)^2))
  reg3.acc[2,i] <- mean(abs(train.reg3$pred$pred-train.reg3$pred$obs))
}

reg8.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(reg8.acc) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd8.cor[,3])-initWindow+1):(length(tsd8.cor[,3])-initWindow-length(forecast_horizons)+2)) 
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl <- trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i],
                       fixedWindow = F, allowParallel = T, seeds=seeds, savePredictions = T) 
  train.reg8 <- train(NetCF ~ Holiday + DayWeek + DayMonth + Holiday*DayWeek + Holiday*DayMonth -1, 
                      data=tsd8.cor, method="lm", trControl = ctrl)
  reg8.acc[1,i] <- sqrt(mean((train.reg8$pred$pred-train.reg8$pred$obs)^2))
  reg8.acc[2,i] <- mean(abs(train.reg8$pred$pred-train.reg8$pred$obs))
}

# Random Forest model
rf3.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(rf3.acc) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd3.cor[,3])-initWindow+1):(length(tsd3.cor[,3])-initWindow-length(forecast_horizons)+2))
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl <- trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i], 
                       fixedWindow = F, allowParallel = T, seeds=seeds, savePredictions = T) 
  train.rf3 <- train(NetCF ~ . - Date - Company, data=tsd3.cor, method = "rf", ntree = 750, trControl = ctrl)
  rf3.mtry2 <- train.rf3$pred[seq(1, length(train.rf3$pred$pred), 2),]
  rf3.acc[1,i] <- sqrt(mean((rf3.mtry2$pred-rf3.mtry2$obs)^2))
  rf3.acc[2,i] <- mean(abs(rf3.mtry2$pred-rf3.mtry2$obs))
}

rf8.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(rf8.acc) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd8.cor[,3])-initWindow+1):(length(tsd8.cor[,3])-initWindow-length(forecast_horizons)+2))
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl <- trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i],
                       fixedWindow = F, allowParallel = T, seeds = seeds, savePredictions = T) 
  train.rf8 <- train(NetCF ~ . - Date - Company, data = tsd8.cor, method = "rf", ntree = 750, trControl = ctrl)
  rf8.mtry2 <- train.rf8$pred[seq(1, length(train.rf8$pred$pred), 2),]
  rf8.acc[1,i] <- sqrt(mean((rf8.mtry2$pred-rf8.mtry2$obs)^2))
  rf8.acc[2,i] <- mean(abs(rf8.mtry2$pred-rf8.mtry2$obs))
}

# Combined error metrics 
rmsse3 <- (rbind(ar3.rmse/naive3.rmse, reg3.acc[1,] / naive3.rmse, rf3.acc[1,] / naive3.rmse))
mase3 <- (rbind(ar3.mae/naive3.mae, reg3.acc[2,] / naive3.mae, rf3.acc[2,] / naive3.mae))   

rmsse8 <- (rbind(ar8.rmse/naive8.rmse, reg8.acc[1,] / naive8.rmse, rf8.acc[1,] / naive8.rmse))
mase8 <- (rbind(ar8.mae/naive8.mae, reg8.acc[2,] / naive8.mae, rf8.acc[2,] / naive8.mae))   

# Visualization of error metrics
library(reshape2)
df.rmsse3 <- as.data.frame(cbind(forecast_horizons, t(rmsse3)))
colnames(df.rmsse3) <- c("horizon", "ARIMA", "Regression", "Random Forest")
rownames(df.rmsse3) <- NULL
melt.rmsse3 <- melt(df.rmsse3, id.var = "horizon", measure.vars = c("ARIMA", "Regression", "Random Forest"))
fc.rmsse3 <- ggplot(melt.rmsse3, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("RMSSE") + labs(colour = "Forecast model") +
  ylim(0.675, 0.775) + scale_color_manual(values=c("red", "blue", "forestgreen"))

df.mase3 <- as.data.frame(cbind(forecast_horizons, t(mase3)))
colnames(df.mase3) <- c("horizon", "ARIMA", "Regression", "Random Forest")
rownames(df.mase3) <- NULL
melt.mase3 <- melt(df.mase3, id.var = "horizon", measure.vars = c("ARIMA", "Regression", "Random Forest"))
fc.mase3 <- ggplot(melt.mase3, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("MASE") + labs(colour = "Forecast model") +
  ylim(0.55, 0.725) + scale_color_manual(values=c("red", "blue", "forestgreen"))

df.rmsse8 <- as.data.frame(cbind(forecast_horizons, t(rmsse8)))
colnames(df.rmsse8) <- c("horizon", "ARIMA", "Regression", "Random Forest")
rownames(df.rmsse8) <- NULL
melt.rmsse8 <- melt(df.rmsse8, id.var = "horizon", measure.vars = c("ARIMA", "Regression", "Random Forest"))
fc.rmsse8 <- ggplot(melt.rmsse8, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("RMSSE") + labs(colour = "Forecast model") +
  ylim(0.6, 0.85) + scale_color_manual(values=c("red", "blue", "forestgreen"))

df.mase8 <- as.data.frame(cbind(forecast_horizons, t(mase8)))
colnames(df.mase8) <- c("horizon", "ARIMA", "Regression", "Random Forest")
rownames(df.mase8) <- NULL
melt.mase8 <- melt(df.mase8, id.var = "horizon", measure.vars = c("ARIMA", "Regression", "Random Forest"))
fc.mase8 <- ggplot(melt.mase8, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("MASE") + labs(colour = "Forecast model") +
  ylim(0.5, 1) + scale_color_manual(values=c("red", "blue", "forestgreen"))

#### Random Forest based on original CF data
tsd3 <- ts(dataset3) 
tsd8 <- ts(dataset8)

naive3n <- tsCV(tsd3[,3], naive, h=horizon)
naive3n.rmse <- sqrt(colMeans(naive3^2, na.rm =TRUE))
naive3n.mae <- colMeans(abs(naive3), na.rm =TRUE)

naive8n <- tsCV(tsd8[,3], naive, h=horizon)
naive8n.rmse <- sqrt(colMeans(naive8^2, na.rm =TRUE))
naive8n.mae <- colMeans(abs(naive8), na.rm =TRUE)

rf3n.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(rf3.acc1) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd3[,3])-initWindow+1):(length(tsd3[,3])-initWindow-length(forecast_horizons)+2))
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl <- trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i], 
                       fixedWindow = F, allowParallel = T, seeds=seeds, savePredictions = T) 
  train.rf3 <- train(NetCF ~ . - Date - Company, data=tsd3, method = "rf", ntree = 750, trControl = ctrl)
  rf3.mtry2 <- train.rf3$pred[seq(1, length(train.rf3$pred$pred), 2),]
  rf3.acc1[1,i] <- sqrt(mean((rf3.mtry2$pred-rf3.mtry2$obs)^2))
  rf3.acc1[2,i] <- mean(abs(rf3.mtry2$pred-rf3.mtry2$obs))
}

rf8n.acc <- matrix(data = NA, nrow = 2, ncol = length(forecast_horizons))
rownames(rf8n.acc) <- c("RMSE", "MAE")
for (i in 1:length(forecast_horizons)) {
  lengthseeds <- c((length(tsd8[,3])-initWindow+1):(length(tsd8[,3])-initWindow-length(forecast_horizons)+2))
  seeds <- vector(mode = "list", length = lengthseeds[i])
  for(e in 1:(lengthseeds[i]-1)) seeds[[e]] <- sample.int(1000, 2)
  seeds[[lengthseeds[i]]] <- sample.int(1000, 1)
  ctrl <- trainControl(method = 'timeslice', initialWindow = initWindow, horizon = forecast_horizons[i], 
                       fixedWindow = F, allowParallel = T, seeds = seeds, savePredictions = T) 
  train.rf8 <- train(NetCF ~ . - Date - Company, data = tsd8, method = "rf", ntree = 750, trControl = ctrl)
  rf8.mtry2 <- train.rf8$pred[seq(1, length(train.rf8$pred$pred), 2),]
  rf8n.acc[1,i] <- sqrt(mean((rf8.mtry2$pred-rf8.mtry2$obs)^2))
  rf8n.acc[2,i] <- mean(abs(rf8.mtry2$pred-rf8.mtry2$obs))
}
stopImplicitCluster()

## Comparison & visualization of results 
rf3.mase <- rbind(mase3[3,], rf3n.acc[2,]/naive3n.mae)
rf3.rmsse <- rbind(rmsse3[3,], rf3n.acc[1,]/naive3n.rmse)
rf8.mase <- rbind(mase8[3,], rf8n.acc[2,]/naive8n.mae)
rf8.rmsse <- rbind(rmsse8[3,], rf8n.acc[1,]/naive8n.rmse)

rf3.mase <- as.data.frame(cbind(forecast_horizons, t(rf3.mase)))
colnames(rf3.mase) <- c("horizon", "Random Forest old", "Random Forest new")
rownames(rf3.mase) <- NULL
melt.rf3.mase <- melt(rf3.mase, id.var = "horizon", measure.vars = c("Random Forest old", "Random Forest new"))
rf3.mase.plot <- ggplot(melt.rf3.mase, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("MASE") + labs(colour = "Forecast model") +
  ylim(0.6, 0.8) + scale_color_manual(values=c("red", "blue", "forestgreen"))

rf3.rmsse <- as.data.frame(cbind(forecast_horizons, t(rf3.rmsse)))
colnames(rf3.rmsse) <- c("horizon", "Random Forest old", "Random Forest new")
rownames(rf3.rmsse) <- NULL
melt.rf3.rmsse <- melt(rf3.rmsse, id.var = "horizon", measure.vars = c("Random Forest old", "Random Forest new"))
rf3.rmsse.plot <- ggplot(melt.rf3.rmsse, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("RMSSE") + labs(colour = "Forecast model") +
  ylim(0.6, 0.8) + scale_color_manual(values=c("red", "blue", "forestgreen"))

rf8.mase <- as.data.frame(cbind(forecast_horizons, t(rf8.mase)))
colnames(rf8.mase) <- c("horizon", "Random Forest old", "Random Forest new")
rownames(rf8.mase) <- NULL
melt.rf8.mase <- melt(rf8.mase, id.var = "horizon", measure.vars = c("Random Forest old", "Random Forest new"))
rf8.mase.plot <- ggplot(melt.rf8.mase, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("MASE") + labs(colour = "Forecast model") +
  ylim(0.7, 1.1) + scale_color_manual(values=c("red", "blue", "forestgreen"))

rf8.rmsse <- as.data.frame(cbind(forecast_horizons, t(rf8.rmsse)))
colnames(rf8.rmsse) <- c("horizon", "Random Forest old", "Random Forest new")
rownames(rf8.rmsse) <- NULL
melt.rf8.rmsse <- melt(rf8.rmsse, id.var = "horizon", measure.vars = c("Random Forest old", "Random Forest new"))
rf8.rmsse.plot <- ggplot(melt.rf8.rmsse, aes(x=horizon, y = value, group = variable, colour = variable)) + geom_line() +
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=18),legend.title=element_text(size=22))+
  xlab("Forecast horizon") + ylab("RMSSE") + labs(colour = "Forecast model") +
  ylim(0.7, 0.9) + scale_color_manual(values=c("red", "blue", "forestgreen"))
