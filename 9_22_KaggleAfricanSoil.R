# Kaggle African Soil Competition
# 9/22 Micah, Michelle, Adom

getwd()
setwd("C:/Users/Michelle/Google Drive/SignalData")

library(dplyr)
library(ggplot2)
library(glmnet)
library(caret)
library(Rmisc)
library(readr)

df_test = read_csv("sorted_test.csv")
names(df_test)
names(df_test)
df_train = read_csv("training.csv")
names(df_train)
View(df_test)
str(df_train)
class(df_train)

k = scale(select(df_train,-PIDN,-(BSAN:Sand)))
ca = df_train$Ca
P = df_train$P
pH=df_train$pH
sand=df_train$Sand
carbon=df_train$SOC
PIDN = df_test$PIDN

alphas = c(1,0.1,0.05,0.01,0.001,0)
fits = lapply(alphas,function(alph) cv.glmnet(k,ca,alpha=alph))
fits[1]

rmse = function(y1,y2) sqrt(mean((y1-y2)^2))
#predict(fits[[1]],k,s = fits[[1]]$lambda.min)
predictions = lapply(fits, function(fit) predict(fit,k,s = fit$lambda.min))
predictions
rmse_test = sapply(predictions,rmse,y2 = ca)
rmse_test


#this gives coeffs for min lambda excluding intercept.
coeffs=lapply(1:6,function(i) coef(fits[[i]],s=fits[[i]]$lambda.min)[-1,] )

#detour. graph any (or all) k values against alpha
coeffs2 = rbind(coeffs[[1]],coeffs[[2]],coeffs[[3]],coeffs[[4]],coeffs[[5]],coeffs[[6]])
graphs=data.frame(alpha=alphas, coeffs = coeffs2)
z = geom_line(aes(x=log(alpha), y=coeffs.m7497.96))
ggplot(graphs) + z

?substring
dimnames(k)

wavenum=unlist(sapply(dimnames(k), function(x) as.numeric(substring(x,2))))
wavenum
str(wavenum)

alpha_dfs= lapply(1:6, function(i) data.frame(k=wavenum,coef=coeffs[[i]]))
p1 = ggplot(alpha_dfs[[1]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))
p2 = ggplot(alpha_dfs[[2]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))
p3 = ggplot(alpha_dfs[[3]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))
p4 = ggplot(alpha_dfs[[4]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))
p5 = ggplot(alpha_dfs[[5]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))
p6 = ggplot(alpha_dfs[[6]]) + geom_point(aes(x = k, y = coef, colour="#CC0000", alpha=.6))

multiplot(p1,p2,p3,p4,p5,p6, cols = 2)

p1 = ggplot(alpha_dfs[[1]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
p2 = ggplot(alpha_dfs[[2]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
p3 = ggplot(alpha_dfs[[3]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
p4 = ggplot(alpha_dfs[[4]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
p5 = ggplot(alpha_dfs[[5]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
p6 = ggplot(alpha_dfs[[6]],aes(x = k, y = coef)) + geom_bar( colour="#CC0000", alpha=.6, stat='identity')
multiplot(p1,p2,p3,p4,p5,p6, cols = 2)


sapply(1:6, function(i) fits[[i]]$lambda.min)
targets=list(ca,P,pH,sand,carbon)
best_val=vector('list',5)
min_rmse=numeric(5)

for (i in 1:length(targets)) {
  
  print("start loop")
#ELASTIC NET REGRESSION
# Set grid of parameter values to search over
param_grid = expand.grid(alpha = 1:10 * 0.1,
                         lambda = 10^seq(-3, 1, length.out=10))

# Set 10-fold cross validation repeated 3x
control = trainControl(method="repeatedcv", number=10,
                       repeats=1, verboseIter=TRUE)
# Search over the grid
caret_fit = train(x=k, y=targets[[i]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
print("caret done")
# View the optimal values of alpha and lambda
best_val[[i]]=caret_fit$bestTune
# View the cross-validated RMSE estimates
min_rmse[i]=min(caret_fit$results$RMSE)

print("done with loop")

}

#ELASTIC NET REGRESSION
param_grid = expand.grid(alpha = seq(0,0.4,length.out = 10),
                         lambda = seq(0,0.1,length.out = 5))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_ca = train(x=k, y=targets[[1]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_ca=caret_fit_ca$bestTune
min_rmse_ca=min(caret_fit_ca$results$RMSE)
min_rmse_ca

best_val[1]
min_rmse
rmse_test
fits[[1]]$lambda.min


#targets=list(ca,P,pH,sand,carbon)

# test for P
param_grid = expand.grid(alpha = 4:10 * 0.1,
                         lambda = 10^seq(-5, -1,length.out = 10))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_P = train(x=k, y=targets[[2]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_P=caret_fit_P$bestTune
min_rmse_P = min(caret_fit_P$results$RMSE)
min_rmse_P

# test for log(P+1)
param_grid = expand.grid(alpha = 1:10 * 0.1,
                         lambda = 10^seq(-4, 1,length.out = 10))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_Plog = train(x=k, y=log(targets[[2]] + 1), method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_Plog=caret_fit_Plog$bestTune
min_rmse_Plog = min(caret_fit_Plog$results$RMSE)


# test for pH
param_grid = expand.grid(alpha = 4:10 * 0.1,
                         lambda = 10^seq(-5, -1,length.out = 8))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_pH = train(x=k, y=targets[[3]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_pH=caret_fit_pH$bestTune
min_rmse_pH=min(caret_fit_pH$results$RMSE)
min_rmse_pH


# test for Sand
param_grid = expand.grid(alpha = 4:10 * 0.1,
                         lambda = 10^seq(-5, -1,length.out = 8))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_sa = train(x=k, y=targets[[4]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_sand=caret_fit_sa$bestTune
min_rmse_sand=min(caret_fit_sa$results$RMSE)
min_rmse_sand

# test for carbon
param_grid = expand.grid(alpha = 4:10 * 0.1,
                         lambda = 10^seq(-5, -1,length.out = 8))

control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
caret_fit_carbon = train(x=k, y=targets[[5]], method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
best_val_carbon=caret_fit_carbon$bestTune
min_rmse_carbon=min(caret_fit_carbon$results$RMSE)
min_rmse_carbon

rmse = function(y1,y2) sqrt(mean((y1-y2)^2))
k_test = scale(select(df_test,-PIDN,-(BSAN:Depth)))
k_test
prediction_ca = predict(caret_fit_ca,k_test,s=caret_fit_ca$bestTune)
#rmse(prediction_ca,ca)
prediction_P = exp(predict(caret_fit_Plog,k_test,s=caret_fit_carbon$bestTune)) - 1
#rmse(prediction_P,P)
prediction_pH = predict(caret_fit_pH,k_test,s=caret_fit_pH$bestTune)
#rmse(prediction_pH,pH)
prediction_sa = predict(caret_fit_sa,k_test,s=caret_fit_sa$bestTune)
#rmse(prediction_sa,sand)
prediction_carbon = predict(caret_fit_carbon,k_test,s=caret_fit_carbon$bestTune)
#rmse(prediction_carbon,carbon)

alldata = data.frame(PIDN = PIDN, Ca = prediction_ca, P = prediction_P, pH = prediction_pH, SOC = prediction_carbon, Sand = prediction_sa)

write.csv(alldata,"AfricanSoil.csv",quote = FALSE, row.names = FALSE)


