# Regularized Linear Regression
# 9/21 Micah and Michelle

library("dplyr")
library("ggplot2")
library("glmnet")
library("caret")
library("Rmisc")

costL1 = lapply(1:nrow(grid), function(i) cost(x,y,grid$aEst[i],grid$lambda[i],p=1))
set.seed(1); j =50; a = .25
x=rnorm(j)
error = sqrt(1-a^2)*rnorm(j)
y=a*x + error

regress = lm(y ~ x-1)
summary(regress)
aEst=coef(regress)
names(regress)
sum(regress$residuals^2)

#given a guess for aEst already
cost = function (x,y,aEst, lambda, p) {
  yEst=aEst*x
  SSE=sum((y-yEst)^2)
  if (p==1) {
    return( SSE+lambda*(abs(aEst)))
  } else if (p==2) {
    return( SSE+lambda*(aEst^2))
  } else return("ERROR. USE p=1, p=2")
}  
cost(1,2,3,4,1) #13

aEst=seq(-.1,.3,.001)
lambda = sapply(-2:7, function(i) 2^i)
?expand.grid
grid=expand.grid(aEst,lambda)
names(grid)=c("aEst","lambda")
costL1 = sapply(1:nrow(grid), function(i) cost(x,y,grid$aEst[i],grid$lambda[i],p=1))
costL2 = sapply(1:nrow(grid), function(i) cost(x,y,grid$aEst[i],grid$lambda[i],p=2))
head(costL1); cost(x,y,-.1,.25,1)
grid=cbind(grid, data.frame(costL1), data.frame(costL2))
head(grid)

get_plot = function(lambdain,p) {
  fgrid = filter(grid,lambda == lambdain)
  if (p == 1) {
    return(qplot(fgrid$aEst,fgrid$costL1))
  } else if (p == 2) {
    return(qplot(fgrid$aEst,fgrid$costL2))
  } else return("ERROR. USE p=1, p=2")
}

plotsL1 = lapply(lambda,get_plot,p=1)
plotsL2 = lapply(lambda,get_plot,p=2)
multiplot(plotlist=plotsL1,cols =2)
multiplot(plotlist=plotsL2,cols =2)


#NOW with REAL data

df = read.csv("C:/Users/Micah/Documents/R/speed-dating-simple.csv")
df_attr = select(df,-intel_o, -amb_o, -fun_o, -sinc_o)
df_attr = filter(df_attr, gender==1)
df_attr = select(df_attr,-gender)

activities_scaled = scale(select(df_attr,-attr_o))
attr_o = df_attr$attr_o
df_attr2 = data
head(activities_scaled)

gimlet1 = glmnet(activities_scaled,attr_o,alpha=1)
names(gimlet1)
str(gimlet1)

gimlet2 = glmnet(activities_scaled,attr_o,alpha=0)
names(gimlet2)
str(gimlet2)
gimlet1$lambda
gimlet2$lambda


?glmnet
rmse = function(y1,y2) sqrt(mean((y1-y2)^2))

get_rmses = function(fit, features, target) {
  lambdas= fit$lambda
  rmses=numeric(length(lambdas))
  for (i in 1:length(lambdas)) {
    predictions=predict(fit, features, s=lambdas[i])
    rmses[i] = rmse(target,predictions)
  }
  rmses
}

rmse1=get_rmses(gimlet1,activities_scaled,attr_o)
rmse2=get_rmses(gimlet2,activities_scaled,attr_o)
qplot(gimlet1$lambda, rmse1)
qplot(gimlet2$lambda, rmse2)

fit1=cv.glmnet(activities_scaled,attr_o,alpha=1)
fit2=cv.glmnet(activities_scaled,attr_o,alpha=0)
qplot(fit1$lambda, fit1$cvm)
qplot(fit2$lambda, fit2$cvm, xlim=c(0,2))
fit1$lambda.min
fit2$lambda.min


##STEPWISE REGRESSION vs REGULARIZATION

#some code from yesterday
split_predict = function(train,test) {
  trained = lm(attr_o ~ . ,train)
  train_pred=predict(trained,train)
  test_pred=predict(trained,test)
  list(train = train_pred, test = test_pred)
}


n_fold = 20
rowshuff=sample(nrow(df_attr))
fold=rowshuff%%n_fold+1

predictions_step = numeric(nrow(df_attr))
predictions_cv1 = numeric(nrow(df_attr))
predictions_cv2 = numeric(nrow(df_attr))

for (i in 1:n_fold) {
  tests = df_attr[fold==i, ]
  trains = df_attr[fold!=i, ]
  model_init = lm(attr_o ~ ., trains)
  model = formula("attr_o ~ .")
  step_reg = step(model_init, model, direction="backward")
  predictions_step[i == fold] = predict(step_reg,tests)

  activities_train = scale(select(trains,-attr_o))
  activities_test = scale(select(tests,-attr_o), center=attr(activities_train, 'scaled:center'),scale=attr(activities_train, 'scaled:scale'))
  attr_train = trains$attr_o
  
  cvfit1 = cv.glmnet(activities_train,attr_train, alpha = 1)
  cvfit2 = cv.glmnet(activities_train,attr_train, alpha = 0)
  
  predictions_cv1[i == fold] =predict(cvfit1,activities_test,s = cvfit1$lambda.min)
  predictions_cv2[i == fold] =predict(cvfit2,activities_test,s = cvfit2$lambda.min)

}

rmse_step = rmse(predictions_step,df_attr$attr_o)
rmse_cv1 = rmse(predictions_cv1,df_attr$attr_o)
rmse_cv2 = rmse(predictions_cv2,df_attr$attr_o)
c(rmse_step,rmse_cv1,rmse_cv2)

#ELASTIC NET REGRESSION
# Set grid of parameter values to search over
param_grid = expand.grid(alpha = 1:10 * 0.1,
                         lambda = 10^seq(-4, 0, length.out=10))
# Set 10-fold cross validation repeated 3x
control = trainControl(method="repeatedcv", number=10,
                       repeats=3, verboseIter=TRUE)
# Search over the grid
caret_fit = train(x=activities_scaled, y=attr_o, method="glmnet",
                  tuneGrid=param_grid, trControl=control,
                  preProc=c("center", "scale"))
# View the optimal values of alpha and lambda
caret_fit$bestTune
# View the cross-validated RMSE estimates

min(caret_fit$results$RMSE)
c(rmse_step,rmse_cv1,rmse_cv2)

coef(cvfit1,s=cvfit1$lambda.min)
coef(cvfit2,s=cvfit2$lambda.min)

