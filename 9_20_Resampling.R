# R: Resampling
# 9/20: William and Micah

setwd('~/signal/week2/')

df = read.csv('speed-dating-simple.csv')

# A single train/test split

#df_males = df[df['gender'] == 1, ]
library(dplyr)
df_males = filter(df, gender==1)
names(df)
df_males_att=select(df_males, attr_o, sports:yoga)
names(df_males_att)
sort(coef(lm(attr_o ~ . ,df_males_att)),decreasing=T)
regress= lm(attr_o ~ . ,df_males_att)

split_data = function(df) {
  rowshuff=sample(nrow(df))
  split=rowshuff%%2
  list(test=df[split==1, ], train=df[split==0, ])
}
split = split_data(df_males_att)

split_predict = function(train,test) {
  trained = lm(attr_o ~ . ,train)
  train_pred=predict(trained,train)
  test_pred=predict(trained,test)
  list(train = train_pred, test = test_pred)
}

predictions = split_predict(split$train,split$test)

rmse = function(y1,y2) {
  sqrt(mean((y1-y2)^2))
}

rmses_test = numeric(100)
rmses_train = numeric(100)
for (i in 1:100){
  split = split_data(df_males_att)
  predictions = split_predict(split$train,split$test)
  rmses_test[i] = rmse(split$test$attr_o, predictions$test)
  rmses_train[i] = rmse(split$train$attr_o, predictions$train)
}

mean(rmses_test) #1.17
mean(rmses_train) #1.01
sd(rmses_test) #0.06
sd(rmses_train) #0.05
require('ggplot2')
ggplot() + geom_histogram(mapping=aes(rmses_test)) + geom_histogram(mapping=aes(rmses_train),color="red",fill="red",alpha=0.5)
#Slightly overfit.

# n-fold cross-validation

nfold_cv = function(df, n_fold) {
  rowshuff=sample(nrow(df))
  split=rowshuff%%n_fold
  tests = lapply(1:n_fold,function(i) df[split==i-1, ])
  trains = lapply(1:n_fold,function(i) df[!(split==i-1), ])
  rmses_test = numeric(n_fold)
  rmses_train = numeric(n_fold)
  for (i in 1:n_fold) {
     predictions = split_predict(trains[[i]],tests[[i]])
     #print(predictions$test)
     #print(tests[[i]]$attr_o)
     #stop()
     rmses_test[i] = rmse(tests[[i]]$attr_o, predictions$test)
  }
  sqrt(sum(rmses_test*rmses_test)/n_fold)
}

rmse2 = numeric(100)
rmse10 = numeric(100)
for (i in 1:100) {
  rmse2[i] = nfold_cv(df_males_att,2)
  rmse10[i] = nfold_cv(df_males_att,10)
}

ggplot() + geom_histogram(mapping=aes(rmse2)) + geom_histogram(mapping=aes(rmse10),fill="red",alpha=0.5)

# Stepwise Linear Regression

backward_step = function(df) {
  n=length(df)
  n_removed=numeric(n-1)
  rmse_cv=numeric(n-1)
  rmse_nocv=numeric(n-1)
  for (i in 1:(n-1)) {
   n_removed[i] = i-1
   rmse_cv[i]=nfold_cv(df,10)
   model= lm(attr_o ~. , df)
   rmse_nocv[i]=sqrt(mean(resid(model)^2))
   pval=summary(model)$coef[,4][-1] #the [-1] removes the intercept 
   pval=sort(pval)
   name=names(pval[length(pval)])
   remove=(names(df)!=name)
   df=df[remove]
  }
  data.frame(n_removed,rmse_cv,rmse_nocv)
}

back_step_result = backward_step(df_males_att)

ggplot(data=back_step_result, aes(x=n_removed)) + geom_point(aes(y=rmse_cv)) + geom_point(aes(y=rmse_nocv),color="red")

ratings = c("attr_o","sinc_o","intel_o","fun_o","amb_o")
models = vector(mode="list",length=5)
for (i in 1:length(ratings)) {
  df_sub=cbind(df_males[ratings[i]], select(df_males,sports:yoga))
  form = formula(paste(ratings[i], "~ ."))
  model_init = lm(form, df_sub)
  model = formula(lm(form, df_sub))
  models[[i]] = step(model_init, model, direction="backward")
}
model_init = lm(attr_o ~ ., df_males_att)
model = formula(lm(attr_o ~ ., df_males_att))
step_reg = step(model_init, model, direction="backward")

# Bootstrapping

bootstrap_bad = function(df,approach) {
  n=100
  rmses=numeric(n)
  for (i in 1:n){
    ind=sample(nrow(df),nrow(df),replace=TRUE)
    boot=df[ind,]
  
    if (approach==1) {
      train=boot
      test=df
      predictions=split_predict(train,test)
      rmses[i] = rmse(predictions$test, test$attr_o)
    }
    else if (approach==2) {
      test=boot
      train=df
      predictions=split_predict(train,test)
      rmses[i] = rmse(predictions$test, test$attr_o)
    }
  }
  sqrt(sum(rmses*rmses)/n)
}

backward_step_bad = function(df) {
  n=length(df)
  n_removed=numeric(n-1)
  rmse_cv=numeric(n-1)
  rmse_nocv=numeric(n-1)
  rmse_bad1=numeric(n-1)
  rmse_bad2=numeric(n-1)
  for (i in 1:(n-1)) {
    n_removed[i] = i-1
    rmse_cv[i]=nfold_cv(df,10)
    model= lm(attr_o ~. , df)
    rmse_nocv[i]=sqrt(mean(resid(model)^2))
    
    rmse_bad1[i] = bootstrap_bad(df,1)
    rmse_bad2[i] = bootstrap_bad(df,2)
    
    pval=summary(model)$coef[,4][-1] #the [-1] removes the intercept 
    pval=sort(pval)
    name=names(pval[length(pval)])
    remove=(names(df)!=name)
    df=df[remove]
  }
  data.frame(n_removed,rmse_cv,rmse_nocv,rmse_bad1,rmse_bad2)
}

bad_data = backward_step_bad(df_males_att)

ggplot(data=bad_data, aes(x=n_removed)) + geom_point(aes(y=rmse_cv)) + 
  geom_point(aes(y=rmse_nocv),color="red") +   geom_point(aes(y=rmse_bad1),color="blue") +
  geom_point(aes(y=rmse_bad2),color="green") 


bootstrap_good = function(df) {
  n=100
  rmses=numeric(n)
  for (i in 1:n){
    ind=sample(nrow(df),nrow(df),replace=TRUE)
    train=df[ind,]
    test=df[-unique(ind),]
    predictions=split_predict(train,test)
    rmses[i] = rmse(predictions$test, test$attr_o)

  }
  sqrt(sum(rmses*rmses)/n)
}

backward_step_good = function(df) {
  n=length(df)
  n_removed=numeric(n-1)
  rmse_cv=numeric(n-1)
  rmse_nocv=numeric(n-1)
  rmse_good=numeric(n-1)
  for (i in 1:(n-1)) {
    n_removed[i] = i-1
    rmse_cv[i]=nfold_cv(df,10)
    model= lm(attr_o ~. , df)
    rmse_nocv[i]=sqrt(mean(resid(model)^2))
    
    rmse_good[i] = bootstrap_good(df)
    
    pval=summary(model)$coef[,4][-1] #the [-1] removes the intercept 
    pval=sort(pval)
    name=names(pval[length(pval)])
    remove=(names(df)!=name)
    df=df[remove]
  }
  data.frame(n_removed,rmse_cv,rmse_nocv,rmse_good)
}

good_data = backward_step_good(df_males_att)

ggplot(data=good_data, aes(x=n_removed)) + geom_point(aes(y=rmse_cv)) + 
  geom_point(aes(y=rmse_nocv),color="red") +   geom_point(aes(y=rmse_good),color="blue") 

calc_alpha = function(X,Y) {
  var(Y)/(var(X)+var(Y))
}

gen_alphas = function(sdX,sdY) {
  X = rnorm(100,mean=10,sd=sdX)
  Y = rnorm(100,mean=10,sd=sdY)
  n_bootstrap=1000
  alphas = numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    indX=sample(100,replace=TRUE)
    indY=sample(100,replace=TRUE)
    alphas[i] = calc_alpha(X[indX],Y[indY])
  }
  alphas
}

a13 = gen_alphas(1,3)
a31 = gen_alphas(3,1)
a11 = gen_alphas(1,1)
a12 = gen_alphas(1,2)
a14 = gen_alphas(1,4)
ggplot() + geom_histogram(bins=60,aes(a31))
ggplot() + geom_histogram(bins=60,aes(a13))
ggplot() + geom_histogram(bins=60,aes(a11))
mean(a13)
mean(a31)
mean(a11)
sd(a13)
sd(a31)
sd(a11)
ggplot() + geom_histogram(bins=60,aes(a12),fill="brown2",alpha=0.5) + geom_histogram(bins=60,aes(a13),fill="coral1",alpha=0.5) + geom_histogram(bins=60,aes(a14),fill="darkorchid4",alpha=0.5)
