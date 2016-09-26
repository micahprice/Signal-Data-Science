# Self Assessment 2
# 9/26 Micah

#Start time 10:16
#stopped at 12:40
#started at 1:15
# END 2:45
#~ 4 hours

library(ggplot2)
library(dplyr)
library(glmnet)

library('psych')
df = msq

#Replace NAs and create features etc.
replace_NA = function(df) {
  for (i in 1:length(df)) {
    df[[i]][is.na(df[[i]])] = mean(df[[i]], na.rm=TRUE)
  }
  df
}

df=replace_NA(df)
#Threw warnings, but seems to have worked.
sum(is.na(df))

features=select(df,active:scornful)
Extra = df[,"Extraversion", drop=FALSE]  #Dont use drop=FALSE (want vector later)
Neuro = df[,"Neuroticism", drop=FALSE]
set.seed = 1
alpha = seq(0, 1, .1)
lambda = 10^seq(1, -3, length.out = 50)

#make random fold assignments
n_fold=10
numrows=nrow(features)
make_nfolds = function(numrows, n_fold) {
  rowshuff=sample(numrows)
  fold=rowshuff%%n_fold + 1
  fold
}
fold=make_nfolds(numrows,n_fold)

#Store folds
#scale feat_test fold by fold, also scale feat_test based on fold (i) of feat_train
feat_train  = vector("list", n_fold)
feat_test   = vector("list", n_fold)
Neuro_train = vector("list", n_fold)
Extra_train = vector("list", n_fold)
Neuro_test  = vector("list", n_fold)
Extra_test  = vector("list", n_fold)
feat_train  = lapply(1:n_fold, function(i) scale(features[fold!=i, ]))
feat_test   = lapply(1:n_fold, function(i) {
      scale(features[fold==i, ], center=attr(feat_train[[i]], 'scaled:center'),scale=attr(feat_train[[i]], 'scaled:scale'))
      })
Neuro_train = lapply(1:n_fold, function(i) Neuro[fold!=i, ])
Extra_train = lapply(1:n_fold, function(i) Extra[fold!=i, ])
Neuro_test = lapply(1:n_fold, function(i) Neuro[fold==i, ])
Extra_test = lapply(1:n_fold, function(i) Extra[fold==i, ])

#RMSE function
rmse = function(y1,y2) sqrt(mean((y1-y2)^2))

#initialize results df
results = data.frame(alpha=numeric(), lambda=numeric(),rmse_Neuro=numeric(),rmse_Extra=numeric())

#GRID SEARCH
for (alph in alpha) {
  print(paste("ANSWER = 42  alpha =" , alph))
  fit_Extra = vector("list", n_fold)
  fit_Neuro = vector("list", n_fold)
  fit_Extra = lapply(1:n_fold, function(i) glmnet(feat_train[[i]],Extra_train[[i]], alpha = alph, lambda = lambda))
  fit_Neuro = lapply(1:n_fold, function(i) glmnet(feat_train[[i]],Neuro_train[[i]], alpha = alph, lambda = lambda))
        
  for (lamb in lambda) {
    print(paste("QUESTION = ??  lambda =" , lamb))
    predict_Extra = vector("numeric", n_fold)
    predict_Neuro = vector("numeric", n_fold)
    predict_Extra = sapply(1:n_fold, function(i) predict(fit_Extra[[i]], newx = feat_test[[i]], s = lamb))
    predict_Neuro = sapply(1:n_fold, function(i) predict(fit_Neuro[[i]], newx = feat_test[[i]], s = lamb))
    rmse_Extra    = rmse(unlist(predict_Extra), unlist(Extra_test))
    rmse_Neuro    = rmse(unlist(predict_Neuro), unlist(Neuro_test))
    results = rbind(results, data.frame(alpha=alph, lambda=lamb, rmse_Neuro=rmse_Neuro, rmse_Extra=rmse_Extra))
  }
}
#stopped at 12:40
#started at 1:15
View(results)

arg_min = function(v) {
  min_index= which(min(v) == v)
  min_index[1]
}

min_Neuro = results[arg_min(results$rmse_Neuro), ]
min_Extra = results[arg_min(results$rmse_Extra), ]
min_Extra; min_Neuro 
#alpha=.1, lambda=.23 and alpha=.1, lambda =.34
?glmnet

#glmnet on whole data set
#DONT FORGET to scale!! (and make sure Y is a vector)
best_fit_Extra = glmnet(scale(features), unlist(Extra), alpha = min_Extra$alpha, lambda = min_Extra$lambda)
best_fit_Neuro = glmnet(scale(features), unlist(Neuro), alpha = min_Neuro$alpha, lambda = min_Neuro$lambda)

#coefficients (named in df)
coef_Neuro = coef(best_fit_Neuro, s = min_Neuro$lambda)
coef_Extra = coef(best_fit_Extra, s = min_Extra$lambda )
coefs = as.data.frame(cbind(as.numeric(coef_Extra),as.numeric(coef_Neuro)))
colnames(coefs) = c("Extraversion", "Neuroticism")
rownames(coefs) = dimnames(coef_Neuro)[[1]]
head(coefs)
#Remove y intercept, coeffs=0
coefs = coefs[-1, ]
coefs = coefs[-which(coefs$Extra & coefs$Neuro == 0), ]

#Is there a valid reason for choosing 75% percentile???
q_Extra = quantile(abs(coefs$Extra))
q_Neuro = quantile(abs(coefs$Neuro))
under_75_ind  = which(abs(coefs$Extra) < q_Extra[4] & abs(coefs$Neuro) < q_Neuro[4])
coefs_under75 = coefs[-under_75_ind, ] 

?cor.plot
cor.plot(coefs_under75, is.corr=FALSE)
#Notice confidence is strongest negative correlation with Neuroticism, lonely is strongest posivitive corr.
#sociable, then bored and confident are best positive correlations for Extraversion

#Start time 10:16
#stopped at 12:40
#started at 1:15
# END 2:45
# ~ 4 hours
