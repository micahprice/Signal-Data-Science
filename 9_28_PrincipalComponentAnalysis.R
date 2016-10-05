# Principal Component Analysis
# 9/28: Michelle and Micah

# Let p = prcomp(df, scale.=TRUE) be the result of PCA run on
# a data frame.  Then:
#   -p$rotation gives matrix w/ rows corresponding to columns of df,
#   columns for PCs, and 'loadings' for each row (coefficients of w_1=A*x2 +B*x2 etc)
#   -p$x is principal component scores (w_1 given x1_i, x2_i etc)
#   -p$sdev gives sqrt(eigenvalues) of covariance matrix) ~nth value is relative proportion of
#    variance explained by nth principal component
#    Also, p%sdev[n]/sum(p$sdev) is the proportion of variance explaine by nth component

library('psych')
library('dplyr')
df=msq
df=select(df,active:scornful)
colSums(is.na(df))
#note variables with over 1k NA's
df=df[ ,colSums(is.na(df)) < 1000]
df = df[rowSums(is.na(df)) == 0, ]

p = prcomp(df, scale. = TRUE)
head(p$rotation[,10])
ncol(p$x)

top = function(n,pca) {
  orders =order(abs(pca$rotation[,n]), decreasing = TRUE)[1:10]
  pca$rotation[orders,n]
}

library('corrplot')
corrplot(p$rotation[,1:10], is.corr = FALSE)

for (i in 1:10) { print(i); print(top(i,p)) }
colnames(p$rotation)[1:5] = c("hyperactive", "meditative", "caffinated", "well-rested", "hostile/assertive")

class(p$sdev)
library(ggplot2)
length(p$sdev)
qplot(,p$sdev)


#PCA on the msq dataset
install.packages("DAAG")
library(DAAG)

df2 = msq
df2=select(df2,Extraversion:Neuroticism,active:scornful)
#note variables with over 1k NA's
df2 = df2[ ,colSums(is.na(df2)) < 1000]
df2 = df2[rowSums(is.na(df2)) == 0, ]

rmse = function(x,y) sqrt(mean((x-y)^2))

p = prcomp(select(df2,active:wide.awake), scale. = TRUE)
test = select(df2,Extraversion:Neuroticism)
n = ncol(p$rotation)
rmse_extra = numeric(n)
rmse_neuro = numeric(n)

for (i in 1:n) {
   test = cbind(test, p$x[,i])
   colnames(test)[ncol(test)] = paste0("PC",i)
  
   #need sumss=0 when plotit=False?
   # HAVE TO USE lm inside formula or it all goes to hell
  sumss = 0
   fit_extra = cv.lm(data = test, form.lm = formula(lm(Extraversion ~ . - Neuroticism, test)), m = 10, plotit = FALSE, printit = FALSE)
   fit_neuro = cv.lm(data = test, form.lm = formula(lm(Neuroticism ~ . - Extraversion, test)), m = 10, plotit = FALSE, printit = FALSE)
  
  rmse_extra[i] = rmse(fit_extra$cvpred, test$Extraversion)
  rmse_neuro[i] = rmse(fit_neuro$cvpred, test$Neuroticism)
}
qplot( , y = rmse_extra)
qplot( , y = rmse_neuro)


#PCA on the speed dating dataset
library('pROC')
setwd('~/R')
list.files()
df = read.csv("speeddating-aggregated.csv")
#lose NAs
df = df[rowSums(is.na(df)) == 0, ]
features = select(df, sports:yoga)
p = prcomp(features, scale. = TRUE)
corrplot(p$rotation[,1:10], is.corr = FALSE)
#interpret at your own risk
for (i in 1:10) { print(i); print(top(i,p)) }
qplot(,p$sdev,xlab = "n")

sigmoid = function(x) {
  exp(x)/(1+exp(x))
}

n = length(p$sdev)
df_gender = df['gender']
gend_auc = numeric(n) 
gend_coef = vector("list",n)


#Gender
for (i in 1:n) {
df_gender = cbind(df_gender,p$x[,i])
colnames(df_gender)[ncol(df_gender)] = paste0("PC",i)

 fit_gender = glm(gender ~ ., data = df_gender, family="binomial")
 gend_auc[i] = roc(df_gender$gender,sigmoid(predict(fit_gender,df_gender)))$auc
 gend_coef[[i]] = coef(fit_gender)
}
print(max(gend_auc),digits=5) #max area under curve =.8296 w/ all PCs
#this is literally the same as yesterday. We didn't cross validate so we think all we've done is sort of rearrange the variables
qplot( ,gend_auc)
lapply(1:17,function(x) gend_coef[[x]])

#Race
df_race = df['race']
x_new = p$x[(df_race == 2 | df_race == 4), ]
df_race = filter(df_race, race == 2 | race == 4)
df_race['race']= factor(df_race[["race"]])
race_auc = numeric(n) 
race_coef = vector("list",n)

for (i in 1:n) {
  df_race = cbind(df_race,x_new[,i])
  colnames(df_race)[ncol(df_race)] = paste0("PC",i)
  
  fit_r = glm(race ~ ., data = df_race, family = "binomial")
  race_auc[i] = roc(df_race$race, sigmoid(predict(fit_r,df_race)))$auc
  race_coef[[i]] = coef(fit_r)
}
print(max(race_auc),digits = 5) #max area under curve = .6914 w/ all PCs
#this is literally the same as yesterday. We didn't cross validate so we think all we've done is sort of rearrange the variables
qplot( ,race_auc)
lapply(1:17,function(x) race_coef[[x]])

#skipping careers

#On to unregularized multinomial logistic regression on careers using glmnet
library("glmnet")
# Most common: Academic (2), Business (7), Lawyer (1), Creative (6)
df_career = filter(df,career_c %in% c(2,7,1,6))
str(df_career)
features = select(df_career,-career_c,-gender,-race)
names(features)
df_career = factor(df_career[['career_c']])
df_career

fit = glmnet(scale(features), df_career, family = "multinomial")
coefs = coef(fit, s = 0)
predictions = predict(fit, scale(features), s = 0)
predictions
pca_predictions = prcomp(scale(as.data.frame(predictions)))
pca_predictions
rownames(pca_predictions$rotation) = c("Lawyer", "Academic", "Creative", "Finance")
corrplot(pca_predictions$rotation, is.corr = FALSE)
qplot(,pca_predictions$sdev)
pca_predictions$sdev[4]^2/sum(pca_predictions$sdev^2)

# The corrplot shows the correlation between business-like vs. non-business-like professions. PCA2 show 
# the differences between Academics and Creatives, etc.

#END