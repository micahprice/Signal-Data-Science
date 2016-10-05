# Logistic Regression
# 9/27 Micah and Will
# https://github.com/signaldatascience/R-curriculum/blob/master/assignments/supervised-learning/logistic-regression/logistic-regression.pdf

library('pROC')
library('dplyr')

setwd('~/signal/week3')

df = read.csv("speeddating-aggregated.csv")

sigmoid = function(x) {
  exp(x)/(1+exp(x))
}

# Unregularized logistic regression

sum(complete.cases(df))
#539/551 are complete

#remove NAs
df = na.omit(df)

df_gender = select(df,gender,sports:yoga)
fit_gender = glm(gender ~ ., data = df_gender, family="binomial")
plot(roc(df$gender,sigmoid(predict(fit_gender,df))))
#area under curve = 0.8296

df_careers = dplyr::filter(df,career_c == 2 | career_c == 7)
df_careers = select(df_careers,career_c,sports:yoga)
#career_c contains 2 and 7's, which doesn't work.
#We could convert to 0's and 1's, instead we convert to
#factors, because:
#"For binomial and quasibinomial families the response can also
#be specified as a factor (when the first level denotes
#failure and all others success)"
df_careers[["career_c"]] = factor(df_careers[["career_c"]])
fit_careers = glm(career_c ~ ., data = df_careers, family="binomial")
plot(roc(df_careers$career_c,sigmoid(predict(fit_careers,df_careers))))
#Area under curve: 0.7594

df_race = select(df,race, sports:yoga)
df_race = filter(df_race, race == 2 | race == 4)
df_race['race']= factor(df_race[["race"]])
fit_race = glm(race ~ ., data = df_race, family="binomial")
plot(roc(df_race$race,sigmoid(predict(fit_race,df_race))))
#Area under curve: 0.6913

#Regularized Linear Regression
############################### 
#SpeedDatingDecisionStarter.R from Signal
library(glmnet)
library(dummies)
df = read.csv("speeddating-full.csv")

#Create data frame with decisions, average decision frequencies, careers and races
df = select(df, gender, iid, pid, wave, dec, attr, race, career_c)
genders = c("female", "male")
df$gender = factor(df$gender, labels = genders)
careers = c("Lawyer", 
            "Academic", 
            "Psychologist", 
            "Doctor", 
            "Engineer",
            "Creative",
            "Business",
            "RealEstate",
            "IntRelations",
            "Undecided",
            "SocialWork",
            "Speech",
            "Politics",
            "Athletics",
            "Other",
            "Journalism",
            "Architecture")
races = c("Black", "White", "Latino", "Asian", "Other")
# df$gender = factor(df$gender, labels = genders)
# df$race = factor(df$race, labels = races)
# df$career_c = factor(df$career_c, labels = careers)
agged = aggregate(df["dec"], df["iid"], FUN = mean, na.rm = T)

colnames(agged) = c("iid", "decAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("dec", "attr")], df["pid"], FUN = mean, na.rm = T)
colnames(agged) = c("pid", "decPartnerAvg", "attrPartnerAvg")
df = inner_join(df, agged)
agged = aggregate(df[c("race", "career_c")], df["iid"], FUN = mean)
agged$race = factor(agged$race, labels = races)
agged$career_c = factor(agged$career_c, labels = careers)
names(agged)
df = inner_join(df[!(names(df) %in% c("race", "career_c"))], agged)
colnames(agged)[1:3] = c("pid", "race_Partner", "career_c_Partner")
df = inner_join(df, agged)

#Cross validate regularized logistic regression at the level of waves

crossValidate = function(features, 
                         target, 
                         waves = df$wave,
                         lambdas = (1.2)^(10:(-30)), 
                         alphas = seq(0, 0.24, 0.03)){
  s = scale(features)
  s = s[,!is.nan(colSums(s))]
  rocs = expand.grid(lambda = lambdas, alpha = alphas)
  rocs$logLoss = 0
  rocs$ROC = 0 
  for(alpha in alphas){
    print(alpha)
    l = lapply(1:21, function(wave){
      trainFeatures = s[waves != wave,]
      testFeatures = s[waves == wave,]
      set.seed(1); m = glmnet(trainFeatures, target[waves != wave], 
                              alpha = alpha, 
                              lambda = lambdas,
                              family = "binomial")
      as.data.frame(predict(m, testFeatures))
    })
    predictions = do.call(rbind, l)
    #  predictions = exp(predictions/(1 + predictions))
    #CORRECTION
    predictions= sigmoid(predictions)
    rocTemp = sapply(predictions, function(cv){
      as.numeric(roc(target,cv)$auc)
    })
    rocs[rocs$alpha == alpha,"ROC"] = rocTemp[length(rocTemp):1]
  }
  rocs
}
#END OF Prewritten code

dums1 = cbind(dummy.data.frame(df['race']), dummy.data.frame(df['career_c']))
dums2 = cbind(dummy.data.frame(df['race_Partner']), dummy.data.frame(df['career_c_Partner']))

dums = cbind(dums1,dums2,select(df,decAvg,decPartnerAvg,attrPartnerAvg))

#Megans interaction term way for the 1st two
# n1 = dums1[grep("race*", names(dums1))]*df$attrPartnerAvg
# names(n1) = paste(names(dums1)[grep("race*", names(dums1))], "attrPartnerAvg", sep=":")
# n2 = dums1[grep("career_c*", names(dums1))]*df$attrPartnerAvg
# names(n2) = paste(names(dums1)[grep("career_c*", names(dums1))], "attrPartnerAvg", sep=":")

#This should work, but for some reason it doesn't work
# features = data.frame(
#   model.matrix(~ -1 + race + race_Partner + career_c + career_c_Partner +
#                attrPartnerAvg + decAvg + decPartnerAvg +
#                race:attrPartnerAvg + career_c:attrPartnerAvg +
#                race:race_Partner + career_c:career_c_Partner,df))


race=1:6
career=7:24
race_P=25:30
career_P=31:48

interaction_terms = function(df1,df2) {
   dfnew = data.frame(stupid=1:nrow(df1))
   for (i in 1:length(df1)) {
    for (j in 1:length(df2)) {
      name = paste(names(df1)[i], names(df2)[j], sep = ':')
      dfnew[name] = df1[[i]]*df2[[j]]
    } 
   }
   #remove stupid column
   dfnew[-1]
}

inter1=interaction_terms(dums[race],dums["attrPartnerAvg"])
inter2=interaction_terms(dums[career],dums["attrPartnerAvg"])
inter3=interaction_terms(dums[race],dums[race_P])
inter4=interaction_terms(dums[career],dums[career_P])

features = cbind(dums,inter1,inter2,inter3,inter4)

#remove columns with 20 or fewer entries
#(this is not exactly right; it doesn't work for
#interactions between attraction and categorical variables)
features = features[,colSums(features) > 20]

male_cv = crossValidate(features = filter(features,df$gender == "male"),
              target = df$dec[df$gender == "male"],
              waves = df$wave[df$gender == "male"])
auc_male = male_cv[male_cv$ROC == max(male_cv$ROC), ]
#best AUC is for lambda=0.03130086 alpha=0.18

female_cv = crossValidate(features = filter(features,df$gender == "female"),
                        target = df$dec[df$gender == "female"],
                        waves = df$wave[df$gender == "female"])

auc_female = female_cv[female_cv$ROC == max(female_cv$ROC), ]
#best AUC is for lambda=0.02608405 alpha=0.15

fit_race = glm( ~ ., data = df_race, family="binomial")
plot(roc(df_race$race,sigmoid(predict(fit_race,df_race))))

#What the actual plot/coefs look like for best auc value of females
female_features=scale(filter(features,df$gender == "female"))
female_features = female_features[,!is.nan(colSums(female_features))]
female_fit = glmnet(female_features,
       df$dec[df$gender == "female"],family = "binomial",
       alpha= auc_female[[2]], lambda = auc_female[[1]])
plot(roc(df$dec[df$gender == "female"],sigmoid(predict(female_fit,female_features))))
coef(female_fit)


# Multinomial Logistic Regression

df = read.csv("speeddating-aggregated.csv")
sort(table(df$career_c),decreasing = T)
# Most common: Academic (2), Business (7), Lawyer (1), Creative (6)

df = filter(df,career_c %in% c(2,7,1,6))
features = select(df, -race,-gender,-career_c)

fit_career = glmnet(scale(features), df$career_c, family = "multinomial")
coef=coef(fit_career,s=0)
coefs = do.call(cbind, coef)
coefs = as.matrix(coefs[-1,])
colnames(coefs) = c("Lawyer", "Academic", "Creative", 'Business')
library('corrplot')
#corr(coefs)
corrplot(coefs, is.corr=FALSE)

predictions = predict(fit_career,scale(features), s=0)
predictions = predictions[ , ,1]
probablities = function (preds, rownum) {
  Ls = preds[rownum, ]
  fake_probs = exp(Ls)
  ans = fake_probs/sum(fake_probs)
  #colnames(ans) = colnames(preds)
  ans
}

probablities(predictions,49)
#END