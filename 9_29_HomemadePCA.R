# Homemade Principal Component Analysis Function
# 9/29: Micah

library(dplyr)
library('psych')

solve_pca = function (w,X) {
  #Takes in Dataframe X
  w = matrix(w, ncol=1)
  -(t(w) %*% t(X) %*% (X) %*% w / (t(w)%*%w))
}
#TEST
set.seed(1)
X = matrix(sample(0:1,100,replace=TRUE),nrow=10)
X=scale(X)
solve_pca(1:10,X) #-6.89

#functions
vnorm = function(v) sqrt(sum(v*v))
find_angle = function(us,them){
  norms = vnorm(us)*vnorm(them)
  acos(sum(us*them)/norms)*180/pi
}

#START
#MSQ data
df2 = msq
df2=select(df2,Extraversion:Neuroticism,active:scornful)
#Remove NAs ect.
df2 = df2[ ,colSums(is.na(df2)) < 1000]
df2 = df2[rowSums(is.na(df2)) == 0, ]

#prcomp stores nth PC1 weights in prcomp()$rotation[ ,n]
p = prcomp(select(df2,active:wide.awake), scale. = TRUE)

#Guess w
w=rep(1,ncol(X))

#find PC1
X = scale(select(df2, active:wide.awake))
our_pc1 = optim(w, solve_pca, X = X, control = list(maxit=10000), method ='CG')

us = our_pc1$par
them = p$rotation[,1]
find_angle(us,them) #2.72 degrees off of prcomp with BFGS, ~0 with CG

#Find PC2
sub1 = (X %*% matrix(us/vnorm(us)) %*% t(matrix(us/vnorm(us))))
X2 = X - sub1 
our_pc2 = optim(w, solve_pca, X = X2, control = list(maxit=10000), method ='CG')
us = our_pc2$par
them=p$rotation[,2]
find_angle(us, them) #179.6 or .04 degrees BFGS, ~0 with CG

#Find PC3
sub2 = (X %*% matrix(us/vnorm(us)) %*% t(matrix(us/vnorm(us))))
X3 = X2-sub2
our_pc3 = optim(w, solve_pca, X = X3, control = list(maxit=10000), method ='CG')
us = our_pc3$par
them = p$rotation[,3]
find_angle(us,them) #179.6 or .04 degrees BFGS, ~0 with CG

#This could easily be iterated with a bit more objected oriented programming (see solutions)

