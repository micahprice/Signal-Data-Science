# Simulated Data Regressions
# 9/13 Micah and Megan

getSamples = function(a,n) {
  y= vector(mode = "numeric",length = n)
  x=rnorm(n)
  for (i in 1:n) {
    y[i]=a*x[i] +rnorm(1, mean=0, sd=sqrt(1-a^2))
  }
  return (data.frame('x'=x ,'y'=y))
}

df=getSamples(.6, 100)
# library("ggplot2")
ggplot(df, aes(x, y)) + geom_point()+geom_smooth(method="lm")

df=getSamples(.9, 100)
ggplot(df, aes(x, y))  + geom_point() + coord_fixed( ratio=2) +geom_smooth(method="lm")
?geom_smooth

estimateSlopes = function (a, n, numTrials = 500) {
  ans=replicate(numTrials, coef(lm(y~x,getSamples(a,n)))[2])
  ans
}
head(estimateSlopes(.6,100))

ggplot(mapping=aes(x=estimateSlopes(.6,100))) + geom_histogram(bins=70)
ggplot(mapping=aes(x=estimateSlopes(.3,2500))) + geom_histogram(bins=70)
ggplot(mapping=aes(x=estimateSlopes(.1,500))) + geom_histogram(bins=70)


?rbind
rows= function(n) sapply(c(.1,.3,.5,.8,.9), function (a) sd(estimateSlopes(a,n)))
rows=lapply(c(100,500,2500,10000),rows)

rows=lapply(rows,round,2)
dfSD=(data.frame(rows))
rownames(dfSD)=c(.1,.3,.5,.8,.9)
names(dfSD)=c(100,500,2500,10000)

dfSD
#columns are n
#looks like it does somewhat depend on a, that shouldnt matter.
#must just be luck

mor_n= sapply(seq(100,10000,300), function (n) sd(estimateSlopes(.1,n)))
#very unneccessarily large
mor_n
x=seq(100,10000,300)

ggplot(mapping= aes(x, y=mor_n))  + geom_point()+geom_smooth()
#first data point is skewing it

#Didn't do p-value section
##########END####################
