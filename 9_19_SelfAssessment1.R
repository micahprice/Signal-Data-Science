# Self Assessment 1
# 9/19 Micah 

#Start time: 10:05AM
#End time: 3:53PM


#Part 1: R and Probability
?runif
#single trial. Question: if given y, <x>?
single_trial= function () {
  x=runif(1)
  y=runif(1,max=x)
  c(x,y)
}
single_trial()

# could use replicate, something like:
# as.vector((replicate(10, single_trial())))
m_carlo= function(n) {
  k=vector("list", n) 
  for (i in 1:length(k)) k[[i]]= single_trial()
  k
}
k=m_carlo(1000)
head(k)

#make k into x,y vectors. Definitely not the best way.
x=sapply(1:length(k),function(i) k[[i]][1])
y=sapply(1:length(k),function(i) k[[i]][2])
head(x)
head(y)
library('ggplot2')
?qplot
qplot(x,y)
#must always be y<=x, evenly distributed
#so <y>=.5x  . but we want <x> as a function of y

qplot(y)
#bins
#this doesn't generalize, but it's quick and dirty for
# bin widths like w=.01
w=.01
bin=ceiling(100*y) #y's are binned
head(table(bin))

x_means=numeric(100)
for (i in 1:100) {
  in_bin_index=which(bin==i)
  #deal with which returning integer(0)
  if (length(in_bin_index)==0) {
    x_means[i]=NA
  } else {
  x_means[i]= mean(x[in_bin_index])
  }
  print(x_means[i])
}
x_means

qplot(x_means, 1:100)
# ignored the bins with 0 y values. 
# stored as NA
# makes sense? for high bin y, x must be high(er)
# for low bin y, x can be anywhere from y<x<1
new=runif(10000)
xave=(new-1)/(log(new))
qplot(xave,new)
#matches
df=data.frame(x_mc = x_means, bins=1:100,x_pred=xave, y_pred=new)
qplot(df)
ggplot(df) + 
  geom_point(aes(x=x_mc,y=bins/100)) +
  geom_smooth(aes(x=x_pred,y=y_pred))

#Part 2: Data Analysis
library('psych')
help(msq)
df=msq

library("plyr")
tbl_df=count(df)

NA_ratio=numeric(length(df))
#definitely not the simplest way
for (i in 1:length(df)) {
  name=(names(df)[i])
  nas=sum(is.na(df[[i]]))
  NA_ratio[i]=(nas/nrow(df))
  names(NA_ratio)[i]=names(df)[i]
}
?sort
NA_ratio["kindly"]
names(NA_ratio)[1:4]

NA_ratio=sort(NA_ratio, decreasing=TRUE)


is.na(df$anxious)
which(df$anxious== NA)
df$anx[6]
count(df[1])
head(table(df))
head(df[1:5])
head(df$Extraversion)

?cbind
names(df)
dfnew=cbind(df[c(1:75)],df["Extraversion"],df["Neuroticism"])
## use select(df, Extra, Neuro, active:scornful)###

####Doesnt work because it refuses to rewrite col while in loop
for (col in dfnew){
  colmean = mean(col,na.rm=TRUE)
  #print(colmean)
  #print(col)
  col[is.na(col)]=colmean
}
#####

replace_NA = function(df) {
  means=lapply(df, function(c) mean(c,na.rm=TRUE))
  
  for (i in 1:length(df)) {
    df[[i]][is.na(df[[i]])]=means[[i]]
  }
  df
}

dfnew=replace_NA(dfnew)
head(dfnew)

?ggplot
ggplot(dfnew) + geom_histogram(aes(dfnew$Extraversion)) 
ggplot(dfnew) + geom_histogram(aes(dfnew$Neuroticism))
ggplot(dfnew) + geom_density(aes(dfnew$Extraversion))
ggplot(dfnew) + geom_density(aes(dfnew$Neuroticism))

ggplot(dfnew) + geom_smooth(aes(x= dfnew$Neuroticism, y=dfnew$Extraversion ))

dfnew[c("Extraversion")]

#Whats the easy way to remove a column whose name you know? (easier to just cbind?)
# doesnt work:  dfneuro=dfnew[-"Extraversion"]
dfneuro=dfnew[-(length(dfnew)-1)]
dfextra=dfnew[-(length(dfnew))]

?lm

lr_neuro=lm(Neuroticism ~ .,dfneuro)
lr_extra=lm(Extraversion ~ . , dfextra)
head(sort(abs(coef(lr_neuro)),decreasing=TRUE),11)
head(sort(abs(coef(lr_extra)),decreasing=TRUE),11)

#Extraversion is correlated with expected emotions like sociable, confident, and excited.
#upset, bored and mayybe lonely make sense, but bored, depressed and tranquil are surprising.
#unfortunately only sociable really sticks out on its own. It would be important to check for the crosscorrelations here
#because many of these emotions are very similar.

#Neuroticism seems like a complicated idea. Confidence seems a strange (largest) correlation,
# but Neuroticism does appear to be a grab bag of seemingly unrelated emotions, which is sort of what we are seeing
# with confidence, guilt, delight, intensity, nervousnes etc.


# PART 3, SQL QUERIES

# The difference between WHERE and HAVING is that HAVING takes a logical concerning some sort of aggregate function, 
# explicitly filtering the GROUP BY command. WHERE comes before GROUP BY, so if you had, say, a SUM in your WHERE statement
# I don't think GROUP BY could do its job properly.
# 
# 
# SELECT DISTINT Salary FROM Employees
# WHERE Salary < 
#       ( SELECT DISTINCT Salary FROM Employees
#         ORDER BY Salary DESC
#         LIMIT 1)
# ORDER BY Salary DESC
# LIMIT 1
# 
# SELECT DISTINT Salary FROM Employees
# WHERE Salary < 
#       ( SELECT MAX(Salary) FROM WORLD)
# ORDER BY Salary DESC
# LIMIT 1
# 
# THIS ONE IS CLOSISH
#   
# SELECT  MAX(Salary)
#       WHERE Salary != (SELECT MAX(Salary) FROM Employees))
# FROM Employees
# 
# 
# INNER JOIN only includes data that has keys in common from both sets of data . LEFT JOIN uses all 
# the keys of the left set (thus includes all the rows), allowing you to access data that the left set keeps track of
# but the right set doesn't . RIGHT JOIN does the same with the right set.
# The example of soccer game data vs goal data makes it clear for me. The goal data only keeps track of games where a
# goal was scored, so a simple JOIN would not include games where the score was 0-0. If you want to consider those games, 
# you need to have a LEFT JOIN (and of course can only access data from the 0-0 games from the game data)
# 
# 
# SELECT course_name, faculty_name
# FROM FACULTY JOIN  
#    (SELECT course_name, faculty_id
#    FROM courses JOIN course_faculty ON course_id=course_id)
#         ON faculty_id = faculty_id


#ENDED at 3:53PM
