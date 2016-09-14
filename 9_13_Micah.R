vec= c(height = 48,  127, ismale= 1)
names(vec)
vec2 = c()
names(vec2)
vec2(1,2,3,4)
names(vec2)=c("first", "second", "a","a")
vec["height"]
x=c(a=1,a=2,b=3)
x
names(x)
x["a"]

x=list("a",1,T)
typeof(x)

combine = function(a,b) {
  ab=list(c(a,b))
  return(ab)
}


a=list(list(1,2), list(3,4))
str(a)
a
typeof(unlist(a))
a=as.vector(a)
typeof(a)

x
x=list(a="yes", hello= T, yo =c(5,4,3,2))
x["yo"]
x$yo
df=data.frame(matrix(1:20, nrow=4))
head(df)
names(df)
names(df["X3"])
length(df)
nrow(df)
ncol(df)

typeof(df)
class(df)
rownames(df)

as.data.frame(x)
x
str(x)
d=list(list("w","e","r","e","r"),list(2))
str(d)
data.frame(d)
str(d)
str(data.frame(d,x))

dim(df)
df1=data.frame(matrix(1:10,nrow=2))
df1
df
rbind(df,df1)
df3=data.frame(matrix(1:16,nrow=4))
df3
cbind(df,df3)
colnames(df)

dfr =data.frame(matrix(1:10,nrow=2))

names(df)[names(df)=='']

rbind(df, dfr)

vec = c(1,2,3)
rep(vec, 2)

lec = list(vec,2)
do.call(rep, lec)

dfdf = rep(df,2)
str(dfdf)
clasS(dfdf)
dfdf = data.frame(dfdf)
#It changes names for the repetitive column names
dim(dfdf)

#Hey, so do.call(func, args) may be useful sometime but...
#Watch out for args being read as unintended!

nulldf = data.frame()
class(nulldf)
nulldf


#Check contraints of factor conversion
pool = letters[1:12]
pool2 = rep(c("m","f"), 6)
pool4 = rep(pool[1:4],3)
pools = list(pool, pool2, pool4)

df12 = data.frame(pool)
str(df12)
#Wow! Still being read as factors at 12 items!

dfchar = data.frame(pool, stringsAsFactors = FALSE)
str(dfchar)

mfdf = data.frame(pool2)
str(mfdf)

unique(mfdf)
unique(df12)

df
x=2:6
x[c(3,1)]
x[c(T,F,T,F,T)]
y2 = c("m", "f", "m", "m")
dfnew=cbind(df, y2)
dfnew[dfnew['y2'] == 'm']

x=1:5
x
df[c(T,F,T,F,T)]
df[c(T,F)]
x[-c(2,1)]
x[c(-3,-1)]
x
x[c(3,1)]
x[-7]
df[-3]
df[2,3]
df
x[3,3]
z[3.5]
x[c(0,-2)]
x[0]
df[0]
x[0:4]
x[c(T,T,F)]
x[3]=NA
x[3]
x[c(T,F)]
df[c("X2","X2")]
x=c(a=1,2:5)
x
y=x["a"]
names(y)
is.atomic(y)

x=list(1:5)
x[3]
str(x)
x[4]
x[1]
?list
x
str(x)
x[1]
x[[2]]=1:3
x[[1]][3]=list(1:5)
x[[2]]
str(x)
x[[1]][3:4]
d=list(1,2,3,4,5)
list(1:5)
d[10]=4
d





#initialize full vectors before filling
y=vector(mode = "list", length = 20)

slowfill=function(n){
  x=list()
  for (i in 1:n) {
   x[i]=i
  }
}

system.time(replicate(100,slowfill(1000)))

fastfill=function(n){
  x=vector(mode = "list", length = n)
  for (i in 1:n) {
    x[i]=i
  }
}
system.time(replicate(100,fastfill(1000)))

df = data.frame(matrix(1:100, nrow=10))
df

df[2:4,3:6]
df[3,5]
df[3:6]
df[c(2,6,7,9)]
df[,c(2,6,7,9)]

df[c(2,6,7,9),]

df["X3"]
df[3] #or df[,3]

df$X3 # or df[[3]]
df[[3]]
df[["X3"]]

df[3,4]
df[[4]][3]

###### SOLUTIONS FROM SIGNAL

# Three different implementations of a nesting depth function

# Checks if any of the elements of L are themselves a list
contains_list = function(L) {
  if (length(L) == 0) {
    return(FALSE)
  }
  for (i in 1:length(L)) {
    if (is.list(L[[i]])) {
      return(TRUE)
    }
  }
  return(FALSE)
}

# Evaluates the nesting depth of L by unlisting it one level at a time until none of the elements are lists
nesting_depth = function(L) {
  count = 1
  while (contains_list(L)) {
    L = unlist(L, recursive=FALSE)
    count = count + 1
  }
  return(count)
}

# Here's another implementation of nesting_depth() which uses a recursive solution
nesting_depth2 = function(L) {
  idx_lists = c()
  for (i in 1:length(L)) {
    if (is.list(L[[i]])) {
      idx_lists = c(idx_lists, i)
    }
  }
  
  if (length(idx_lists) == 0) {
    return(1)
  }
  
  depths = c()
  for (i in 1:length(idx_lists)) {
    depths = c(depths, nesting_depth(L[[idx_lists[i]]]))
  }
  depths = 1 + depths
  
  return(max(depths))
}

# Even simpler recursive solution
nesting_depth3 = function(L, depth=1) {
  if (!is.list(L)) {
    return(depth-1)
  } else if(length(L) == 0) {
    return(depth)
  } else {
    return(max(unlist(sapply(L, nesting_depth3, depth=depth+1))))
  }
}

test = list(list()) # depth 2
test2 = list(list(list(list(1)))) # depth 4
test3 = list("asdf", list(list(1), 2, list(2, list(3, list())), 4, 5), 6, 7) # depth 5

nesting_depth(test)
nesting_depth(test2)
nesting_depth(test3)

nesting_depth2(test)
nesting_depth2(test2)
nesting_depth2(test3)

nesting_depth3(test)
nesting_depth3(test2)
nesting_depth3(test3)

##################################
unlist(test3)


domino = function(n) {
  ans=vector("list" , (n+1)*n/2 + 1 + n)
  #print(length(ans))
  count=1
  for (a in 0:n) {
    for (b in a:n) {
      ans[[count]]=c(a,b)
      count=count+1
    }
  }
  #SETTING GLOBAL VARIABLE DOMINOS
  #dominos<<-ans
  return (ans)
}

str(domino(4))
domino(2)
domino(16)

a=list(1,2,3)
a[[2]]=c(2,6)
a[2]=8
str(a)
str(a[[2]])
a[2]=list(c(2,6))
)
domino(4)[1]
?is.null

a=table(unlist(domino(4)))
typeof(a)
str(a)
names(a)
a$"0"
a["0"]
a[["0"]]
a[[]]
a
a==6
?any
any(a==6)
 ds

## THERE"S SOME ERROR HERE
 is_circle = function (L) {
   doms = unlist(L)
   if (doms[1] == doms[length(doms)]) {
     return (FALSE)
   }
   for (i in seq(2,(length(doms)-2),2) {
     if (doms[i] != doms[i+1]) {
       return (FALSE)
     }
   }
   return (TRUE)
 }  
 

 x=1:5
 x[1:2]=c(10,11)
 x
 ?rep
 x[x %% 2==0]=100
 x
 x=1:5
 x[6]
 mtcars
 str(mtcars)
 mtcars[1:20,]
 df[3:10]=NA
 df
 df[is.na(df)]=0
 df
 
 x=c("a","b","a","a","b","x","b","a")
 y=c(1,2,3,1,1,2,2)
 fruits = c(a="apple", b="banana", x=NA)
 fruits[x]
 fruits[y]
 
 mtcars
 
want=order(names(mtcars))
want
names(mtcars)[want]
mtcars[,want]

?sample
sample(10,3)
sample(3,6)  #error
sample(mtcars, length(mtcars)) # randomly switches columns

#randomly switchs rows
nrow(mtcars)
rand_cols=sample(nrow(mtcars))
mtcars[rand_cols,]    


k=6
cols=sample(k, replace=T)
 mtcars[,cols]
 
##ENDED after sampled with replacement bullet. 
 #SKIPPED TO 2nd pdf
 
 getSamples = function(a,n) {
   y= vector(mode = "numeric",length = n)
   x=rnorm(n)
   for (i in 1:n) {
     y[i]=a*x[i]
   }
   
   return (data.frame('x'=x ,'y'=y))
 }
 
 df=getSamples(.6, 100)
 df
 ??ggplot
 library("ggplot2")
 
 ggplot(df, aes(x, y)) + geom_point()

 df=getSamples(.9, 100)
 ggplot(df, aes(x, y))  + geom_point() + coord_fixed( ratio=2) +geom_smooth()
 ?ggplot
 ?geom_point
 