# R: Functional Programming
#9/15 Micah and Rafael

df= data.frame(matrix(1:100, nrow=10))
df

#loop for means
means=c()
for (i in 1:ncol(df)) {
  means = c(means, mean(df[[i]]))
}
means

#or use sapply()

means = sapply(1:ncol(df)), function(i) mean(df[[i]])
means


double = function(x) {
  c(2*x, x^2)
}

sapply(1:10, double)

doubles=c()

for (i in 1:10) {
doubles=c(doubles, double(i))  
}
doubles

lapply(1:10, function(x) 2*x)

theclass = lapply(1:length(mtcars), function(x) class(mtcars[[x]]))

head(mtcars)
str(mtcars)

unlist(theclass)

standardize = lapply(1:length(mtcars), function(x) )

standardizedf = function(df) {
  changes=lapply(df, function(c) (c-mean(c))/sd(c))
  df[1:ncol(df)] = changes
  df
}

#making new df is unneccesary
changes=lapply(df, function(c) (c-mean(c))/sd(c))
newdfchg = data.frame(changes)
names(newdfchg) = names(df)

dfsimple = data.frame(matrix(1:9, nrow=3))
names(dfsimple) = c("a","b","c")
standardizedf(dfsimple)


standardize_numeric = function(df) {
  newdf = lapply(df, function(column) {
    if (is.numeric(column)) {
      (column-mean(column))/sd(column)
    } else {
      column
    }
  })
  newdf= data.frame(newdf)
  newdf
}

df = data.frame(matrix(1:100, nrow = 10))
df[1:5]= lapply(df[1:5], as.character)
standardize_numeric(df)
str(df)
df
typeof(df)
class(df)

double = function(x) 2*x



#only works with SINGLE INPUT functions
my_lapply = function(args, func) {
  output=vector("list",length(args))
  n=1
  for (x in args) {
   output[[n]] = func(x)
   n=n+1
  }
  output
}

my_lapply(c(4,5,5), double)


subtract_left_column = function (df) {
  newdf = lapply(rev(1:length(df)), function(i) {
  if (i>1){
    return (df[[i]]-df[[i-1]])
  } else {
    df[[i]]
  }
  })
  df[rev(1:length(df))] = newdf
  df
}  

df = data.frame(matrix(1:100, nrow = 10))
df
subtract_left_column(df)

multiply = function (x, k=2) k*x
sapply(1:10, function(x) multiply(x, k=5))
sapply(1:10, multiply, k=3 )


?mean
L=lapply(1:5, function(x) sample(c(1:4,NA)))
L
sapply(L, mean, na.rm =TRUE)

df = data.frame(matrix(1:100, nrow = 10))
new_names= lapply(names(df), function(name) name=paste(name, "n", sep = "_"))
names(df)=new_names  
head(df)

new_names= lapply(1:length(df), function(i) names(df)[i]=paste(names(df)[i], i, sep = "_"))
names(df)=new_names  
head(df)


library("timeit")
install.packages("timeit")
?timeit

sum(sapply(10:100, function(i) i^3+4*i^2))
sum(sapply(10:100, function(i) ((2^i/i)+(3^i)/(i^2))))
?sum
sapply(seq(3,6,.1), function(x) exp(x)*cos(x) )



prep_for_lr= function(df) {
  n = length(df)
  dfnew=data.frame(matrix(nrow=nrow(df)))
  for (i in 1:n) {
    col = df[[i]]
    if (is.factor(col)) {
      for (j in 2:length(levels(col))) {
        current_level=levels(col)[j]
        level_index = which(current_level==col)
        new_col=numeric(length(col))
        new_col[level_index]=1
        new_name= paste(names(df[i]),j, sep = "_")
        dfnew[new_name]=new_col
      }
      
    } else {
      dfnew[names(df[i])]= col
    }
  }
  dfnew=dfnew[-1]
  return(dfnew) 
}

#DOESNT WORK

prep_linreg = function(df) {
  #new_df=data.frame(matrix(1:nrow(df)*ncol(df), nrow=nrow(df), ncol=ncol(df)))
  
  new_df_stuff = lapply(names(df), function(n) {
    
    new_levels=lapply(2:length(levels(df[[n]])), function(lev) {
      lev_index=(lev==df[[n]])
      
      new_col=numeric(nrow(df))
      new_col[lev_index]=1
      
      new_name=paste(n, lev, sep="_")
      return (list(new_col, new_name))
    })
  })
unlist(new_df_stuff, recursive=F)
}

test=data.frame(num=as.factor(c(2,3,3,3,2)), gender= c("male", "female","male", "female", "dunno"), morfactor= as.factor(c(12,12,12,7,12)))
prep_linreg(test)
lapply(1:10, function(x) return(c(x,x^2)))


#apply()
m = matrix(1:9, nrow=3)
m
apply(m, 2, mean)
apply(m, c(1,2), mean)


values = lapply(1:10, function(x) rnorm(10))
weights = lapply(1:10, function(x) rnorm(10))
str(weights)
str(values)

weighted= Map(weighted.mean, values, weights)
weighted= Map(function(v, w) sum(v*w)/length(v), values, weights)
head(weighted)

a=b=c(1,2,3,4,5,6)
adds =function(x,y) x+y
Map(adds, a, b)


#SUM function
Reduce("+",a)
Reduce(function(x,y) x+y , a)

c=c(2,4,6,8,10)
e=c(28,29,4)
list_of_vecs=list(a,b,c,e)
Reduce(union,rev(list_of_vecs))
Reduce(intersect, rev(list_of_vecs))

check_Reduce = function(func, vec) {
  ans=Reduce(func,vec)
  if (all(Reduce(func, rev(vec))==ans)) {
    return (ans)
  } else {
    return(NA)
  }
}

check_Reduce(union, list_of_vecs)
#intersect works

my_Reduce = function(func,vec) {
  ans=func(vec[1],vec[2])
  for (arg in vec[3:length(vec)]){
    ans=func(ans,arg)
  }
  ans
}

my_Reduce(function(x,y) x+y,a)
my_Reduce(union, list_of_vecs)
#doesn't work for a list of vecs

#infinite continued fraction
d=rep(c(1,2),100)
Reduce(function(x,y) (y+1/x), d)
f=100:2
Reduce(function (x,y) sqrt(1+y*x), f)

g=c(1,2,3,4,4,5,5)
is_five = function(x) x==5
Find(is_five, g)
Filter(is_five, g)
Position(is_five, g, right=T)

  #Any would do this
Reduce("|", is_five(f))
  #All would do this
Reduce("&", is_five(c(5,5,5,5,5,5)))
getwd()
