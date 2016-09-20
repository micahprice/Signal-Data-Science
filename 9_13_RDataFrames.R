# R: Data Frames 
# 9/13 Micah and Megan

#chunks of this were done in Megan's script

vec= c(height = 48,  127, ismale= 1)
names(vec)
vec2 = c()
names(vec2)
vec2(1,2,3,4)
names(vec2)=c("first", "second", "a","a")
vec["height"]
x=c(a=1,a=2,b=3)
names(x)
x["a"]

#Lists
x=list("a",1,T)
typeof(x)

combine = function(a,b) c(a,b)
combine(list(1,2), list(3,4,"hi"))
list(1,2,3,4,"hi")

a=list(list(1,2,list(3)), list(3,4));a ;str(a)
unlist(a); str(unlist(a))
typeof(unlist(a))
a=as.vector(a) 
#doesn't work. Lists ARE vectors(but not atomic vectors).Use unlist
is.vector(a)
x=list(a=c("yes","fine"), hello= T, yo=c(5,4,3,2))
x["yo"]; x$yo

#Data Frames
df=data.frame(matrix(1:20, nrow=4))
head(df)
names(df) #same as colnames. can use rownames()
names(df["X3"]);length(df);nrow(df);ncol(df)
typeof(df);class(df);rownames(df)

as.data.frame(x) #duplicates empty values
str(x)
d=list(list("w","e","r","e","r"),list(2))
str(d);f=data.frame(d);str(f)
data.frame(d,x) #smashes two dfs together

dim(df)
df1=data.frame(matrix(1:10,nrow=2))
df1;df; rbind(df,df1)
df3=data.frame(matrix(1:16,nrow=4))
df3;cbind(df,df3) #duplicates names

vec = c(1,2,3); rep(vec, 2)
lec = list(vec,2) #args for rep
do.call(rep, lec)

dfdf = rep(df,2);dfdf
class(dfdf) #makes dataframes into lists!
dfdf = data.frame(dfdf); dfdf
#It changes names for the repetitive column names
###Hey, so do.call(func, args) may be useful sometime but...
#Watch out for args being read as unintended!
nulldf = data.frame(); class(nulldf); nulldf

#Subsetting
#Check contraints of factor conversion
pool = letters[1:12]
pool2 = rep(c("m","f"), 6)
pool4 = rep(pool[1:4],3)
pools = list(pool, pool2, pool4)
df12 = data.frame(pool); str(df12) #Being read as factors!
dfchar = data.frame(pool, stringsAsFactors = FALSE)
str(dfchar) #Read as char
unique(pool2); unique(df12)

df
x=2:6; x; x[c(3,1)]; x[c(T,F,T,F,T)]
y2 = c("m", "f", "m", "m")
dfnew=cbind(df, y2);dfnew
dfnew[dfnew['y2'] == 'm']
df[c(T,F,T,F,T)];df[c(T,F)]
x; x[-c(2,1)]; x[c(-3,-1)]; x[c(3,1)] #use - to delete
x[-7] #nothing
df; df[-3]; df[2,3]; df[-2,-3]
x[c(0,-2)]; x[0]; df[0]; x[0:4]; x[c(T,T,F)]
x[3]=NA; x[3]; x[c(T,F)]
df[c("X2","X2")]
x=c(2:5, a=1); x
y=x["a"]; names(y); is.atomic(y)

#initialize full vectors before filling (way faster)
y=vector(mode = "list", length = 20) #How to make empty list
slowfill=function(n){
  x=list()
  for (i in 1:n) x[i]=i
}
system.time(replicate(100,slowfill(1000)))

fastfill=function(n){
  x=vector(mode = "list", length = n)
  for (i in 1:n) x[i]=i
}
system.time(replicate(100,fastfill(1000)))

#Advanced Subsetting (LISTS)
x=list(1:5); x; x[3]; str(x); x[1]
x[[2]]=1:3; x
x[[1]][3]=list(1:5);x #1d lists are turned into vectors
x[[1]][3:4]
d=list(1,2,3,4,5); d; list(1:5); d[10]=4; d

df = data.frame(matrix(1:100, nrow=10))
df; df[2:4,3:6]; df[3,5]; df[3:6]
df[c(2,6,7,9)]; df[,c(2,6,7,9)]; df[c(2,6,7,9),]
df["X3"]; df[3] #or df[,3]
df$X3 ; df[[3]]; df[["X3"]] #same thing
df[[4]][3]

#Supplemental Exercises


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

nesting_depth(test); nesting_depth(test2); nesting_depth(test3)
nesting_depth2(test); nesting_depth2(test2); nesting_depth2(test3)
nesting_depth3(test); nesting_depth3(test2); nesting_depth3(test3)

##################################
unlist(test3)

uniq_domino = function(n) {
  ans=vector("list" , (n+1)*n/2 + 1 + n) # use combinatorics
  #print(length(ans))
  count=1
  for (a in 0:n) {
    for (b in a:n) {
      ans[[count]]=c(a,b)
      count=count+1
    }
  } #could use Lapply here?
  return (ans)
}
str(uniq_domino(4))
uniq_domino(2)
tail(uniq_domino(16))
uniq_domino(4)[1]
a=table(unlist(uniq_domino(4))); a; names(a); #table tabulates
typeof(a); str(a); a["0"]; any(a==6)

## THERE'S SOME ERROR HERE (UPDATE. DONT USE "L" as variable!)
 is_a_circle = function (alist) {
   doms = unlist(alist)
   if (doms[1] != doms[length(doms)]) {
     return (FALSE)
   }
   for (i in seq(2,(length(doms)-2),2)) {
     if (doms[i] != doms[i+1]) {
       return (FALSE)
     }
   }
   return (TRUE)
 }  
test=list(list(1, 2), list(2, 3), list(3, 1))
is_a_circle(test)


 x=1:5; x[1:2]=c(10,11); x; x[x %% 2==0]=100; x
 head(mtcars);mtcars[1:20];
 mtcars[1:20,]
 df[3:10]=NA; df; df[is.na(df)]=0; df
 
 x=c("a","b","a","a","b","x","b","a")
 y=c(1,2,3,1,1,2,2)
 fruits = c(a="apple", b="banana", x=NA)
 fruits[x]
 fruits[y]
 
want=order(names(mtcars)); want
names(mtcars)[want]; mtcars[,want]

?sample
sample(10,3)
sample(3,6)  #error
sample(mtcars, length(mtcars)) # randomly switches columns
#randomly switchs rows
nrow(mtcars); rand_cols=sample(nrow(mtcars)); mtcars[rand_cols,]    

k=6; cols=sample(k, replace=T); mtcars[,cols]

randcols = function(df, m) {
  if (m>length(df)) return ("Error")
  start=sample(1:(length(df)-m+1),1)
  print(start)
  dfnew=(lapply(start:(start+m-1),function(i) df[[i]]))
  print(names(dfnew))
  names(dfnew)=names(df[start:(start+m-1)])
  data.frame(dfnew)
}
head(randcols(mtcars, 6))
head(mtcars)
names(mtcars)

names(mtcars) %in% 'disp' #PUT large vector to scan first
col_rm = function (df, colname) {
  location = names(df) %in% colname
  df=df[!location]
  df
}
head(col_rm(mtcars,"disp"))

#Write a function that uses grep() and strsplit() t
grep("[a-c]", letters, value=TRUE)
?strsplit
x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
strsplit(a, "e")
letters

#sapply is very smart and keeps track of names for you
#grepl finds each letter in names(df), returns T or F, sum counts them
sapply(letters, function(i) sum(grepl(i,x=names(mtcars))))

gsub("thank",'screw', "Well thank you, man")
names(mtcars)[1]='m p g'
names(mtcars)=gsub(" ",".",names(mtcars))
gsub("$",".mod", names(mtcars))
#there are lots of special patterns (from PERL?) for beginning/end of line, etc

nam=names(mtcars)
substr(nam, 1, nchar(nam)-4)
# substr takes what you want to keep as input and removes everything else

Reduce(function(x,y) paste(x,y,sep = "_"),rownames(mtcars))
#Not how your supposed to do it on this day

#Spiral Problem
df=data.frame(matrix(1:16, nrow=4))
df
ans=numeric(nrow(df)*ncol(df))
first=unlist(df[,1])
df=df[,-1]
second=unlist(df[nrow(df),])
df=df[-nrow(df),]
third= rev(unlist(df[,ncol(df)]))
df=df[,-ncol(df)]
fourth= unlist(rev(df[1,]))
df=df[-1,]
#and so on. You could put the slices in the ans as you go.
fifth=unlist(df[,1])
df=df[,-1,drop=FALSE]
#drop = False needed in all of these
df
sixth=unlist(df[nrow(df),])
df=df[-nrow(df), ,drop=FALSE]
ans=c(first,second,third,fourth,fifth,sixth)
ans
#then seventh, would need a while loop for each cut to check if 
#there are 0 dimension. Clockwise would complicate things, but just with rules
#about when to rev or not


######Solutions####
# Traverses data frame in counterclockwise spiral
# dir = 1 2 3 or 4 <=> left bottom right or up
get_slice = function(df, dir, clockwise=FALSE) {
  if (dir == 1) {
    slice = df[, 1]
    df = df[, -1, drop=FALSE]
  } else if (dir == 2) {
    slice = df[nrow(df), ]
    df = df[-nrow(df), , drop=FALSE]
  } else if (dir == 3) {
    slice = df[, ncol(df)]
    df = df[, -ncol(df), drop=FALSE]
  } else {
    slice = df[1, ]
    df = df[-1, , drop=FALSE]
  }
  
  if (!clockwise & (dir == 3 | dir == 4)) {
    slice = rev(slice)
  } else if (clockwise & (dir == 1 | dir == 2)) {
    slice = rev(slice)
  }
  
  names(slice) = NULL
  list(slice=slice, df=df)
}

spiral = function(df, clockwise=FALSE) {
  stop = FALSE
  nums = c()
  iter = 1:4
  if (clockwise) {
    iter = rev(iter)
  }
  while (!stop) {
    for (dir in iter) {
      tmp = get_slice(df, dir, clockwise)
      slice = tmp$slice
      df = tmp$df
      
      nums = c(nums, slice)
      stop = 0 %in% dim(df)
# 1:10 %in% c(1,3,5,9)  %in% returns if right is in left
#so here, if either dimension =0, stop
      if (stop) {
        break
      }
    }
  }
  unlist(nums)
}

testdf1 = data.frame(matrix(1:9, nrow=3))
testdf2 = data.frame(matrix(1:6, nrow=2))

testdf1
spiral(testdf1)
spiral(testdf1, clockwise=TRUE)

testdf2
spiral(testdf2)
spiral(testdf2, clockwise=TRUE)

#####END OF R.Data Frames ###########
 