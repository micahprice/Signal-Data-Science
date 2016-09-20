# R: Attributes, Factors, and Matrices
# 9/14 Micah and Adom


attributes(mtcars)

attributes(mtcars)[1] = NULL 
mtcars

attributes(mtcars)[2] = "list" 
mtcars


paste(rep(c("a"), 2), collapse = "")
(c("a", "b"))

double_name = function(L) {
  original_names = names(L)
  i = 1
  doubled_names = vector(mode="character", length=length(original_names))
  for (name in original_names) {
    doubled_names[i] = paste(rep(name, 2), collapse = "")
    i = i + 1
  }
  attr(L, "names") = doubled_names
  return(L)
}

double_name(mtcars)

attributes(mtcars)
x=factor(c(10,4,3,10))
y=factor(c(4,2))
x
y
(c(x,y))
c(x)

# c( a factor ) returns a vector ordering the numbers from low to high. DONT DO IT

f1=factor(letters)
f2 = rev(factor(letters))
f3 = factor(letters, levels = rev(letters))

f1
c(f1)
f2
c(f2)
f3
c(f3)

?factor

data= c("apple", "grapefruit", "NA", "apple", "apple", "-", "grapefruit", "durian")
x=factor(data, c("apple", "grapefruit", "durian"), exclude = NULL)
x
table(x)

# Exclude = NULL returns all NA values as a factor
more= c(NA,NA,"hi")
factor(more, exclude = NULL)


turn_factor = function(df) {
  n=floor(ncol(df)/2)
  for (i in 1:n) {
    df[[i]]=factor(df[[i]])
  }
  return (df)
}
test=turn_factor(mtcars)
str(test)


turn_factor2 = function(df) {
  n=length(df)
  for (i in 1:n) {
    if ( length(unique(df[[i]])) <= 5) {
      df[[i]]=factor(df[[i]])
    } 
  }
  return (df)
}
test=turn_factor2(mtcars)
str(test)

replace_NAs= function(df) {
  n = length(df)
  for (i in 1:n) {
    col = df[[i]]
    if (is.factor(col)) {
      count = sort(table(col), decreasing = TRUE)
      common_value = names(count)[1]
      bad_value_indices = which(is.na(col))
      col[bad_value_indices] = common_value
      df[[i]] = col
    } 
  }
  return(df) 
}



test_case = data.frame(a = c(NA, "a", "a"), b = c("a", "a", "b"))
replace_NAs(test_case)



replace_NAs_random= function(df) {
  n = length(df)
  for (i in 1:n) {
    col = df[[i]]
    if (is.factor(col)) {
      weighting = table(col) / nrow(df)
      bad_value_indices = which(is.na(col))
      col[bad_value_indices] = sample(levels(col), length(bad_value_indices), prob = weighting, replace = TRUE)
      df[[i]] = col
    } 
  }
  return(df) 
}

test_case = data.frame(a = c(NA, NA, NA, NA, "a", "a"), b = c(NA, NA, NA, NA, "a", "b"), c = as.factor(c(NA, NA, NA, 1, 2, 3)))
replace_NAs_random(test_case)



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

?data.frame

test=data.frame(num=c(2,3,5,6,7), gender= c("male", "female","male", "female", "dunno"), morenum= c(3,4,5,6,7))
str(test)
test
prep_for_lr(test)


setwd("/home/adom/Downloads")

load("time.dat")
df
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
# remove rows with incomplete/unwanted data
problem_rows = c(which(df[[1]] == "999996" | df[[1]] == "999998" | df[[2]] == "999996" | df[[2]] == "999998"))
problem_rows
df[problem_rows,]
df = df[-problem_rows, ]

library("ggplot2")

names(df)
df
head(df, 25)
str(df)


parse_times = function(time_string) {
  values = unlist(strsplit(time_string, ":"))
  hours = as.numeric(values[1])
  mins = as.numeric(substr(values[2], 1, 2))
  is_am = (substr(values[2], 3, 3)=="A")
  return(list(hours, mins, is_am))
}

difference_from_8 = function(time_string) {
  time_list = parse_times(time_string)
  time = time_list[[1]] + time_list[[2]] / 60
  difference = time - 8
  if (((time_list[[3]] & ((time >= 5) & (!time == 12)))) | (!time_list[[3]] & (time == 12))) {
    return(difference - 12)
  } else if (time_list[[3]] & (!time==12)) {
    return(difference + 12)
  } else return(difference)
}


table(df[2])


clean_data = function(df) {
  for (col_index in 1:length(df)) {
    for (row_index in 1:nrow(df)) {
      current_value = as.character(df[row_index, col_index])
      df[row_index, col_index] = round(difference_from_8(current_value), 2)
    }
  }
  return(df)
}

# Test midnight and noon:       difference_from_8("12:00P")


cleaned = clean_data(df)
table(df[1])
table(cleaned[1])
str(cleaned)

#Convert to numeric
for (col_index in 1:lenth(cleaned)) {
  cleaned[col_index]=as.numeric(cleaned[[col_index]])
}

#Manually
cleaned[2]=as.numeric(cleaned[[2]])


  
ggplot(data = cleaned) + geom_histogram(mapping = aes(x = H2GH42), fill = "blue", alpha = .7,  binwidth=1, show.legend = TRUE) + 
geom_histogram(mapping = aes(x = H2GH43), fill = "green", alpha = .9, binwidth=1,show.legend = TRUE)

?geom_histogram

(matrix(matrix(1:10, nrow=5), nrow=2))
matrix(1:9)

transpose = function (a_matrix) {
  new_matrix=matrix(a_matrix, byrow=TRUE, ncol=nrow(a_matrix))
  new_matrix
}

transpose2 = function (a_matrix) {
  attr(a_matrix,"dim") = rev(dim(a_matrix))
  a_matrix
}

x=matrix(1:10, nrow=2)
x
transpose(x)
transpose2(x)

#ENDED ON TOP OF PG. 5

