# Project Euler Problems
# 9/26 Micah
# Problems 3, 5, 7, 15,

# 9/29
# 18, 24, 31, 67, 1, 2, 4, 40

#3
#The prime factors of 13195 are 5, 7, 13 and 29.
#What is the largest prime factor of the number 600851475143 ?
factorit = function(num) {
  if(num < 2) return ("AUUGGHH WHAT HAVE YOU DONE")
  dummy = num
  n=2
  factors=1
  while (TRUE) {
    if (num %% n != 0){
      n=n+1
      next
    } else if (n==num){
        factors = c(factors, n)
        return (factors)
    } else {
      factors = c(factors, n)
      num=num/n
      n=2
    }
  }
}

lgst_prime_factor = function(n) {
  f=factorit(n)
  f[length(f)]
}
lgst_prime_factor(600851475143)
#6857


#5
#2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
#What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
#cleverly only test some values (skip odd, etc) (1:20), or cleverly only test on some numbers
# only need to test 11:20
#start with even number
n=2520
n=232792500
x=11:20
while(sum(n %% x) != 0) {
    n = n + 2
    print(n)
}


#7
#By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
#What is the 10 001st prime number?

bign=10000000
twos = seq(4,bign,2)
threes=seq(6,bign,3)
fives=seq(10,bign,5)
sevens=seq(14,bign,7)
values=unique(c(twos, threes,fives, sevens))

marker=logical(bign)
marker[values]=TRUE
marker[c(1,2)] = TRUE
#True means is multiple of 2,3,5,7
marker=(which(!marker))
head(marker,20)

nextprime = function(prime) {
  i=1
  n=marker[i]
  prime = prime + 2
  while (TRUE) {
    if (n > sqrt(prime)) {
      return (prime)

    } else if (prime %% n == 0) {
      prime = prime + 2
      i=1
      n=marker[i]
      next
    }  else {
      i=i+1
      n=marker[i]
    }  
  }
}


prime = 3 #have to start with odd prime
i=2
library(tictoc)
tic()
primes= numeric(10001)
primes[1]=2
primes[2]=3
while (i!=10001) {
  i=i+1
  print(paste("number:",i))
  prime = nextprime(prime)
  primes[i]=prime
}
toc()
primes; #104743
# Instead do Sieve of Eratosthenes
# My method sort of half ass did it without realizing it.


#15
#Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
#How many such routes are there through a 20×20 grid?
#for nxn grid
   # (n+n)! ordered permutations of right arrows and down arrows
   # divided by (n!) because right arrows aren't ordered and (n!) because
   # down arrows aren't ordered
nbyn = function(n) factorial(2*n)/(factorial(n)^2)
nbyn(20) # 137846528820

#18 and #67

list.files('~/R')
triangle = read.csv('~/R/p067_triangle.csv', header=T, sep='')
#As is requires a header (copy paste largest row of numbers to the top), otherwise would only read 5 columns..
# 75
# 95 64
# 17 47 82
# 18 35 87 10
# 20 04 82 47 65
# 19 01 23 75 03 34
# 88 02 77 73 07 63 67
# 99 65 04 28 06 16 70 92
# 41 41 26 56 83 40 80 70 33
# 41 48 72 33 47 32 37 16 94 29
# 53 71 44 65 25 43 91 52 97 51 14
# 70 11 33 28 77 73 17 78 39 68 17 57
# 91 71 52 38 17 14 91 43 58 50 27 29 48
# 63 66 04 68 89 53 67 30 73 16 69 87 40 31
# 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
max_path = function (triangle) {
  previous = triangle[nrow(triangle),]
  for(j in (nrow(triangle)-1):1){
    current = triangle[j,]
    new = numeric(j)
    
    for (i in 1:j) {
      new[i] = max(current[i] + previous[i], current[i] + previous[i+1])
    }
    previous = new
    print (previous)
  }
  previous
}
max_path(triangle) #1074 and #7273

factorial(3)
#24
# with n! possible permutations, the number of permutations with a particular digit
# in front is (n-1)! . This can be used recursively to find out what the left-most digit is 
# at a given lexigraphic permutation

ans = numeric()
index = 1000000
digits = c(0,1,2,3,4,5,6,7,8,9) #must be ordered

while(length(digits) != 0) {
  n = length(digits)
  groups = factorial(n-1)
  newdig = ceiling(index/groups) 
  #1millionth permutation is in group with third digit being leftmost
  ans = c(ans,digits[newdig])
  digits = digits[-newdig]
  index = index - (newdig-1)*groups #subtract off permutations already cycled through
  print (ans)
  print(index)
}  

#31
#coins = 1,2,5,10,20,50,100
#get to 200
#uses generating functions trick inscrutably, not really a great way
#http://people.qc.cuny.edu/faculty/christopher.hanusa/courses/636fa12/Documents/636fa12ch35.pdf

#these are coefficients of geometric series's that represent each coin
c1 = rep(1,201)
c2 = rep(c(1,0),length.out = 201)
c5 = rep(c(1,rep(0,4)),length.out = 201)
c10 = rep(c(1,rep(0,9)),length.out = 201)
c20 = rep(c(1,rep(0,19)),length.out = 201)
c50 = rep(c(1,rep(0,49)),length.out = 201)
c100 = rep(c(1,rep(0,99)),length.out = 201)


mult = function(c1,c2) {
  ans = numeric(201)
  for (k in 1:201) {
    cumsum = 0
    for (i in 1:k) {
     cumsum = cumsum + c1[i]*c2[k-i+1]
    }
    ans[k] = cumsum 
  }
  ans
}
Reduce(mult,c(c1,c2,c5,c10,c20,c50,c100))

c12 = mult(c1,c2)
c15 = mult(c12,c5)
c110 = mult(c15,c10)
c120 = mult(c110,c20)
c150 = mult(c120,c50)
c1100 = mult(c150,c100)
c1100[201] #73681
#add 1 because 1 200 coin also adds up to 200


#SOME MORE
#1 
sum((1:1000)[1:1000%%5 == 0 | 1:1000%%3 == 0]) - 1000
# 233168

#2
fibs = function(n) {
  i = 0 
  first = 0
  second = 1
  while(i!=n){
    dummy = first
    first = second
    second = dummy +second
    i=i+1
  }
  second
}
fib = sapply(1:40,fibs)
sum(fib[fib <= 4000000 & fib%%2 == 0]) 
# 4613732

#4
#want to do it w/out converting to string. (This is a dumb way)
999*999 #ans will be six digits
a = sort(unique(c(outer(500:999,500:999,"*"))), decreasing = TRUE)
#should have guessed the answer would be in 900:999 or something

find_pali_numerically = function(x) {
  hundred = 100*(x%%10)
  ten = 10*floor(x%%100/10)
  one  = floor(x%%1000/100)
  floor(x/1000)==hundred+ten+one
}
Position (find_pali_numerically , a) #2216
a[2216] #906609
#913*993

#40
#brute force, takes awhile and makes R unhappy
# nums = as.list(1:200000) 
# morenums = do.call(paste0, nums)
# nchar(morenums) #big enough for 1mil digits
# paste((c(3,4,45)))
# digits = sapply(c(1,10,100,1e3,1e4,1e5,1e6),function(x) substr(morenums,x,x))
# Reduce("*", as.numeric(digits)) #210
#better to a append to a string of n in while loop as n=n+1 


