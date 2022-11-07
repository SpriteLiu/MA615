myName <- "Hsueh-Pin Liu"

#1(a)
v1 <- c(1:20)

#1(b)
v2 <- c(20:1)

#1(c)
v3 <- seq(1,19,2)

#1(d)
v4 <- rep(c(3,7,11),10)

#1(e)
v5 <- rep(c(3,7,11),11,31)

#2
x <- seq(3,6,0.1)
x1 <- exp(x)*sin(x)

#3
i <- c(10:100)
sum1 <- sum(i^3+4*i^2)

#4(a)
str1 <- paste("label",1:30, sep=" ")

#4(b)
str2 <- paste("function",1:30,sep="")

#5
vs <- paste(c(1,'function',NA,seq(1,5,2),0.125),collapse=',')

#6
A <- matrix(1:9,3,3)
m1_ans <- A%*%A%*%A

#7
B <- matrix(rep(c(12,-12,12),each=17),17,3)
m2_ans <- t(B)%*%B

#8
m <- c(0:-4)
A <- matrix(abs((rep(abs(m),times=5)+rep(m,each=5)))+1,5,5)
y <- c(7,-1,-3,5,17)
m3_ans <- solve(A)%*%y

#9(a)
xv=seq(0,1,0.1)
function1 <- function(xv)
{
  xv^(1:length(xv))
}
func1_ans <- function1(xv)

#9(b)
xv=seq(0,1,0.1)
function2 <- function(xv)
{
  xv^(1:length(xv))/(1:length(xv))
}
func2_ans <- function2(xv)

#9(c)
xv=seq(0,1,0.1)
function3 <- function(x,n)
{
  sum((x^(1:n)/(1:n)))+1
}
func3_ans <- function3(xv,length(xv))
func3_ans

#10
cel_to_far <- function(x)
{
  x*9/5+32
}
far_to_cel <- function(x)
{
  (x-32)*5/9
}

#11
func_odd <- function(x)
{
  seq(1,x,2)
}
odd_ans <- func_odd(2000)

#12
func_r <- function(r)
{
  sum((1:r)^0.5/(11+3.5*r^1.2))
}
r <- c(1:10)
sum_ans <- sum(sapply(r,function(r)(r^0.5/(11+3.5*10^1.2))))

#13
modNumber <- function(x,y)
{
  if (x%%y==0)
  {
    return(x)
  }
  else
  {
    return((x%/%y+1)*y)
  }
}

#14
numberOfWheels <- function(vehicle)
{
  switch(vehicle,"unicycle"=1,"bike"=2,"car"=4,"truck"=4,"tricycle"=3,"motorcycle"=2)
}

#15
myFactorial <- function(x)
{
  y <- 1
  for(i in 1:x)
  {
    y=y*i
  }
  print (y)
}

#16
myCustomFactorial <- function(x,y)
{
  z <- 1
  for(i in x:y)
  {
    z=z*i
  }
  print(z)
}

#17
library(datasets)
customRiverMean <- function(x)
{
  n <- 0
  sum=0
  for(i in 1:length(rivers))
  {
    if(rivers[i]<x)
  {
    sum=sum+rivers[i]
    n=n+1
    }
  }
  print(sum/n)
}

#18
library(datasets)
func_teeth <- function(y)
{
  x <- ToothGrowth$len
  x <- x[-which(x<y)]
  print(x)
}
longTeeth <- func_teeth(15)

#19
library(datasets)
apply(mtcars,2,mean)
averageHorsePower <- 146.687500
averageWeight <- 3.217250

#20
z <- function(xVec,yVec)
{
  rowSums(sapply(yVec,FUN=function(y){y<xVec}))
}







