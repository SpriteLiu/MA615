library(magrittr)
library(readr)
library(tidyr)
library(dplyr)

#0
myName <- "Hsueh-Pin Liu"

#1
print_order <- function(x)
{
  y <- c()  
  max <- max(x)
  min <- min(x)
  for (i in 1:3)
  {
    if (x[i] == max){y[1] = x[i]}
    else if (x[i] == min){y[3] = x[i]}
    else{y[2] = x[i]}
  }
  return(y)
}


#2
print_string <- function(x)
{
  for(i in 1:x)
  {
    y <- ifelse(i/3!=i%/%3&i/5!=i%/%5,i,ifelse(i/3==i%/%3&i/5!=i%/%5,"YES",ifelse(i/3!=i%/%3&i/5==i%/%5,"NO","UNKNOWN")))
    print(y)
   }
}

#3
calc_sum_of_factor <- function(x)
{
  j <- 1
  y <- vector()
  for(i in 1:x)
  {
    if(x/i==x%/%i)
    {
      y[j]=i
      j <- j+1
    }
  }
  print(sum(y^2))
}
calc_sum_of_factor(12)

#4
find_intersect <- function(x,y,z)
{
  a <- vector()
  b <- 1
  c <- vector()
  d <- 1
  for(i in 1:length(x))
  {
    for(j in 1:length(y))
    {
      if(y[j]==x[i])
      {
        a[b] <- x[i]
        b <- b+1
        break
      }
    }
  }
  for(i in 1:length(a))
  {
    for(j in 1:length(z))
    {
      if(z[j]==a[i])
      {
        c[d] <- a[i]
        d <- d+1
        break
      }
    }
  }
  print(c)
}

#5
factorial_base <- function(x)
{
  y=1
  for(i in 1:x)
  {
    y=y*i
  }
  print(y)
}

#6
T <- function(n)
{
  print(n*(n+1)/2)
}
perfect_sqr <- function(x)
{
  y <- ifelse(sqrt(x)==trunc(sqrt(x)),"TRUE","FALSE")
  print(y)
}
num_tri_sqr <- function(n)
{
  y <- vector()
  j <- 1
  for(i in 1:n)
  {
    if(sqrt(i*(i+1)/2)==trunc(sqrt(i*(i+1)/2)))
    {
      y[j] <- i*(i+1)/2
      j <- j+1
    }
  }
  print(y)
}
new_num_tri_sqr <- function(n)
{
  sum=0
  for(i in 1:n)
  {
    if(sqrt(i*(i+1)/2)==trunc(sqrt(i*(i+1)/2)))
    {
      sum <- sum+i*(i+1)/2
    }
  }
  print(sum)
}
q6_sum <- new_num_tri_sqr(1500000)

#2022 H-1B Employer Data Hub
#1
h1b_2022 <- read_csv("https://www.uscis.gov/sites/default/files/document/data/h1b_datahubexport-2022.csv")

#3
na_num <- sum(is.na(h1b_2022))
h1b_2022a <- h1b_2022%>%drop_na()
h1b_2022a <- h1b_2022a[!h1b_2022a$City=="-",]
h1b_2022a <- h1b_2022a[!h1b_2022a$State=="-",]

#4
df_num <- aggregate(cbind(h1b_2022a$`Initial Approval`+h1b_2022a$`Initial Denial`,h1b_2022a$`Continuing Approval`+h1b_2022a$`Continuing Denial`,h1b_2022a$`Initial Approval`,h1b_2022a$`Initial Denial`),by=list(State=h1b_2022a$State),sum)
names(df_num) <- c("State","Init App","Conti App","Approve","Denial")
df_num <- as_tibble(df_num)

#5
app_num <- sum(df_num$Approve)
den_num <- sum(df_num$Denial)

#6
city_num <- as.data.frame(table(h1b_2022a$City))
names(city_num) <- c("City","Count")
city_num$City <- as.character(city_num$City)

#7
visa_num <- as.data.frame(table(h1b_2022a$NAICS))
names(visa_num) <- c("NAICS","Number")
visa_num$Percentage <- round(100*(visa_num$Number)/sum(visa_num$Number),3)
visa_num$NAICS <- as.numeric(levels(visa_num$NAICS)[visa_num$NAICS])

#Extra
non_integer_factorial <- function(x)
{
return(gamma(x+1))
}

