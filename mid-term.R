myName <- "Hsueh-Pin Liu"

library(tidyverse)
library(magrittr)
library(readxl)

strawb <- read_xlsx("Desktop/MA 615 Homework/untitled folder/strawberries-2022oct30-a.xlsx")

cnames <- colnames(strawb)
x <- 1:dim(strawb)[2]

unique(strawb[1])
unique(strawb[2])
unique(strawb[3])

T <- NULL
for(i in x){T <- c(T, dim(unique(strawb[i]))[1])}
drop_cols <- cnames[which(T == 1)]
strawb %<>% dplyr::select(!all_of(drop_cols))
strawb %<>% arrange(Year, State)

colnames(strawb)
temp1 <- strawb %>% dplyr::select(`Data Item`) %>% 
         distinct()
strawb2 <- strawb %>% separate(col=`Data Item`,
                into = c("Strawberries", "items", "units"),
                sep = ",",
                fill = "right")
strawb3 <- strawb %>% separate(col=`Data Item`,
            into = c("Strawberries", "type", "items", "units"),
                               sep = ",",
                               fill = "right")
rm(strawb2, strawb3)
strawb %<>% separate(col=`Data Item`,
                    into = c("Strawberries", "type", "items", "units"),
                    sep = ",",
                    fill = "right")

#1
#285CWT=28500LB

#2
s1 <- filter(strawb, State == 'CALIFORNIA' & 
               Year == 2016 & 
               Domain == 'ORGANIC STATUS')
c1 <- as.numeric(s1$Value)
#(231304956-13.7,231304956+13.7)

#3
s2 <- filter(strawb, State == 'CALIFORNIA' & 
               Year == 2016 & 
               Domain != 'ORGANIC STATUS')
s2 <- filter(s2, Value != "(NA)" & 
         Value != "(D)" & 
         Domain != "TOTAL")
#NA

#4
unique(strawb[10])
s3 <- filter(strawb, Domain != 'ORGANIC STATUS' & 
                     Domain != 'TOTAL')
a <- grep("TOTAL",
     s3$`Domain Category`,
     ignore.case = T)
a <- unique(s3[11])
#175-36=139

#5
s4 <- filter(strawb, State == 'FLORIDA' & 
               Domain != 'ORGANIC STATUS' & 
               Domain != 'TOTAL')
s5 <- filter(strawb, State == 'CALIFORNIA' & 
         Domain != 'ORGANIC STATUS' & 
         Domain != 'TOTAL')
a <- grep("TOTAL",
     s4$`Domain Category`,
     ignore.case = T)
unique(s4[11])
#(142-16)-(119-16)=23








