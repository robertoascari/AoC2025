setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/6

# Part One:
library(readr)
input <- read_csv("Day6.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day6_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))

data <- input

operations <- data[nrow(data),]$X1
operations <- unlist(strsplit(operations, " "))
operations <- operations[-c(which(operations==""))]

values <- matrix(NA, nrow = length(operations), ncol = nrow(data) - 1)

for(l in 1:nrow(values)){
  temp <- as.numeric(unlist(strsplit((data[-nrow(data),]$X1)[l], " ")))
  values[,l] <- temp[-which(is.na(temp))]
}

ll <-
unlist(lapply(1:nrow(values), function(i){
  if(operations[i] == "+") {
    out <- sum(values[i,])
  } else {
    out <- prod(values[i,])
  }
  return(out)
}))

sum(ll)
