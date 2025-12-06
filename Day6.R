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



# Part Two:
rm(list=ls())

input <- read_csv("Day6.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()), trim_ws = FALSE)
test <- read_csv("Day6_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()), trim_ws = FALSE)

data <- input

operations <- data[nrow(data),]$X1
operations <- unlist(strsplit(operations, " "))
operations <- operations[-c(which(operations==""))]



N <- nchar(data[3,1])
data_df <- matrix(NA, ncol=N, nrow=nrow(data))

for(d in 1:N){
  data_df[,d] <- (substring(data$X1, d,d))
  
  if(all(data_df[,d] == " ")){
    print("separatore")
  } else {
    data_df[data_df[,d] == " ",d] <- 0
  }
}
data_df <- data_df[-nrow(data_df),]
data_df <- t(data_df)

values2 <- 
  as.numeric(apply(data_df, 1, function(x) paste(x, collapse = "")))

for(l in 1:length(values2)){
  values2[l] <- as.numeric(gsub('0', '', values2[l]))
}


list_oper <- vector(mode = "list", length(operations))


ll <- 1
id <- 1
for(i in 1:length(values2)){
  if(is.na(values2[i])){
    list_oper[[ll]] <- values2[id:(i-1)]
    id <- i + 1
    ll <- ll + 1
  }
  if(i == length(values2)){
    list_oper[[ll]] <- values2[id:(i)]
  }
}
list_oper

ll <-
  unlist(lapply(1:length(list_oper), function(l){
    if(operations[l] == "+") {
      out <- sum(list_oper[[l]])
    } else {
      out <- prod(list_oper[[l]])
    }
    return(out)
  }))

sum(ll)



