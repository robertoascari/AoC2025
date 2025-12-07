setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/7

# Part One:
library(readr)
input <- read_csv("Day7.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day7_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))

data <- test

wide <- nchar(data$X1[1])
data_df <- matrix(NA, ncol = wide, nrow = nrow(data))

for(d in 1:wide){
  data_df[,d] <- (substring(data$X1, d,d))
}

count_split <- 0
for(r in 2:nrow(data)){
  which_old <- which(data_df[r-1,] == "|" | data_df[r-1,] == "S")
  
  for(l in which_old){
    if(data_df[r,l] == "."){
      data_df[r,l] <- "|"
    } else if(data_df[r,l] == "^") { 
      data_df[r,c(l-1, l+1)] <- "|"
      count_split <- count_split + 1
    }
  }
}
count_split


# Part Two:
library(tidyverse)
rm(list=setdiff(ls(), c("input", "test")))

data <- input 

wide <- nchar(data$X1[1])
data_df <- matrix(NA, ncol = wide, nrow = nrow(data))

for(d in 1:wide){
  data_df[,d] <- (substring(data$X1, d,d))
}

many_split <- length(which(data_df == "^"))
where_split <- 
  as.data.frame(which(data_df == "^", arr.ind = T)) %>% 
  arrange(row)

data_df_2 <- data_df

pesi <- matrix(0, ncol = ncol(data_df_2), nrow = nrow(data_df_2))
pesi[1,which(data_df_2[1,] == "S")] <- 1

for(r in 2:nrow(data_df_2)){
  pesi[r,] <- pesi[r-1,]
  # Se ci sono degli splitter:
  if(length(which(data_df[r,] == "^")) > 0){
    where_split_r <- which(data_df[r,] == "^")
    for(l in where_split_r){
      if(pesi[r-1,l] > 0){ # ho un raggio sopra allo splitter
        pesi[r,l] <- 0
        pesi[r,l-1] <- pesi[r,l-1] + pesi[r-1,l] 
        pesi[r,l+1] <- pesi[r,l+1] + pesi[r-1,l] 
      }
    }
    
    pesi[r, pesi[r,] == 0 & data_df_2[r,] != "^"] <- 
      pesi[r-1, pesi[r,] == 0 & data_df_2[r,] != "^"]
    
    # pesi[r,which(data_df[r,] != "^")] <- 
    #   pesi[r-1,which(data_df[r,] != "^")]
  } else {
    # abbassare i pesi
    pesi[r,] <- pesi[r-1,]
  }
}
sum(pesi[nrow(data_df_2),])

