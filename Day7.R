setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/7

# Part One:
library(readr)
input <- read_csv("Day7.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day7_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))

data <- input

wide <- nchar(data$X1[1])
data_df <- matrix(NA, ncol = wide, nrow = nrow(data))

count_split <- 0
for(d in 1:wide){
  data_df[,d] <- (substring(data$X1, d,d))
}

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
