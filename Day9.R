setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/9

# Part One:
library(readr)
library(tidyverse)
input <- read_csv("Day9.txt", col_names = FALSE)
test <- read_csv("Day9_Example.txt", col_names = FALSE)

data <- input
colnames(data) <- c("X", "Y")
data <- data %>% arrange(X)

N_red <- nrow(data)

pair <- expand.grid(1:N_red, 1:N_red)
pair <- pair %>%  filter(Var1 != Var2)

pair$Area <- NA

for(i in 1:nrow(pair)){
  tile1 <- as.numeric(data[pair$Var1[i],])
  tile2 <- as.numeric(data[pair$Var2[i],])
  pair$Area[i] <- 
    (abs(tile2[1] - tile1[1])+1)*(abs(tile2[2] - tile1[2])+1)
}

max(pair$Area)


# Part Two:
rm(list=setdiff(ls(), c("input", "test")))

data <- input 
