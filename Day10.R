setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/10

# Part One:
library(readr)
library(tidyverse)
input <- read_delim("Day10.txt", delim = ";", col_names = FALSE,
                    escape_double = FALSE, trim_ws = TRUE)
test <- read_delim("Day10_Example.txt", delim = ";", col_names = FALSE,
                   escape_double = FALSE, trim_ws = TRUE)

data <- test


lights <- vector(mode = "list", length = nrow(data))
buttons <- vector(mode = "list", length = nrow(data))
qnt_buttons <- numeric(nrow(data))
jolt <- vector(mode = "list", length = nrow(data))

for(i in 1:nrow(data)){
  lights[[i]] <- substring(data[i,]$X1, 1, 
                           as.numeric(gregexpr("]", data[i,]$X1)[[1]]))
  tonde <- as.numeric(gregexpr(")", data[i,]$X1)[[1]])
  qnt_buttons[i] <- length(tonde)
  buttons[[i]] <- substring(data[i,]$X1, 
                            2 + as.numeric(gregexpr("]", data[i,]$X1)[[1]]),
                            tonde[length(tonde)])
  
  jolt[[i]] <- substring(data[i,]$X1, 
                         2 + tonde[length(tonde)],
                         nchar(data[i,]$X1))
}








