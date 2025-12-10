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
  lights[[i]] <- gsub("\\[|\\]", "",substring(data[i,]$X1, 1, 
                           as.numeric(gregexpr("]", data[i,]$X1)[[1]])))
  
  lights[[i]] <- gsub("\\.", "0", lights[[i]])
  lights[[i]] <- gsub("#", "1", lights[[i]])
  lights[[i]] <- lapply(strsplit(lights[[i]], ""), function(x) as.numeric(x))[[1]]
  
  tonde <- as.numeric(gregexpr(")", data[i,]$X1)[[1]])
  qnt_buttons[i] <- length(tonde)
  
  butt <-  substring(data[i,]$X1, 
                            2 + as.numeric(gregexpr("]", data[i,]$X1)[[1]]),
                            tonde[length(tonde)])
  
  butt <- gsub(" \\(", "",butt)
  butt <- strsplit(gsub("\\(", "",butt), "\\)")[[1]]

  buttons[[i]] <- lapply(strsplit(butt, ","), function(x) as.numeric(x) + 1)
    
  jj  <-  substring(data[i,]$X1, 
                        2 + tonde[length(tonde)],
                    nchar(data[i,]$X1))
  
  jj <- gsub("\\{|\\}", "",jj)
  jolt[[i]] <- as.numeric(strsplit(jj, ",")[[1]])
}


switch_light <- function(lights, button){
  # button is a list
  for(l in 1:length(button)){
    butt <- button[[l]]
    for(i in 1:length(butt)){
      lights[butt[i]] <- ifelse(lights[butt[i]] == 0, 1, 0)
    }
  }
  
  return(lights)
}

min_k <- numeric(length(lights))
for(l in 1:length(lights)){
  light_off <- numeric(length(lights[[l]]))
  butt <- buttons[[l]]
  k <- 0
  cond <- T
  print(paste("l = ", l, sep=""))
  while(cond){
    k <- k + 1
    print(k)
    comb <- combn(1:length(butt), k)
    
    comb_lights <- lapply(1:ncol(comb), 
           function(x) switch_light(light_off, butt[comb[,x]]))
    
    comb_lights <- any(unlist(lapply(comb_lights, function(x) 
      all(x == lights[[l]]))))
    
    cond <- !comb_lights
  }
  min_k[l] <- k
}
sum(min_k)


# Part Two:
rm(list=setdiff(ls(), c("input","test", "jolt", "buttons")))
library(gtools)


data <- test


lights <- vector(mode = "list", length = nrow(data))
buttons <- vector(mode = "list", length = nrow(data))
qnt_buttons <- numeric(nrow(data))
jolt <- vector(mode = "list", length = nrow(data))

for(i in 1:nrow(data)){
  lights[[i]] <- gsub("\\[|\\]", "",substring(data[i,]$X1, 1, 
                                              as.numeric(gregexpr("]", data[i,]$X1)[[1]])))
  
  lights[[i]] <- gsub("\\.", "0", lights[[i]])
  lights[[i]] <- gsub("#", "1", lights[[i]])
  lights[[i]] <- lapply(strsplit(lights[[i]], ""), function(x) as.numeric(x))[[1]]
  
  tonde <- as.numeric(gregexpr(")", data[i,]$X1)[[1]])
  qnt_buttons[i] <- length(tonde)
  
  butt <-  substring(data[i,]$X1, 
                     2 + as.numeric(gregexpr("]", data[i,]$X1)[[1]]),
                     tonde[length(tonde)])
  
  butt <- gsub(" \\(", "",butt)
  butt <- strsplit(gsub("\\(", "",butt), "\\)")[[1]]
  
  buttons[[i]] <- lapply(strsplit(butt, ","), function(x) as.numeric(x) + 1)
  
  jj  <-  substring(data[i,]$X1, 
                    2 + tonde[length(tonde)],
                    nchar(data[i,]$X1))
  
  jj <- gsub("\\{|\\}", "",jj)
  jolt[[i]] <- as.numeric(strsplit(jj, ",")[[1]])
}

press_jolt <- function(jolt, button){
  # button is a list
  for(l in 1:length(button)){
    butt <- button[[l]]
    
    for(i in 1:length(butt)){
      jolt[butt[i]] <- jolt[butt[i]] + 1
    }
  }
  
  return(jolt)
}


combinations(n = length(v), r = 2, v = v, repeats.allowed = TRUE)

min_k <- numeric(length(jolt))
for(l in 1:length(jolt)){
  jolt_off <- numeric(length(jolt[[l]]))
  butt <- buttons[[l]]
  k <- 0
  cond <- T
  print(paste("l = ", l, sep=""))

  while(cond){
    k <- k + 1
    print(k)
    comb <- combinations(n = length(butt), 
                         r = k, 
                         v = 1:length(butt), 
                         repeats.allowed = TRUE)

    
    comb_jolt <- lapply(1:nrow(comb), 
                        function(x) press_jolt(jolt_off, butt[comb[x,]]))
    
    comb_jolt_2 <- any(unlist(lapply(comb_jolt, function(x) 
      all(x == jolt[[l]]))))
    
    comb <- comb[!unlist(lapply(comb_jolt, function(x) any(x > jolt[[l]]))),]
    
    cond <- !comb_jolt_2;cond
  }
  min_k[l] <- k
}

sum(min_k)










min_k <- numeric(length(jolt))
for(l in 1:length(jolt)){
  jolt_off <- numeric(length(jolt[[l]]))
  butt <- buttons[[l]]
  k <- 1
  cond <- T
  print(paste("l = ", l, sep=""))
  comb <- matrix(1:length(butt), nrow=length(butt), ncol=1)
  
  while(cond){
    k <- k + 1
    print(k)
    comb_id <- expand.grid(1:nrow(comb), 1:length(butt))
    comb <- cbind(comb[comb_id$Var1,], comb_id$Var2)
    
    comb_jolt <- lapply(1:nrow(comb), 
                        function(x) press_jolt(jolt_off, butt[comb[x,]]))
    
    comb_jolt_2 <- any(unlist(lapply(comb_jolt, function(x) 
      all(x == jolt[[l]]))))
    
    comb <- comb[!unlist(lapply(comb_jolt, function(x) any(x > jolt[[l]]))),]
    
    cond <- !comb_jolt_2;cond
  }
  min_k[l] <- k
}

sum(min_k)
