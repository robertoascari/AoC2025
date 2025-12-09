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
pair <- pair %>%  
  arrange(Var1) %>% 
  filter(Var1 < Var2)

pair$Area <- NA

for(i in 1:nrow(pair)){
  print(i)
  tile1 <- as.numeric(data[pair$Var1[i],])
  tile2 <- as.numeric(data[pair$Var2[i],])
  pair$Area[i] <- 
    (abs(tile2[1] - tile1[1])+1)*(abs(tile2[2] - tile1[2])+1)
}

max(pair$Area)




# Part Two:
rm(list=setdiff(ls(), c("input", "test")))

data <- input
colnames(data) <- c("X", "Y")

N_red <- nrow(data)

plot(data$X, data$Y, pch = 20)
polygon(c(data$X,data$X[1]), 
        c(data$Y, data$Y[1]))

library(sp)
#point.in.polygon(point.x,
#                 point.y,
#                 pol.x, pol.y)

pair <- expand.grid(1:N_red, 1:N_red)
pair <- pair %>%  filter(Var1 != Var2)
pair <- pair %>%  
  arrange(Var1) %>% 
  filter(Var1 < Var2)

pair$Keep <- TRUE




for(i in 1:nrow(pair)){
  # check if the other corners are inside the polygon:
  print(i)
  tile1 <- as.numeric(data[pair$Var1[i],])
  tile2 <- as.numeric(data[pair$Var2[i],])
  
  if(tile1[1] == tile2[1] | tile1[2] == tile2[2]){
    pair$Keep[i] <- FALSE
  } else {
    
    tile3 <- c(tile1[1], tile2[2])
    tile4 <- c(tile2[1], tile1[2])
    
    poly3 <- point.in.polygon(point.x = tile3[1],
                              point.y = tile3[2],
                              pol.x = c(data$X,data$X[1]),
                              pol.y = c(data$Y, data$Y[1]))
    poly4 <- point.in.polygon(point.x = tile4[1],
                              point.y = tile4[2],
                              pol.x = c(data$X,data$X[1]),
                              pol.y = c(data$Y, data$Y[1]))
    
    overall <- point.in.polygon(point.x = c(data$X,data$X[1]),
                                point.y = c(data$Y, data$Y[1]),
                                pol.x = c(tile1[1], tile3[1], tile2[1], tile4[1]),
                                pol.y = c(tile1[2], tile3[2], tile2[2], tile4[2]))
    
    if(poly3 == 0 | poly4 == 0 | any(overall == 1)){
      pair$Keep[i] <- FALSE
    }
  }
}

pair2 <- pair[pair$Keep,]


pair2$Area <- NA

for(i in 1:nrow(pair2)){
  tile1 <- as.numeric(data[pair2$Var1[i],])
  tile2 <- as.numeric(data[pair2$Var2[i],])
  pair2$Area[i] <- 
    (abs(tile2[1] - tile1[1])+1)*(abs(tile2[2] - tile1[2])+1)
}

pair2 <- pair2 %>% arrange(desc(Area))

length_out <- 10000

for(i in 1:nrow(pair2)){
  print(i)
  tile1 <- as.numeric(data[pair2$Var1[i],])
  tile2 <- as.numeric(data[pair2$Var2[i],])
  
  # Creo sequenza per la bisettrice:
  x <- seq(min(tile1[1], tile2[1]), max(tile1[1], tile2[1]), 
           length.out = length_out)
  y <- seq(min(tile1[2], tile2[2]), max(tile1[2], tile2[2]), 
           length.out = length_out)
  
  pp <-  point.in.polygon(point.x = x[-c(1, length_out)],
                         point.y = y[-c(1, length_out)],
                         pol.x = c(data$X),
                         pol.y = c(data$Y))

  if(all(pp ==1 )){ 
    break()
  }
}
i
pair2[i,]

data[pair2[i,1]-1,]
data[pair2[i,2],]





data[c(216,249),]

data[pair2[i,2]-1,]
# 1684971312: too high
# 343555368


plot(data$X, data$Y, pch = 20)
polygon(c(data$X,data$X[1]), 
        c(data$Y, data$Y[1]))
points(data$X[pair2[i,1]],
       data$Y[pair2[i,1]], pch = 20, col = 2)
points(data$X[pair2[i,2]],
       data$Y[pair2[i,2]], pch = 20, col = 2)

i <- 410
polygon(c(data$X[pair2[i,1]], data$X[pair2[i,1]], data$X[pair2[i,2]], data$X[pair2[i,2]]), 
        c(data$Y[pair2[i,1]], data$Y[pair2[i,2]], data$Y[pair2[i,2]], data$Y[pair2[i,1]]),
        col = 2)
i <- 416
polygon(c(data$X[pair2[i,1]], data$X[pair2[i,1]], data$X[pair2[i,2]], data$X[pair2[i,2]]), 
        c(data$Y[pair2[i,1]], data$Y[pair2[i,2]], data$Y[pair2[i,2]], data$Y[pair2[i,1]]),
        col = "blue")

points(x,y, pch  =20, type = "l")
