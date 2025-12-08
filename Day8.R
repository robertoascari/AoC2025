setwd("G:/Il mio Drive/R_advent/")
options(scipen=999)

# https://adventofcode.com/2025/day/8

# Part One:
library(readr)
input <- read_csv("Day8.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day8_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))

data <- input
colnames(data) <- c("X","Y","Z")
data$clust <- NA

n <- nrow(data)
dist <- as.matrix(dist(data))
dist[!upper.tri(dist)] <- NA

connections <- 1000
n_clust <- 1
for(i in 1:connections){
  pair <- which(dist == min(dist, na.rm= T), arr.ind = T)
  
  if(all(is.na(data$clust[pair]))){ # Entrambi senza circuito
    data$clust[pair] <- n_clust
    
    n_clust <- n_clust + 1
    
  } else if(sum(is.na(data$clust[pair])) == 1){ # Uno senza circuito
    # allora devo unire al circuito quello nuovo
    id_na <- which(is.na(data$clust[pair]))
    id_no_na <- which(!is.na(data$clust[pair]))
    data$clust[pair[id_na]] <- data$clust[pair[id_no_na]]
    
  } else if(all(!is.na(data$clust[pair])) & 
            data$clust[pair[1]] != data$clust[pair[2]]){
    # Unisci i circuiti (TUTTI gli elementi dei circuiti)
    id_change <- data$clust[pair[2]]
    data$clust[data$clust == id_change] <- data$clust[pair[1]]
  } else if(all(!is.na(data$clust[pair])) & 
             data$clust[pair[1]] == data$clust[pair[2]]){
    # Non fare niente se sono gia' nello stesso circuito
  }
  dist[pair[1], pair[2]] <- NA
}

table(data$clust)

prod(sort(table(data$clust), decreasing = T)[1:3])


# Part Two:
rm(list=setdiff(ls(), c("input","test")))


data <- input
colnames(data) <- c("X","Y","Z")
data$clust <- NA

n <- nrow(data)
dist <- as.matrix(dist(data))
dist[!upper.tri(dist)] <- NA

cond <- T
n_clust <- 1
i <- 1
while(cond){
  pair <- which(dist == min(dist, na.rm= T), arr.ind = T)
  
  if(all(is.na(data$clust[pair]))){ # Entrambi senza circuito
    data$clust[pair] <- n_clust
    
    n_clust <- n_clust + 1
    
  } else if(sum(is.na(data$clust[pair])) == 1){ # Uno senza circuito
    # allora devo unire al circuito quello nuovo
    id_na <- which(is.na(data$clust[pair]))
    id_no_na <- which(!is.na(data$clust[pair]))
    data$clust[pair[id_na]] <- data$clust[pair[id_no_na]]
    
  } else if(all(!is.na(data$clust[pair])) & 
            data$clust[pair[1]] != data$clust[pair[2]]){
    # Unisci i circuiti (TUTTI gli elementi dei circuiti)
    id_change <- data$clust[pair[2]]
    data$clust[data$clust == id_change] <- data$clust[pair[1]]
  } else if(all(!is.na(data$clust[pair])) & 
            data$clust[pair[1]] == data$clust[pair[2]]){
    # Non fare niente se sono gia' nello stesso circuito
  }
  dist[pair[1], pair[2]] <- NA
  i <- i + 1
  
  if(all(!is.na(data$clust)) & var(data$clust[!is.na(data$clust)]) == 0){
    print(paste("i: ", i, ", first: ", pair[1], ", and second: ", pair[2], sep=""))
    cond <- F
  }
}

as.numeric(data$X[pair[1]]) * as.numeric(data$X[pair[2]])
