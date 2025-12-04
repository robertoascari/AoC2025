setwd("G:/Il mio Drive/R_advent/")

# https://adventofcode.com/2025/day/4

# Part One:

library(readr)
input <- read_csv("Day4.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day4_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))

data <- input
N <- nchar(data[1,1])
data_df <- matrix(NA, ncol=N, nrow=nrow(data))

for(d in 1:N){
  data_df[,d] <- (substring(data$X1, d,d))
}

get_adj_border <- function(i, j, N = N){
  if(i == 1){
    grid <- expand.grid(i=c(1,2), j=c(j-1,j,j+1))[-3,]
  } else if(i == N){
    grid <- expand.grid(i=c(N-1,N), j=c(j-1,j,j+1))[-4,]
  } else if(j == 1){
    grid <- expand.grid(i=c(i-1,i,i+1), j=c(1,2))[-2,]
  } else if(j == N){
    grid <- expand.grid(i=c(i-1,i,i+1), j=c(N-1,N))[-5,]
  } else stop("ERRORE IN get_adj_border()")
  return(grid)
}


get_adj_inner <- function(i, j, N = N){
  grid <- expand.grid(i=c(i-1,i,i+1), j=c(j-1,j,j+1))[-5,]
  return(grid)
}


threshold <- 4
count <- 0

to_check_index <- which(data_df == "@", arr.ind = T)

for(l in 1:nrow(to_check_index)){
  i <- to_check_index[l,1]
  j <- to_check_index[l,2]
  
  if(i %in% c(1,N) & j %in% c(1,N)){ # Always take papers at the corners
    count <- count + 1
  } else if(i %in% c(1,N) | j %in% c(1,N)){ # If I am at the border:
    index <- get_adj_border(i, j, N)
    
    if(sum(apply(index, 1, function(x){
      data_df[x[1],x[2]] == "@"
    })) < threshold) {
      count <- count + 1
    }
  } else {
    index <- get_adj_inner(i, j, N)
    
    if(sum(apply(index, 1, function(x){
      data_df[x[1],x[2]] == "@"
    })) < threshold) {
      count <- count + 1
    }
  }
}
count


# Part Two:

data <- input
N <- nchar(data[1,1])
data_df <- matrix(NA, ncol=N, nrow=nrow(data))

for(d in 1:N){
  data_df[,d] <- (substring(data$X1, d,d))
}

get_adj_border <- function(i, j, N = N){
  if(i == 1){
    grid <- expand.grid(i=c(1,2), j=c(j-1,j,j+1))[-3,]
  } else if(i == N){
    grid <- expand.grid(i=c(N-1,N), j=c(j-1,j,j+1))[-4,]
  } else if(j == 1){
    grid <- expand.grid(i=c(i-1,i,i+1), j=c(1,2))[-2,]
  } else if(j == N){
    grid <- expand.grid(i=c(i-1,i,i+1), j=c(N-1,N))[-5,]
  } else stop("ERRORE IN get_adj_border()")
  return(grid)
}

get_adj_inner <- function(i, j, N = N){
  grid <- expand.grid(i=c(i-1,i,i+1), j=c(j-1,j,j+1))[-5,]
  return(grid)
}


threshold <- 4

cond <- T
count_tot <- 0
while(cond){
  count <- 0
  to_check_index <- which(data_df == "@", arr.ind = T)
  i_to_remove <- c()
  j_to_remove <- c()
  to_remove <- 0
  
  for(l in 1:nrow(to_check_index)){
    i <- to_check_index[l,1]
    j <- to_check_index[l,2]
    
    if(i %in% c(1,N) & j %in% c(1,N)){ # Always take papers at the corners
      count <- count + 1
      to_remove <- to_remove + 1
      i_to_remove[to_remove] <- i
      j_to_remove[to_remove] <- j
      
    } else if(i %in% c(1,N) | j %in% c(1,N)){ # If I am at the border:
      index <- get_adj_border(i, j, N)
      
      if(sum(apply(index, 1, function(x){
        data_df[x[1],x[2]] == "@"
      })) < threshold) {
        count <- count + 1
        to_remove <- to_remove + 1
        i_to_remove[to_remove] <- i
        j_to_remove[to_remove] <- j
      }
    } else { # If I am inside the grid:
      index <- get_adj_inner(i, j, N)
      
      if(sum(apply(index, 1, function(x){
        data_df[x[1],x[2]] == "@"
      })) < threshold) {
        count <- count + 1
        to_remove <- to_remove + 1
        i_to_remove[to_remove] <- i
        j_to_remove[to_remove] <- j
      }
    }
  }
  
  if(count != 0){
    count_tot <- count_tot + count
    # remove the selected ones:
    for(d in 1:to_remove){
      data_df[i_to_remove[d], j_to_remove[d]] <- "."
    }
  } else cond <- F
}; count_tot

