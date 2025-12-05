setwd("G:/Il mio Drive/R_advent/")

# https://adventofcode.com/2025/day/3

library(readr)
input <- read_csv("Day3.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day3_Example.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))

# Do they all have the same length?

var(apply(test, 1, function(x) nchar(x)))
var(apply(input, 1, function(x) nchar(x)))

data <- input
n_bank <- nrow(data)
n_bat <- nchar(data[1,1])

data_df <- matrix(NA, ncol=n_bat, nrow=n_bank)

for(d in 1:n_bat){
  data_df[,d] <- as.numeric(substring(data$X1, d,d))
}

id <- 
  t(apply(data_df, 1, function(x) {
  id_max <- which.max(x)
  if(id_max == n_bat){
    # Cerca il max precedente
    id_max_2 <- which.max(x[-id_max])
    out_id <- c(id_max_2, id_max)
  } else {
    # Cerca il max successivo{
    id_max_2 <- which.max(x[(id_max+1):n_bat])
    out_id <- c(id_max, id_max_2+id_max)
  }
  return(out_id)
  }
  ))

sum <- 0
for(l in 1:n_bank){
  sum <- sum + as.numeric(paste(data_df[l, id[l,1]], data_df[l, id[l,2]], sep = ""))
};sum



# Part Two:
rm(list=setdiff(ls(), c("input", "test")))

data <- input
n_bank <- nrow(data)
n_bat <- nchar(data[1,1])

data_df <- matrix(NA, ncol=n_bat, nrow=n_bank)

for(d in 1:n_bat){
  data_df[,d] <- as.numeric(substring(data$X1, d,d))
}

# Voglio tenere 12 cifre, le quali sono le piu' grandi possibili.
# Tolgo quindi tutte le cifre basse, stando attento
# a mantenere almeno 12 cifre. Mi baso sulle frequenze 
# retrocumulate.

# Questo riduce la complessita' del problema.
options(scipen=999)
ID_list <- vector(mode="list", length=n_bank)

lunghezze <- numeric(nrow(data_df))
sum <- 0



for(i in 1:nrow(data_df)){
  bank_i_sub <- data_df[i,]
  
  how_many_sub <- length(bank_i_sub)
  ID <- numeric(12)
  
  lunghezze[i] <- how_many_sub
  
  da_scegliere <- 12
  for(t in 1:12){
    print(t)
    
    bank_i_to_inspect <- 
      bank_i_sub[1:(how_many_sub - (12-t))]
    
    ID[t] <-
      bank_i_to_inspect[which.max(bank_i_to_inspect)]
    
    da_scegliere <- da_scegliere - 1
    
    bank_i_sub <- 
      bank_i_sub[-c(1:(which(bank_i_sub == ID[t])[1]))]
    
    how_many_sub <- length(bank_i_sub)
    
    if(da_scegliere == how_many_sub){
      break()
    } 
  }
  
  if(t != 12){
    ID <- c(ID[1:t], bank_i_sub)
  }
  
  ID_list[[i]] <- ID
  sum <- sum + as.numeric(paste(ID,collapse=""))
}
sum

