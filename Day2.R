setwd("C:/Users/roberto.ascari/Desktop/R_advent")
#https://adventofcode.com/2025/day/2#part2

# Part One:
input <- read.table("Day2.txt", sep = ",")
test <- read.table("Day2_Example.txt", sep = ",")

input <- t(input)
test <- t(test)


data <- input
inizio_ID <- numeric(nrow(data))
fine_ID <- numeric(nrow(data))

for(l in 1:nrow(data)){
  dash_id <- as.numeric(gregexpr(pattern ="-",data[l]))
  
  inizio_ID[l] <- as.numeric(substring(data[l], first = 1, last = dash_id-1))
  fine_ID[l] <- as.numeric(substring(data[l], first = dash_id+1, last = nchar(data[l])))
}
data_df <- data.frame(code=data[,1], inizio_ID=inizio_ID, fine_ID=fine_ID)

count <- 0
for(l in 1:nrow(data_df)){
  inizio <- data_df$inizio_ID[l]
  fine <- data_df$fine_ID[l]
  
  for(i in inizio:fine){
    if(nchar(i) %% 2 == 0){ # se il numero ha numero dispari di digit, allora non
                            # puo' contenere ID errati
      leng <- nchar(i)
      half1 <- substring(i, first = 1, last = leng/2)
      half2 <- substring(i, first = leng/2 + 1, last = leng)
      if(half1 == half2) count <- count + i
    }
  }
}
count


# Part Two:
rm(list=ls())

setwd("C:/Users/roberto.ascari/Desktop/R_advent")
input <- read.table("Day2.txt", sep = ",")
test <- read.table("Day2_Example.txt", sep = ",")

input <- t(input)
test <- t(test)


data <- test
inizio_ID <- numeric(nrow(data))
fine_ID <- numeric(nrow(data))


for(l in 1:nrow(data)){
  dash_id <- as.numeric(gregexpr(pattern ="-",data[l]))
  
  inizio_ID[l] <- as.numeric(substring(data[l], first = 1, last = dash_id-1))
  fine_ID[l] <- as.numeric(substring(data[l], first = dash_id+1, last = nchar(data[l])))
}
data_df <- data.frame(code=data[,1], inizio_ID=inizio_ID, fine_ID=fine_ID)


divisors <- function(x) {
  y <- seq_len(x)[-1]
  y[ x%%y == 0 ]
}



count_2 <- 0
for(l in 1:nrow(data_df)){
  inizio <- data_df$inizio_ID[l]
  fine <- data_df$fine_ID[l]
  
  for(i in inizio:fine){
    almeno_una_volta <- 0
    
    for(div in divisors(nchar(i))){
      
      cut <- c(seq(1, nchar(i), by=div), nchar(i)+1)
      cand <- numeric(length(cut)-1)
      for(p in 1:(length(cut)-1)){
        cand[p] <- as.numeric(substring(i, first = cut[p], last = cut[p+1]-1))
      }
      if(var(cand) == 0) almeno_una_volta <- almeno_una_volta + 1
      
      # gestire diversamente i casi in cui abbiamo un numero primo.
    }
    if(almeno_una_volta > 0) count_2 <- count_2 + i
  }
}
count_2

