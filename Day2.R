setwd("C:/Users/roberto.ascari/Desktop/R_advent")
setwd("C:/Users/ascari/Desktop/R_advent")
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
    if(nchar(i) %% 2 == 0){ # se il numero ha numero dispari di digit, allora 
                            # non puo' contenere ID errati
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


data <- input
inizio_ID <- numeric(nrow(data))
fine_ID <- numeric(nrow(data))


for(l in 1:nrow(data)){
  dash_id <- as.numeric(gregexpr(pattern ="-",data[l]))
  
  inizio_ID[l] <- as.numeric(substring(data[l], first = 1, last = dash_id-1))
  fine_ID[l] <- as.numeric(substring(data[l], first = dash_id+1, last = nchar(data[l])))
}
data_df <- data.frame(code=data[,1], inizio_ID=inizio_ID, fine_ID=fine_ID)


divisors <- function(x) {
  y <- seq_len(x)
  y[ x%%y == 0 ]
}

options(scipen=999)



count_2 <- 0
for(l in 1:nrow(data_df)){
  print(paste("l = ", l, " su ", nrow(data_df), sep=""))
  inizio <- data_df$inizio_ID[l]
  fine <- data_df$fine_ID[l]
  
  # Valuto tutti gli ID:
  for(i in inizio:fine){
    almeno_una_volta <- 0
    divisori <- divisors(nchar(i))
    
    if(nchar(i) != 1){
      for(d in 1:(length(divisori))){
        div <- divisori[d]
        if(div != 1){
          cut <- c(seq(1, nchar(i), by=nchar(i)/div),nchar(i)+1);cut
          cand <- numeric(length(cut)-1)
          
          for(p in 1:(length(cut)-1)){
            cand[p] <- as.numeric(substring(i, first = cut[p], last = cut[p+1]-1))
          };cand
          
          if(var(cand) == 0) almeno_una_volta <- almeno_una_volta + 1
        }
      }
    }
    if(almeno_una_volta > 0) count_2 <- count_2 + i
  }
}
count_2



# caso in cui valuto l'uguaglianza di tutti i singoli digit:

# if(div != nchar(i)){
#   cut_ultimo <- seq(1, nchar(i), by=1)
#   cand <- numeric(length(cut_ultimo))
#   for(p in 1:(length(cut_ultimo))){
#     cand[p] <- as.numeric(substring(i, first = cut_ultimo[p], last = cut_ultimo[p]))
#   }
#   if(var(cand) == 0) almeno_una_volta <- almeno_una_volta + 1
# }
