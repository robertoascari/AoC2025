setwd("C:/Users/roberto.ascari/Desktop/R_advent")

code <- read.table("Day1.txt")

n_rotations <- nrow(code)
direction <- apply(code, 1, function(x) 
  substr(x, 1, 1))

rotations <- as.numeric(apply(code, 1, function(x) 
  sub('^.', '', x)))

# Check:
# cbind(direction, rotations, code)

position <- numeric(n_rotations+1)
position[1] <- 50
count_0 <- 0
rot <- ifelse(direction == "L", -1, 1)*rotations

for(i in 2:(n_rotations+1)){
  position[i] <- position[i-1] + rot[i-1]
  
  while(!(position[i] <= 99 & position[i] >=0)){
    if(position[i] < 0) {
      position[i] <- position[i] + 100
    } else if(position[i] >= 100){
      position[i] <- position[i] -100
    }
  }
  
  if(position[i] == 0) count_0 <- count_0 + 1
}



# Part Two:
rm(list=ls())


code <- read.table("Day1.txt")

n_rotations <- nrow(code)
direction <- apply(code, 1, function(x) 
  substr(x, 1, 1))

rotations <- as.numeric(apply(code, 1, function(x) 
  sub('^.', '', x)))

rot <- ifelse(direction == "L", -1, 1)*rotations
increases <- numeric(n_rotations)

count_0_cum <- 0
# riporto tutto tra -99 e 99
for(l in 1:n_rotations){
  if(rot[l] >= 100) {
    count_0_cum <- count_0_cum + floor(rot[l]/100)
    increases[l] <- floor(rot[l]/100)
    rot[l] <- rot[l] - floor(rot[l]/100)*100
    
  } else if(rot[l] <= -100){
    count_0_cum <- count_0_cum + floor(abs(rot[l])/100)
    increases[l] <- floor(abs(rot[l])/100)
    rot[l] <- rot[l] + floor(abs(rot[l])/100)*100
  }
}
count_0_cum
cbind(ifelse(direction == "L", -1, 1)*rotations, rot, increases)


start <- 50
count_0_cum_2 <- 0

for(i in 1:length(rot)){
  start <- start + rot[i]
  
  if(start > 100){
    count_0_cum_2 <- count_0_cum_2 + 1
    start <- start - 100
  } else if(start < 0){
    count_0_cum_2 <- count_0_cum_2 + 1
    start <- start + 100
    
  } else if(start == 0 & (rot[i+1] > 0)) {
    count_0_cum_2 <- count_0_cum_2 + 1
    
  } else if(start == 100) {
    start <- start - 100
    if(rot[i+1] > 0){
      count_0_cum_2 <- count_0_cum_2 + 1
    }
  } else {
    print(start)
  }
}
if(start == 0) count_0_cum_2 <- count_0_cum_2 + 1

count_0_cum_2 + count_0_cum
