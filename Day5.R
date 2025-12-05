setwd("G:/Il mio Drive/R_advent/")

# https://adventofcode.com/2025/day/5

# Part One:
library(readr)
input <- read_csv("Day5.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day5_Example.txt", col_names = FALSE, 
                 col_types = cols(X1 = col_character()))
  
data <- input

ranges <- subset(data, grepl("-", data$X1))
fresh <- subset(data, !grepl("-", data$X1))
fresh$X1 <- as.numeric(fresh$X1)

n_ingr <- nrow(fresh)

ranges_df <- matrix(NA, ncol = 2, nrow = nrow(ranges))
dash_id <- unlist(gregexpr("-", ranges$X1))
for(d in 1:nrow(ranges)){
  ranges_df[d,1] <- as.numeric((substring(ranges$X1[d], 1,dash_id[d]-1)))
  ranges_df[d,2] <- as.numeric((substring(ranges$X1[d], dash_id[d]+1, nchar(ranges$X1[d]))))
}


is_fresh <- logical(n_ingr)
for(ing in 1:n_ingr){
  id_ing <- fresh$X1[ing]
  
  belong <- sum(id_ing <= ranges_df[,2] & id_ing >= ranges_df[,1])
  is_fresh[ing] <- ifelse(belong > 0, 1, 0)
}
sum(is_fresh)

# Part Two:
library(tidyverse)
options(scipen=999)

rm(list=setdiff(ls(), c("input", "test")))

data <- input
ranges <- subset(data, grepl("-", data$X1))
len_range <- nrow(ranges)

ranges_df <- matrix(NA, ncol = 2, nrow = len_range)
dash_id <- unlist(gregexpr("-", ranges$X1))
for(d in 1:len_range){
  ranges_df[d,1] <- as.numeric((substring(ranges$X1[d], 1,dash_id[d]-1)))
  ranges_df[d,2] <- as.numeric((substring(ranges$X1[d], dash_id[d]+1, nchar(ranges$X1[d]))))
}

colnames(ranges_df) <- c("Min","Max")
ranges_df <- as.data.frame(ranges_df)

ranges_df <- 
  ranges_df %>% arrange(Min)


# If min(i) <= max(i-1), then collapse the two rows
# into (min(i-1), max(i))
ranges_df_2 <- ranges_df
cond <- T
d <- 2
while(cond){
  
  if(ranges_df_2$Min[d] <= ranges_df_2$Max[d-1] &
     ranges_df_2$Max[d] <= ranges_df_2$Max[d-1]){
    #ranges_df_2$Max[d-1] <- ranges_df_2$Max[d]
    ranges_df_2 <- ranges_df_2[-d,]
  } else if(ranges_df_2$Min[d] <= ranges_df_2$Max[d-1] &
             ranges_df_2$Max[d] > ranges_df_2$Max[d-1]){
    ranges_df_2$Max[d-1] <- ranges_df_2$Max[d]
    ranges_df_2 <- ranges_df_2[-d,]
  } else {
    d <- d + 1
  }
  cond <- !(d > nrow(ranges_df_2))
}

sum(ranges_df_2[,2] - ranges_df_2[,1]) + 
  nrow(ranges_df_2)
