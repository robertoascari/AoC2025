setwd("G:/Il mio Drive/R_advent/")

# https://adventofcode.com/2025/day/3

library(readr)
input <- read_csv("Day3.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))
test <- read_csv("Day3_Example.txt", col_names = FALSE, 
                  col_types = cols(X1 = col_character()))

# Do they all have the same lenght?

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

