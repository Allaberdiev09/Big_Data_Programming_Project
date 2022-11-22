install.packages("tidyverse")
install.packages("data.table")
install.packages("microbenchmark")

#sequential processing starts here
library(data.table)
library(tidyverse)
library(microbenchmark)
library(ggplot2)

seqf <- function(i){
  list.files(path = "C:/Users/User/Desktop/BIG DATA/Area_level_grocery_purchases(ALL)/", pattern = "*.csv") %>%
    map_df(~fread(.))
}
seqp <- microbenchmark("Sequential Processing All Area Level Purchases" = {lapply(1:100, seqf)})
seqp
autoplot(seqp)
#sequential processing ends here

#parallel processing starts here
pllf <- function(i){
  library(data.table)
  library(tidyverse)
  list.files(path = "C:/Users/User/Desktop/BIG DATA/Area_level_grocery_purchases(ALL)/", pattern = "*.csv") %>%
    map_df(~fread(.))
}

pllfcluster <- function(i){
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, pllf)
  stopCluster(cl)
}
library(microbenchmark)
pllp <- microbenchmark("Sequential Processing All Area Level Purchases" = {lapply(1:100, pllf)}, pllfcluster())
pllp
autoplot(pllp)
#parallel processing ends here