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
pllp <- microbenchmark("Parallel Processing All Area Level Purchases" = {lapply(1:100, pllf)}, pllfcluster())
pllp
autoplot(pllp)
#parallel processing ends here

#hypothesis testing starts here
objFile<-read.csv("C:/Users/user/OneDrive/Desktop/BIG DATA/objective.csv")
View(objFile)

esaturate <- lm(f_obese ~ energy_saturate, data = objFile)
print(summary(esaturate))

eprotein <- lm(f_obese ~ energy_protein, data = objFile)
print(summary(eprotein))
#hypothesis testing ends here

#correlation starts here
highestcorrelation <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("Energy Saturate", "Energy Protein","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
highestcorr <- select(objFile, 'f_obese', 'energy_saturate', 'energy_protein')
highestcorrelation(highestcorr, 10)

#correlation between energy_saturate and f_obese
library("ggpubr")
ggscatter(objFile, x = "energy_saturate", y = "f_obese", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Saturate", ylab = "Affect to obesity")


#correlation between energy_protein and f_obese
library("ggpubr")
ggscatter(objFile, x = "energy_protein", y = "f_obese", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Energy Protein", ylab = "Affect to obesity")
#correlation ends here

#regression starts here
#regression between energy_saturate and f_obese
x <- c(objFile$energy_saturate)
y <- c(objFile$f_obese)
relation <- lm(y~x)

plot(y,x,col = "blue",main = "Energy Saturate Affect to obesity",
     abline(lm(x~y)),cex = 1.1,pch = 13,xlab = "energy_saturate",ylab = "f_obese")


#regression between energy_protein and f_obese
x2 <- c(objFile$energy_protein)
y2 <- c(objFile$f_obese)
relation <- lm(y~x)

plot(y2,x2,col = "red",main = "Energy Protein Affect to obesity",
     abline(lm(x2~y2)),cex = 1.1,pch = 13,xlab = "energy_protein",ylab = "f_obese")
#regression ends here


