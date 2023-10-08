library(tidyverse)
library(ggplot2)

setwd("/Users/zhanranl/Desktop/Wharton/Courses/statME/hw2-chris_lin/")
getwd()

num <- 50000
p_list <- c(2,25,50,75,99)
n <- 100

for (i in 1:5){
  p <- p_list[3]
  sample <- rf(num, p-1, n-p)
  sample <- sample/(sample + (n-p)/(p-1))
  fig <-  hist(sample, breaks = seq(0, 1, by = 0.01), main = paste("p =",p), xlab = "Value", ylab = "Frequency", col = "orange")
  file_name <- paste("./figures-and-tables/2-2-", p, ".png", sep = "")
  ggsave(file_name, plot = fig)
  }
