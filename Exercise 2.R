install.packages("tinytex")
library(tinytex)
###3rd assignment###
install.packages("bayesAR")
library(ggplot2)
UNRATE <- read.csv("C:/Users/miria/Desktop/SoSe2023/Macroeconometrics/3rd assignment/UNRATE.csv")
class(UNRATE)
mean<-mean(UNRATE$UNRATE)
adjusted_ts<-UNRATE$UNRATE-mean ##demean the timeseries
mean(adjusted_ts)
#fit AR(1) model with N(0,1)


         
