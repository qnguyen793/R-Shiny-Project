library(dlpyr)
library(ggplot2)

arrivals = read.csv('International_Arrivals.csv')
colnames(arrivals)[-1:-3] = c(lapply(colnames(arrivals)[-1:-3],function(x){substr(x,2,5)}))
arrivals$Indicator.Name=NULL
