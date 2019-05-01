library(dplyr)
library(ggplot2)
library(tidyverse)

arrivals = read.csv('International_Arrivals.csv')
colnames(arrivals)[-1:-3] = c(lapply(colnames(arrivals)[-1:-3],function(x){substr(x,2,5)}))
arrivals$Indicator.Name=NULL
arrivals = arrivals %>% gather(.,key='year',value='arrivals', c(-1:-2),na.rm = TRUE)

country_data = read.csv('Country_Region_Income.csv')
country_data = country_data[1:3]
Full = inner_join(arrivals,country_data,by='Country.Code')
Full = Full %>% filter(.,Region!='')

departures = read.csv('International_Departures.csv')
colnames(departures)[-1:-3] = c(lapply(colnames(departures)[-1:-3],function(x){substr(x,2,5)}))
departures$Indicator.Name=NULL
departures$Country.Name=NULL
departures = departures %>% gather(.,key='year',value='departures',c(-1),na.rm = TRUE)
Full = left_join(Full,departures,by=c('Country.Code','year'))

total_receipts = read.csv('Tourism_Receipts.csv')
colnames(total_receipts)[-1:-3] = c(lapply(colnames(total_receipts)[-1:-3],function(x){substr(x,2,5)}))
total_receipts$Indicator.Name=NULL
total_receipts$Country.Name=NULL
total_receipts = total_receipts %>% gather(.,key='year',value='total_receipts',c(-1),na.rm = TRUE)
Full = left_join(Full,total_receipts,by=c('Country.Code','year'))

receipt_transport = read.csv('Receipt_for_Passenger_Transport_Items.csv')
colnames(receipt_transport)[-1:-3] = c(lapply(colnames(receipt_transport)[-1:-3],function(x){substr(x,2,5)}))
receipt_transport$Indicator.Name=NULL
receipt_transport$Country.Name=NULL
receipt_transport = receipt_transport %>% gather(.,key='year',value='receipt_transport',c(-1),na.rm = TRUE)
Full = left_join(Full,receipt_transport,by=c('Country.Code','year'))

receipt_travel = read.csv('Receipts_for_Travel_Items.csv')
colnames(receipt_travel)[-1:-3] = c(lapply(colnames(receipt_travel)[-1:-3],function(x){substr(x,2,5)}))
receipt_travel$Indicator.Name=NULL
receipt_travel$Country.Name=NULL
receipt_travel = receipt_travel %>% gather(.,key='year',value='receipt_travel',c(-1),na.rm = TRUE)
Full = left_join(Full,receipt_travel,by=c('Country.Code','year'))

ratio_exports = read.csv('Receipt_Percent_of_Total_Export.csv')
colnames(ratio_exports)[-1:-3] = c(lapply(colnames(ratio_exports)[-1:-3],function(x){substr(x,2,5)}))
ratio_exports$Indicator.Name=NULL
ratio_exports$Country.Name=NULL
ratio_exports = ratio_exports %>% gather(.,key='year',value='ratio_exports',c(-1),na.rm = TRUE)
Full = left_join(Full,ratio_exports,by=c('Country.Code','year'))

total_expenditure = read.csv('Expenditures.csv')
colnames(total_expenditure)[-1:-3] = c(lapply(colnames(total_expenditure)[-1:-3],function(x){substr(x,2,5)}))
total_expenditure$Indicator.Name=NULL
total_expenditure$Country.Name=NULL
total_expenditure = total_expenditure %>% gather(.,key='year',value='total_expenditures',c(-1),na.rm = TRUE)
Full = left_join(Full,total_expenditure,by=c('Country.Code','year'))

expenditure_transport = read.csv('Expenditure_for_Passenger_Transport_Items.csv')
colnames(expenditure_transport)[-1:-3] = c(lapply(colnames(expenditure_transport)[-1:-3],function(x){substr(x,2,5)}))
expenditure_transport$Indicator.Name=NULL
expenditure_transport$Country.Name=NULL
expenditure_transport = expenditure_transport %>% gather(.,key='year',value='expenditure_transport',c(-1),na.rm = TRUE)
Full = left_join(Full,expenditure_transport,by=c('Country.Code','year'))

expenditure_travel = read.csv('Expenditures_for_TRavel_Items.csv')
colnames(expenditure_travel)[-1:-3] = c(lapply(colnames(expenditure_travel)[-1:-3],function(x){substr(x,2,5)}))
expenditure_travel$Indicator.Name=NULL
expenditure_travel$Country.Name=NULL
expenditure_travel = expenditure_travel %>% gather(.,key='year',value='expenditure_travel',c(-1),na.rm = TRUE)
Full = left_join(Full,expenditure_travel,by=c('Country.Code','year'))

write.csv(Full,file='Combined_Tourism_Data.csv')
