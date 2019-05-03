library(data.table)
library(shinydashboard)
library(scales)
library(leaflet)
library(DT)

tourism = read.csv('Combined_tourism_Data.csv')
tourism$X=NULL
tourism = tourism %>% select(.,Country.Name,Country.Code,Region,IncomeGroup,year,arrivals,departures,total_receipts,
                             receipt_transport,receipt_travel,ratio_exports,total_expenditures,expenditure_transport,
                             expenditure_travel)

country = tourism$Country.Name

most_arrivals = tourism %>% filter(.,year == 2017) %>% arrange(.,desc(arrivals))


