library(data.table)
library(shinydashboard)
library(scales)
library(leaflet)
library(DT)
library(maps)
library(googleVis)

tourism = read.csv('Combined_tourism_Data.csv',stringsAsFactors = F)
tourism$X=NULL
tourism = tourism %>% select(.,Country.Name,Country.Code,Region,IncomeGroup,year,arrivals,departures,total_receipts,
                             receipt_transport,receipt_travel,ratio_exports,total_expenditures,expenditure_transport,
                             expenditure_travel)

country = tourism$Country.Name
currency = tourism
currency[c(8:10,12:14)] = sapply((currency[c(8:10,12:14)]),dollar_format())
currency[6:7] = sapply(currency[6:7],comma)
currency[11] = percent(currency$ratio_exports/100)

most_arrivals = tourism %>% filter(.,year == 2017) %>% arrange(.,desc(arrivals))

# Geo=gvisGeoChart(tourism, locationvar="Country.Name",hovervar = "Country.Name")
# plot(Geo)
