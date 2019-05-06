library(data.table)
library(shinydashboard)
library(scales)
library(leaflet)
library(DT)
library(maps)
library(googleVis)
library(dplyr)
library(ggmap)
library(ggplot2)
library(MazamaSpatialUtils)

tourism = read.csv('Combined_tourism_Data.csv',stringsAsFactors = F)
tourism$X=NULL
tourism$ratio_exports = sapply(tourism$ratio_exports,function(x){round(x,2)})
tourism = tourism %>% mutate(dollar_per = ceiling(total_receipts/arrivals))
tourism = tourism %>% select(.,Country.Name,Country.Code,Region,IncomeGroup,year,arrivals,total_receipts,dollar_per,ratio_exports,
                             receipt_transport,receipt_travel,departures,total_expenditures,expenditure_transport,
                             expenditure_travel)

country = tourism$Country.Name
year = tourism %>% select(.,year) %>% arrange(.,desc(year))
region = tourism %>% select(.,Region) %>% arrange(.,Region)
income = tourism %>% select(.,IncomeGroup)

most_arrivals = tourism %>% filter(.,year == 2017) %>% arrange(.,desc(arrivals))
front_map = tourism %>% filter(year==2017) %>% mutate(Country.Code = iso3ToIso2(Country.Code))

world = tourism %>% group_by(year) %>% filter(!is.na(arrivals)) %>% summarise(s_arrivals=sum(arrivals))


m = tourism %>% group_by(Country.Name) %>% filter(year %in% c(2012,2017),IncomeGroup != 'Low income',na.omit(arrivals))
n = (m %>% filter(year==2012,!is.na(arrivals)))
p = (m %>% filter(year==2017,!is.na(arrivals)))
s = inner_join(n,p,by = "Country.Name") %>% mutate(change=arrivals.y-arrivals.x) %>% 
  mutate(percent = change/arrivals.x*100)
p_change = s %>% select(Country.Name,percent)
# Geo=gvisGeoChart(tourism, locationvar="Country.Name",hovervar = "Country.Name")
# plot(Geo)
