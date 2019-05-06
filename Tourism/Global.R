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
library(plotly)

tourism = read.csv('Combined_tourism_Data.csv',stringsAsFactors = F)
tourism$X=NULL
tourism$ratio_exports = sapply(tourism$ratio_exports,function(x){round(x,2)})
tourism = tourism %>% mutate(dollar_per = ceiling(total_receipts/arrivals),total_exports = ceiling(total_receipts/(ratio_exports/100)))
tourism = tourism %>% select(.,Country.Name,Country.Code,Region,IncomeGroup,year,arrivals,total_receipts,total_exports,ratio_exports,dollar_per,
                             receipt_transport,receipt_travel,departures,total_expenditures,expenditure_transport,
                             expenditure_travel)

country = tourism$Country.Name #country names
year = tourism %>% select(.,year) %>% arrange(.,desc(year)) #descending years
region = tourism %>% select(.,Region) %>% arrange(.,Region) #regions
# income = tourism %>% select(.,IncomeGroup)

most_arrivals = tourism %>% filter(.,year == 2017) %>% arrange(.,desc(arrivals)) #arrivals in 2017
front_map = tourism %>% filter(year==2017) %>% mutate(Country.Code = iso3ToIso2(Country.Code)) #map in front

world = tourism %>% group_by(year) %>% filter(!is.na(arrivals)) %>% summarise(Total_Tourists=sum(arrivals)) #arrivals by year


m = tourism %>% group_by(Country.Name) %>% filter(year %in% c(2012,2017),IncomeGroup != 'Low income',na.omit(arrivals))
n = (m %>% filter(year==2012,!is.na(arrivals)))
p = (m %>% filter(year==2017,!is.na(arrivals)))
s = inner_join(n,p,by = "Country.Name") %>% mutate(change=arrivals.y-arrivals.x) %>% 
  mutate(percent = change/arrivals.x*100)
p_change = s %>% select(Country.Name,percent) #percent change from 2012-2017

