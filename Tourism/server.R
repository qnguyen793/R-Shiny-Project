
shinyServer(function(input, output){
  
  output$mymap = renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.NatGeoWorldMap')
  })
  
  output$hotCountry = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Country', sample(most_arrivals$Country.Name,replace = T), icon=icon('flag'),color='blue',fill=F)
  })
  
  output$year = renderInfoBox({
    infoBox('Year',most_arrivals$year,icon=icon('hourglass'),color = 'blue',fill = F)
  })

  output$tourists = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Tourists',sample(comma(most_arrivals$arrivals),replace = T),icon=icon('users'),color = 'blue',fill = F)
  })
  
  output$table = DT::renderDataTable(
    currency %>% filter(.,Country.Name == input$selected) %>% select(.,-1:-4) %>% arrange(.,desc(year)),
    options = list(scrollX=TRUE)
  )
  
  output$country_abb = renderInfoBox({
    infoBox((tourism %>% filter(.,Country.Name == input$selected) %>% select(.,Country.Code))[1],
            input$selected,
            icon=icon('flag'),color = 'red',fill = TRUE)
  })
  
  output$region = renderInfoBox({
    infoBox('REGION',(tourism %>% filter(.,Country.Name == input$selected) %>% select(.,Region))[1],
            icon = icon('globe-americas'),color = 'orange',fill = TRUE)
  })
  
  output$income = renderInfoBox({
    infoBox('INCOME GROUP',(tourism %>% filter(.,Country.Name == input$selected) %>% select(.,IncomeGroup))[1],
            icon = icon('coins'),color = 'yellow',fill = TRUE)
  })
  
  output$arrivals = renderGvis({
    line = gvisLineChart((tourism %>% group_by(.,Country.Name) %>% filter(.,Country.Name == input$selected2,!is.na(arrivals))),
                         xvar = 'year',yvar = 'arrivals',
                         options = list(title='Tourists per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                        width=800,height=500,backgroundColor='azure4',
                                        vAxes="[{title:'Visitors',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                        hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]") 
                         )
  })
  
  # Percent change in most recent year
  output$percent_change1.1 = renderValueBox({
    valueBox(percent(as.numeric(round(((group_country %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[1])/
                            ((group_country %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[2])-1,2))), 
    'Change in Most Recent Year',icon=icon('percent'),color='olive')
  })
  
  # Percent change lifetime
  output$percent_change1.2 = renderValueBox({
    valueBox(percent(as.numeric(round(((group_country %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[1])/
                            ((group_country %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)) %>% arrange(.,year))$arrivals[1])-1,2))), 
             'Change Since 1995',icon=icon('percent'),color='olive')
  })
  
  #Latest data of Arrivals
  output$recent_arrivals = renderValueBox({
    valueBox(comma((group_country %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[1]),
             paste('People Visited in',max((tourism %>% filter(.,Country.Name == input$selected2,!is.na(arrivals)))$year)),
             icon=icon('users'),color='olive')
  })
  
  output$departures = renderGvis({
    line = gvisLineChart((tourism %>% group_by(.,Country.Name) %>% filter(.,Country.Name == input$selected3,!is.na(departures))),
                         xvar = 'year',yvar = 'departures',
                         options = list(title='Resident Travelers per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                        width=800,height=500,backgroundColor='gray',
                                        vAxes="[{title:'Travelers',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                        hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]") 
    )
  })
  # Percent change in most recent year
  output$percent_change2.1 = renderValueBox({
    valueBox(percent(as.numeric(round(((group_country %>% filter(.,Country.Name == input$selected2,!is.na(departures)) %>% arrange(.,desc(year)))$departures[1])/
                            ((group_country %>% filter(.,Country.Name == input$selected2,!is.na(departures)) %>% arrange(.,desc(year)))$departures[2])-1,2))), 
             'Change in Most Recent Year',icon=icon('percent'),color='olive')
  })
  
  # Percent change lifetime
  output$percent_change2.2 = renderValueBox({
    valueBox(percent(as.numeric(round(((group_country %>% filter(.,Country.Name == input$selected2,!is.na(departures)) %>% arrange(.,desc(year)))$departures[1])/
                            ((group_country %>% filter(.,Country.Name == input$selected2,!is.na(departures)) %>% arrange(.,year))$departures[1])-1,2))), 
             'Change Since 1995',icon=icon('percent'),color='olive')
  })
  
  #Latest data of Arrivals
  output$recent_departures = renderValueBox({
    valueBox(comma((group_country %>% filter(.,Country.Name == input$selected2,!is.na(departures)) %>% arrange(.,desc(year)))$departures[1]),
             paste('People Traveled in',max((tourism %>% filter(.,Country.Name == input$selected2,!is.na(departures)))$year)),
             icon=icon('users'),color='olive')
  })
  
  

})
