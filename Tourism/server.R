

shinyServer(function(input, output){
  
  output$mymap = renderGvis({
    geo = gvisGeoChart(front_map %>% mutate(Visitors=arrivals),locationvar = 'Country.Code',colorvar = 'Visitors',hovervar = c('Country.Name'),
                             options=list(title='Country Tourism in 2017',width = 900,height=650,colorAxis="{colors:['white','orangered']}",background='blue'))  
    })
  
  output$hotCountry = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Country', sample(most_arrivals$Country.Name,replace = T), icon=icon('flag'),color='light-blue',fill=F)
  })
  
  output$year = renderInfoBox({
    infoBox('Year',most_arrivals$year,icon=icon('hourglass'),color = 'light-blue',fill = F)
  })

  output$tourists = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Tourists',sample(comma(most_arrivals$arrivals),replace = T),icon=icon('users'),color = 'light-blue',fill = F)
  })
  
  output$table_country = DT::renderDataTable(
    tourism %>% filter(.,Country.Name == input$selected_country) %>% select(.,-1:-4) %>% arrange(.,desc(year)),
    options = list(scrollX=TRUE)
  )
  
  country_data = reactive({
    tourism %>% filter(.,Country.Name==input$selected_country)
  })
  
  output$country_abb = renderInfoBox({
    infoBox((country_data() %>% select(.,Country.Code))[1],
            input$selected_country,
            icon=icon('flag'),color = 'blue',fill = TRUE)
  })
  
  output$region = renderInfoBox({
    infoBox('REGION',(country_data() %>% select(.,Region))[1],
            icon = icon('globe-americas'),color = 'light-blue',fill = TRUE)
  })
  
  output$income = renderInfoBox({
    infoBox('INCOME GROUP',(country_data() %>% select(.,IncomeGroup))[1],
            icon = icon('coins'),color = 'teal',fill = TRUE)
  })
  
  output$table_year = DT::renderDataTable(
    tourism %>% filter(.,year==input$selected_year1) %>% select(.,-2:-5) %>% arrange(.,desc(arrivals)),
    options = list(scrollX=TRUE)
  )
  
  country_year = reactive({
    tourism %>% filter(.,year == input$selected_year1)
  })
  
  output$most_visited = renderInfoBox({
    infoBox(paste('Most Visited in',input$selected_year1),(country_year() %>% arrange(desc(arrivals)))$Country.Name[1],
            icon=icon('flag'),color = 'red',fill=T)
  })
  
  output$most_money = renderInfoBox({
    infoBox(paste('Most Money in',input$selected_year1),(country_year() %>% arrange(desc(total_receipts)))$Country.Name[1],
            icon=icon('coins'),color = 'orange',fill=T)
  })
  
  output$ratio_export = renderInfoBox({
    infoBox(paste('Highest Export Ratio in',input$selected_year1),(country_year() %>% arrange(desc(ratio_exports)))$Country.Name[1],
            icon=icon('percentage'),color = 'yellow',fill=T)
  })
  
  
  output$table_region = DT::renderDataTable(
    tourism %>% group_by(.,Region,year) %>% filter(.,year == input$selected_year) %>% select(c(-1:-5)) %>% 
      summarise_all(.,function(x){ceiling(mean(x,na.rm = T))}) %>% arrange(.,desc(arrivals)),
    options = list(scrollX=TRUE)
  )
  
  country_year3 = reactive({
    tourism %>% filter(.,year == input$selected_year)
  })
  
  output$most_region_arrival = renderInfoBox({
    infoBox(paste('Most Visited Region in',input$selected_year),(country_year3() %>% arrange(desc(arrivals)))$Region[1],
            icon=icon('globe-americas'),color = 'green',fill=T)
  })
  
  output$most_region_money = renderInfoBox({
    infoBox(paste('Most Money in',input$selected_year),(country_year3() %>% arrange(desc(total_receipts)))$Region[1],
            icon=icon('coins'),color = 'olive',fill=T)
  })
  
  output$ratio_export_region = renderInfoBox({
    infoBox(paste('Highest Export Ratio in',input$selected_year),(country_year3() %>% arrange(desc(ratio_exports)))$Region[1],
            icon=icon('percentage'),color = 'lime',fill=T)
  })
  
  
  country_arrival = reactive({
    tourism %>% filter(.,Country.Name==input$selected2)
  })
  
  output$world = renderGvis({
    world_graph = gvisLineChart(world,xvar = 'year',yvar = 's_arrivals',
                                options = list(title='Tourists per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                               width=600,height=360,backgroundColor='azure',series="[{color:'blue'}]",legend='none',
                                               vAxes="[{title:'Visitors',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                               hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]")) 
    
  })
  
  output$popular = renderGvis({
  bar_popular = gvisBarChart(head(tourism %>% filter(year==2017) %>% select(Country.Name,arrivals) %>% arrange(desc(arrivals)),15),
                             options = list(title='Popular Countries in 2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                            width=600,height=360,backgroundColor='azure',legend = 'none',
                                            vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                            hAxes="[{title:'Visitors',format:'decimal',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
})
  
  output$arrivals = renderGvis({
    line = gvisLineChart((country_arrival() %>% filter(!is.na(arrivals))),
                         xvar = 'year',yvar = c('arrivals','departures'),
                         options = list(title='Tourists per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                        width=875,height=525,backgroundColor='azure',series="[{color:'blue'}]",
                                        vAxes="[{title:'Visitors',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                        hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]") 
                         )
  })
  
  output$change = renderGvis({
    bar_change = gvisBarChart(head(p_change %>% arrange(desc(percent)),15),
                               options = list(title='Largest Increase from 2012-2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                              width=600,height=360,backgroundColor='azure',legend = 'none',
                                              vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                              hAxes="[{title:'Percent Change',format:'##',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  
  output$change2 = renderGvis({
    bar_change = gvisBarChart(head(p_change %>% arrange(percent),15),
                              options = list(title='Largest Decrease from 2012-2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                             width=600,height=360,backgroundColor='azure',legend = 'none',
                                             vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                             hAxes="[{title:'Percent Change',format:'##',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  
  
  #Selected Country
  output$country_abb2 = renderValueBox({
    valueBox(input$selected2,country_arrival()$Country.Code[1],
             icon=icon('flag'),color='teal')
  })
  
  #Latest data of Arrivals
  output$recent_arrivals = renderValueBox({
    valueBox(comma((country_arrival() %>% filter(!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[1]),
             paste('People Visited in',max((country_arrival() %>% filter(!is.na(arrivals)))$year)),
             icon=icon('users'),color='teal')
  })
  
  #Latest data of Departure
  output$recent_departures = renderValueBox({
    valueBox(comma((country_arrival() %>% filter(!is.na(departures)) %>% arrange(.,desc(year)))$departures[1]),
             paste('People Traveled in',max((country_arrival() %>% filter(!is.na(departures)))$year)),
             icon=icon('users'),color='teal')
  })
  
  # Percent change in Visitor
  output$percent_change1.1 = renderUI({
    valueBox(paste0(round((((country_arrival() %>% filter(!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[1])/
                            ((country_arrival() %>% filter(!is.na(arrivals)) %>% arrange(.,desc(year)))$arrivals[2])-1)*100,2),'%'), 
    paste('Arrivals',max((country_arrival() %>% filter(!is.na(arrivals)))$year)),icon=icon('percent'),color='maroon',width = 12)
  })
  
  # Percent change Travel
  output$percent_change1.2 = renderUI({
    valueBox(paste0(round((((country_arrival() %>% filter(!is.na(departures)) %>% arrange(.,desc(year)))$departures[1])/
                            ((country_arrival() %>% filter(!is.na(departures)) %>% arrange(.,desc(year)))$departures[2])-1)*100,2),'%'), 
    paste('Departures',max((country_arrival() %>% filter(!is.na(departures)))$year)),icon=icon('percent'),color='maroon',width = 12)
  })
  
  # output$departures = renderGvis({
  #   line = gvisLineChart((country_departure()),
  #                        xvar = 'year',yvar = 'departures',
  #                        options = list(title='Resident Travelers per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
  #                                       width=800,height=480,backgroundColor='azure',
  #                                       vAxes="[{title:'Travelers',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
  #                                       hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]") 
  #   )
  # })
  
  # # Percent change in most recent year
  # output$percent_change2.1 = renderValueBox({
  #   valueBox(paste0(round((((country_departure() %>% arrange(.,desc(year)))$departures[1])/
  #                           ((country_departure() %>% arrange(.,desc(year)))$departures[2])-1)*100,2),'%'), 
  #            paste('Percent Change in',max(country_departure()$year)),icon=icon('percent'),color='olive')
  # })
  # 
  # # Percent change lifetime
  # output$percent_change2.2 = renderValueBox({
  #   valueBox(paste0(round((((country_departure() %>% arrange(.,desc(year)))$departures[1])/
  #                           ((country_departure() %>% arrange(.,year))$departures[1])-1)*100,2),'%'), 
  #            paste('Percent Change Since',min(country_departure()$year)),icon=icon('percent'),color='olive')
  # })
  
  country_receipt = reactive({
    tourism %>% filter(.,Country.Name == input$receipts,!is.na(total_receipts))
  })
  
  output$receipts = renderGvis({
    line_receipts = gvisLineChart(country_receipt(),
                                  xvar = 'year',yvar = c('receipt_travel','receipt_transport','total_receipts'),
                                  options = list(title='Tourism Gross Profit per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                                 width=600,height=360,backgroundColor='azure', series="[{color:'orange'},{color:'red'},{color:'blue'}]",
                                                 vAxes="[{title:'Gross Profit ($USD)',format:'short',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                                 hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]")
    )
  })
  
  output$most_money2 = renderGvis({
    bar_money = gvisBarChart(head(tourism %>% filter(year==2017) %>% select(Country.Name,total_receipts) %>% arrange(desc(total_receipts)),15),
                               options = list(title='Top Receipts from Countries in 2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                              width=600,height=360,backgroundColor='azure',legend = 'none',
                                              vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                              hAxes="[{title:'$USD',format:'##',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  
  output$percent_change3.1 = renderValueBox({
    valueBox(paste0(round((((country_receipt() %>% arrange(.,desc(year)))$total_receipts[1]/(country_receipt() %>% arrange(.,desc(year)))$total_receipt[2]-1)*100),2),'%'),
             paste('Percent Change in ',max(country_receipt()$year)),
             icon=icon('percent'),color='orange')
  })
  
  output$percent_change3.2 = renderValueBox({
    valueBox(paste0(round((((country_receipt() %>% arrange(.,desc(year)))$total_receipts[1]/(country_receipt() %>% arrange(.,year))$total_receipt[1]-1)*100),2),'%'),
             paste('Percent Change Since ',min(country_receipt()$year)),
             icon=icon('percent'),color='orange')

  })
  
  output$latest_receipt = renderValueBox({
    valueBox((country_receipt() %>% arrange(.,desc(year)))$total_receipt[1],
             paste('Receipt in',max(country_receipt()$year)),
             icon=icon('money-bill-wave'),color='orange')
    
  })
  
  country_impact = reactive({
    tourism %>% filter(.,year == input$selected_year2,IncomeGroup != 'Low income')
  })
  
  output$impact = renderGvis({
    line_impact = gvisBarChart(head(country_impact() %>% filter(!is.na(ratio_exports)) %>% select(Country.Name,ratio_exports) %>% arrange(desc(ratio_exports)),15),
                                  options = list(title='Proportion of Tourism on Total Exports',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                                 width=600,height=360,backgroundColor='azure',legend = 'none',
                                                 vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                                 hAxes="[{title:'Percent of Total Exports',format:'##',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  
  output$dollar_per = renderGvis({
    line_dollar = gvisBarChart(head(country_impact() %>% filter(!is.na(dollar_per)) %>% select(Country.Name,dollar_per) %>% arrange(desc(dollar_per)),15),
                               options = list(title='Dollar per Visitor',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                              width=600,height=360,backgroundColor='azure',legend = 'none',
                                              vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                              hAxes="[{title:'$USD per Visitor',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  output$percent_change4.1 = renderValueBox({
    valueBox(paste0(round((((country_expenditure() %>% arrange(.,desc(year)))$total_expenditures[1]/(country_expenditure() %>% arrange(.,desc(year)))$total_expenditures[2]-1)*100),2),'%'),
             paste('Percent Change in ',max(country_expenditure()$year)),
             icon=icon('percent'),color='olive')
  })
  
  output$percent_change4.2 = renderValueBox({
    valueBox(paste0(round((((country_expenditure() %>% arrange(.,desc(year)))$total_expenditures[1]/(country_expenditure() %>% arrange(.,year))$total_expenditures[1]-1)*100),2),'%'),
             paste('Percent Change Since ',min(country_expenditure()$year)),
             icon=icon('percent'),color='olive')
    
  })
  
  output$latest_expenditure = renderValueBox({
    valueBox((country_expenditure() %>% arrange(.,desc(year)))$total_expenditures[1],
             paste('Expenditure in',max(country_expenditure()$year)),
             icon=icon('money-bill-wave'),color='olive')
    
  })
  
  output$highest_ratio = renderUI({
    infoBox(paste('Highest Ratio in',country_impact()$year[1]),
            (country_impact() %>% arrange(desc(ratio_exports)))$Country.Name[1],
             icon=icon('percent'),color='navy',width=8)
    
  })
  
  output$most_expensive = renderUI({
    infoBox(paste('Most Expensive in',country_impact()$year[1]),
            (country_impact() %>% arrange(.,desc(dollar_per)))$Country.Name[1],
             icon=icon('money-bill-wave'),color='navy',width=8)
    
  })
})
