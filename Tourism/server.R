

shinyServer(function(input, output){
  
  ####FRONT PAGE####
  output$mymap = renderGvis({
    geo = gvisGeoChart(front_map %>% mutate(Visitors=arrivals),locationvar = 'Country.Code',colorvar = 'Visitors',hovervar = c('Country.Name'),
                             options=list(title='Country Tourism in 2017',width = 900,height=650,colorAxis="{colors:['white','orangered']}",background='blue'))  
    })
  
  output$randCountry = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Country', sample(most_arrivals$Country.Name,replace = T), icon=icon('flag'),color='light-blue',fill=F)
  })
  
  output$randyear = renderInfoBox({
    infoBox('Year',most_arrivals$year,icon=icon('hourglass'),color = 'light-blue',fill = F)
  })

  output$randtourists = renderInfoBox({
    set.seed(as.numeric(Sys.time()))
    infoBox('Tourists',sample(comma(most_arrivals$arrivals),replace = T),icon=icon('users'),color = 'light-blue',fill = F)
  })
  
  
  ####DATA PAGE####
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
  
  ####WORLD TAB####
  country_arrival = reactive({
    tourism %>% filter(.,Country.Name==input$selected2)
  })
  
  output$world = renderGvis({
    world_graph = gvisLineChart(world,xvar = 'year',yvar = 's_arrivals',
                                options = list(title='Tourism Over the Years',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                               width=600,height=360,backgroundColor='azure',series="[{color:'blue'}]",legend='none',
                                               vAxes="[{title:'Visitors',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                               hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]")) 
    
  })
  
  output$worldplotly = renderPlotly({
    world = ggplot(world,aes(x=year,y=Total_Tourists))+geom_line(color='blue')+geom_point(color='blue',size=1)+
      labs(title = 'Tourists over Years',y='Tourists')+theme(plot.background = element_rect(fill='azure'),title = element_text(color='orange'),
                                                             axis.title = element_text(color='orange'),line = element_line(colour='blue'))
    worldp = ggplotly(world)
  })
  
  output$popular = renderGvis({
  bar_popular = gvisBarChart(head(tourism %>% filter(year==2017) %>% select(Country.Name,arrivals) %>% arrange(desc(arrivals)),15),
                             options = list(title='Popular Countries in 2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                            width=600,height=360,backgroundColor='azure',legend = 'none',
                                            vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                            hAxes="[{title:'Visitors',format:'decimal',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
})
  
  output$popularplotly = renderPlotly({
    dat = head(tourism %>% filter(year==2017) %>% select(Country.Name,arrivals) %>% arrange(desc(arrivals)),15)
    pop = ggplot(dat,aes(x=Country.Name,y=arrivals))+geom_bar(stat='identity', fill='blue')+coord_flip()+scale_x_discrete(limits=rev(dat$Country.Name))+
      labs(title = 'Popular Countries in 2017',x='Country',y='Tourists')+theme(plot.background = element_rect(fill='azure'),title = element_text(color='orange'),
                                                                         axis.title = element_text(color='orange'))
    popularp = ggplotly(pop)
  })
  
  ####BY COUNTRY TAB####
  output$arrivals = renderGvis({
    line = gvisLineChart((country_arrival() %>% filter(!is.na(arrivals))),
                         xvar = 'year',yvar = c('arrivals','departures'),
                         options = list(title='Tourists per Year',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                        width=875,height=525,backgroundColor='azure',series="[{color:'blue'}]",
                                        vAxes="[{title:'Visitors',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                        hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]") 
                         )
  })
  
  # output$second_selection = renderUI({
  #   selectInput('trend2','Select Year To',year[year>input$trend1],selected=T)
  # })
  
  p_change = reactive({

    inner_join((tourism %>% group_by(Country.Name) %>% filter(year %in% c(input$trend1,input$trend2), na.omit(arrivals)) %>%
                  filter(year==input$trend1,!is.na(arrivals))),(tourism %>% group_by(Country.Name) %>% filter(year %in% c(input$trend1,input$trend2), na.omit(arrivals)) %>% 
                                                                  filter(year==input$trend2,!is.na(arrivals))),by = "Country.Name") %>% mutate(change=arrivals.y-arrivals.x) %>% 
      mutate(percent = change/arrivals.x*100) %>% select(Country.Name,percent)
    
  })
  
  output$change = renderGvis({
    bar_change = gvisBarChart(head(p_change() %>% arrange(desc(percent)),15),
                               options = list(title=paste0('Largest Increase from ',input$trend1,'-',input$trend2),titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                              width=600,height=360,backgroundColor='azure',legend = 'none',
                                              vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                              hAxes="[{title:'Percent Change',format:'##',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
  })
  
  output$change2 = renderGvis({
    bar_change = gvisBarChart(head(p_change() %>% arrange(percent),15),
                              options = list(title=paste0('Smallest Increase from ',input$trend1,'-',input$trend2),titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
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
  
  ####RECEIPT TAB####
  country_receipt = reactive({
    tourism %>% filter(.,Country.Name == input$receipts,!is.na(total_receipts))
  })
  
  output$receipts = renderGvis({
    line_receipts = gvisLineChart(country_receipt(),
                                  xvar = 'year',yvar = c('receipt_travel','receipt_transport','total_receipts'),
                                  options = list(title='Receipts from Tourism',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                                 width=600,height=360,backgroundColor='azure', series="[{color:'orange'},{color:'red'},{color:'blue'}]",
                                                 vAxes="[{title:'$USD',format:'short',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                                 hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]")
    )
  })
  
  output$most_money2 = renderGvis({
    bar_money = gvisBarChart(head(tourism %>% filter(year==2017) %>% select(Country.Name,total_receipts) %>% arrange(desc(total_receipts)),15),
                               options = list(title='Most Receipts from Tourism in 2017',titleTextStyle="{color:'darkorange',fontSize:20}",pointSize=5,
                                              width=600,height=360,backgroundColor='azure',legend = 'none',
                                              vAxes="[{title:'Country',titleTextStyle:{color:'darkorange',fontSize:16},textPosition:'out'}]",
                                              hAxes="[{title:'$USD',format:'short',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:16}}]"))
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
  
  ####TOURISM IMPACT####
  country_impact = reactive({
    tourism %>% filter(.,year == input$selected_year2)
  })
  
  output$impact = renderGvis({
    bar_impact = gvisBarChart(head(country_impact() %>% filter(!is.na(ratio_exports)) %>% select(Country.Name,ratio_exports) %>% arrange(desc(ratio_exports)),15),
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
  
  output$country_ratio = renderGvis({
    line_country_ratio = gvisLineChart(tourism %>% filter(Country.Name==input$ratio_country),
                                  xvar = 'year',yvar = 'ratio_exports',
                                  options = list(title='Ratio of Tourism Receipts to Exports',titleTextStyle="{color:'darkorange',fontSize:24}",pointSize=5,
                                                 width=1150,height=600,backgroundColor='azure', series="[{color:'blue'}]",legend='none',
                                                 vAxes="[{title:'Percent',format:'short',titleTextStyle:{color:'darkorange',fontSize:20},textPosition:'out'}]",
                                                 hAxes="[{title:'Year',format:'####',textPosition:'out',titleTextStyle:{color:'darkorange',fontSize:20}}]")
    )
    
  })
})
