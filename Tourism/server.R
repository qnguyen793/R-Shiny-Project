
shinyServer(function(input, output){
  
  output$mymap = renderLeaflet({
    leaflet() %>% addProviderTiles('Esri.NatGeoWorldMap')
  })
  
  output$hotCountry = renderInfoBox({
    infoBox('Hot Country', most_arrivals$Country.Name, icon=icon('hotjar'),color='red',fill=TRUE)
  })
  
  output$year = renderInfoBox({
    infoBox('Year',most_arrivals$year,icon=icon('hourglass'),color = 'red',fill = TRUE)
  })

  output$tourists = renderInfoBox({
    infoBox('Tourists',comma(most_arrivals$arrivals),icon=icon('users'),color = 'red',fill = TRUE)
  })
  
  output$table = DT::renderDataTable(
    tourism %>% filter(.,Country.Name == input$selected) %>% select(.,-1:-4),
    options = list(scrollX=TRUE)
  )
  
  output$pick_country = renderInfoBox({
    infoBox(input$selected, (tourism %>% filter(.,Country.Name == input$selected) %>% select(.,Country.Code)[[1]][2]),
            icon=icon('flag'),color = 'blue',fill = TRUE)
  })
  
})
