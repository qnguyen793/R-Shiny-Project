
library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
  skin = 'green',
  
  dashboardHeader(title = "World Tourism",titleWidth = 250), 
  
  dashboardSidebar(width = 250,
    sidebarUserPanel('',image = 'world.jpg'),
    sidebarMenu(
      menuItem('Home',tabName = 'Home',icon = icon('home'),badgeLabel = 'new',badgeColor = 'blue'),
      menuItem('Country',tabName = 'Country',icon = icon('flag')),
      menuItem('Tourism by the Numbers',tabName = 'Tourism by the Numbers',icon = icon('map-marked-alt'),
                menuItem('Arrivals',tabName = 'Arrivals',icon=icon('plane-arrival')),
                menuItem('Departures',tabName = 'Departures',icon = icon('plane-departure'))),
      menuItem('Tourism Profit',tabName = 'Tourism Profit',icon = icon('coins'))
                )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Home',
              fixedRow(responsive = NULL,
                infoBoxOutput('hotCountry'),
                infoBoxOutput('year'),
                infoBoxOutput('tourists'),
                leafletOutput('mymap'),
                box(title = 'Travel the World', status='success',solidHeader = TRUE)
              )),
      tabItem(tabName = 'Country',
              fluidRow(
                selectizeInput('selected','Select Country to Display',country),
                infoBoxOutput('pick_country'),
                # infoBoxOutput(),
                # infoBoxOutput(),
                DT::dataTableOutput('table')
              )),
      tabItem(tabName = 'Country Tourism'),
      tabItem(tabName = 'Country Profit')
    )
  )
))

