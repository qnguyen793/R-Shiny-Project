
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
      menuItem('Tourism by the Person',tabName = 'Tourism by the Person',icon = icon('map-marked-alt'),
                menuItem('Inbound',tabName = 'Inbound',icon=icon('plane-arrival')),
                menuItem('Outbound',tabName = 'Outbound',icon = icon('plane-departure'))),
      menuItem('Tourism by the Dollar',tabName = 'Tourism by the Dollar',icon = icon('coins'),
               menuItem('Receipt',tabName = 'Receipt',icon=icon('money-bill-wave')),
               menuItem('Expenditure',tabName = 'Expenditure',icon=icon('cc-apple-pay')))
                )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Home',
              fixedRow(responsive = NULL,
                infoBoxOutput('hotCountry'),
                infoBoxOutput('year'),
                infoBoxOutput('tourists'),
                box(HTML('This shiny app has a collection of data on tourism throughout the world. How many tourists visit a country in a certain year?
How many people travel from a certain country in a year? Which countries have become hotbeds for international tourism? How much does the tourism industry impact an economy?
                         Find the answer playing around with the app. Maybe it could help you book your next trip!'),
                         title = 'Travel the World', width=12, status='success',solidHeader = TRUE),
                
                leafletOutput('mymap')
              )),
      tabItem(tabName = 'Country',
              fluidRow(
                column(width = 4,offset =0.5,
                       selectizeInput('selected','Select Country to Display',country))),
              fluidRow(
                infoBoxOutput('country_abb'),
                infoBoxOutput('region'),
                infoBoxOutput('income')),
              fluidRow(
                column(width=12,offset=0.5,
                       DT::dataTableOutput('table')
              )),
              fluidRow(
                column(12,
                box(HTML("*arrivals: Number of tourists visited that year <br/> *departures: Number of residence traveling from country that year <br/> 
                                *total_receipt: Total amount visitors spent traveling to country ($USD) <br> *receipt_transport: Portion of total_receipt used on transportation (flights, trains, etc.) ($USD) <br/> 
                                *receipt_travel: Portion of total_receipt spent in the country ($USD) <br/> *ratio_exports: Ratio of total_recipts (tourism industry) to a country's total exports (%) <br/> 
                                *total_expenditures: Total amount residences spent traveling to a different country ($USD) <br/> *expenditures_transport: Portion of total_expenditures used on transportation (flights, trains, etc.) ($USD) <br/> 
                                *expenditures_travel: Potion of total_expenditures spent in the traveling country ($USD)"),title = 'KEY',status='danger',solidHeader = TRUE,width=12)
              ))),
      tabItem(tabName = 'Inbound',
              fluidRow(
                column(12,offset=3,
                       box(title='Inbound Tourism',status = 'success',solidHeader = T))
              ),
              fluidRow(
                column(width=4,offset = 0.5,
                       selectizeInput('selected2','Select Country to Display',country))), 
              fluidRow(
                htmlOutput('arrivals')),
              fluidRow(
                valueBoxOutput('percent_change1.1'),
                valueBoxOutput('percent_change1.2'),
                valueBoxOutput('recent_arrivals')
              )),
      tabItem(tabName = 'Outbound',
              fluidRow(
                column(12,offset=3,
                       box(title='Oubound Tourism',status = 'success',solidHeader = T))
              ),
              fluidRow(
                column(width=4,offset = 0.5,
                       selectizeInput('selected3','Select Country to Display',country))),
                fluidRow(
                  htmlOutput('departures'),
                  fluidRow(
                    valueBoxOutput('percent_change2.1'),
                    valueBoxOutput('percent_change2.2'),
                    valueBoxOutput('recent_departures')
                  )
              ))
      tabItem()
        
    )
  )
))

