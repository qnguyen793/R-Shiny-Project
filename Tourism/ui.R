
library(shiny)
library(shinydashboard)


shinyUI(dashboardPage(
  skin = 'green',
  
  dashboardHeader(title = "World Tourism",titleWidth = 250),

  dashboardSidebar(width = 250,
    sidebarUserPanel('',image = 'world.jpg'),
    sidebarMenu(
      menuItem('Home',tabName = 'Home',icon = icon('home'),badgeLabel = 'new',badgeColor = 'blue'),
      menuItem('Number of Tourists',tabName = 'Numbers',icon = icon('user-friends'),
               menuItem('World',tabName = 'World',icon = icon('globe')),
               menuItem('By Country',tabName = 'by_country',icon = icon('flag'))),
      menuItem('Tourism by the Dollar',tabName = 'Tourism by the Dollar',icon = icon('coins'),
               menuItem('Receipt',tabName = 'Receipt',icon=icon('money-bill-wave')),
               menuItem('Tourism Impact',tabName = 'Export_Ratio',icon=icon('business-time'))),
      menuItem('Tourism Data',tabName = 'Country',icon = icon('database'))
      
      )
  ), 
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Home',
              fluidRow(
                infoBoxOutput('randCountry'),
                infoBoxOutput('randyear'),
                infoBoxOutput('randtourists')
              ),
              fluidRow(
                box(HTML("This shiny app explores a collection of data on tourism throughout the world since 1995. 
                        From looking at different features of tourism we can find out which countries are trending towards more international visitations and which countries rely on tourism in their economy.
                         In future analyses I'd like to see which tourist activites are attracting the most attention in popular countries. Enjoy looking through the app, maybe you'll find your next vacation spot!"),
                         title = 'Travel the World', width=12, status='success',solidHeader = TRUE)
                ),
              fluidRow(
                tabBox(
                  width=12,
                  tabPanel('World Map 2017', htmlOutput('mymap')),
                  tabPanel('Tourism Video',tags$iframe(src = "https://www.youtube.com/embed/O3bx5miizBw",width = 800,height=450))))

              ),

      tabItem(tabName = 'World',
              fluidRow(
                box(title = 'World Tourism',width=12,status = 'success',solidHeader = T,
                  valueBox(comma((world %>% filter(year==2017))$Total_Tourists[1]),'Travelers in 2017',color = 'purple',icon = icon('users')),
                  valueBox(comma((world %>% filter(year==1995))$Total_Tourists[1]),'Travelers in 1995',color='purple',icon=icon('users')),
                  valueBox(paste0(round((((world %>% filter(year==2017))$Total_Tourists[1])/((world %>% filter(year==1995))$Total_Tourists[1])-1)*100,2),'%'),
                           'Percent Increase Since 1995',color='purple',icon = icon('chart-line'))
                )
              ),
              fluidRow(
                box(width=12,status='success',
                column(6,
                  plotlyOutput('worldplotly')),
                column(6,
                  plotlyOutput('popularplotly'))
              ))),
      
      tabItem(tabName ='by_country',
              fluidRow(
                box(title='Country Tourism',status = 'success',solidHeader = T, width=12,
                    fluidRow(
                      column(4,offset = 0.5,
                            selectizeInput('selected2','Select Country to Display',country,selected=T))),
                    fluidRow(
                      valueBoxOutput('country_abb2'),
                      valueBoxOutput('recent_arrivals'),
                      valueBoxOutput('recent_departures')
                    ))),
                fluidRow(
                  column(9,
                         htmlOutput('arrivals')),
                  column(3,
                         box(title = 'Percent Change in Recent Year',status = 'danger',width=NULL,
                          fluidRow(uiOutput('percent_change1.1')),
                          fluidRow(uiOutput('percent_change1.2'))))
                        ),
              br(),
              fluidRow(
                column(6,
                       htmlOutput('change')),
                column(6,
                       htmlOutput('change2'))
              )
              
            ),
                    
      tabItem(tabName = 'Receipt',
              fluidRow(
                box(title='Money from Tourism',status='success',solidHeader = T,width = 12,
                    fluidRow(
                      column(width=4,offset=0.5,
                             selectizeInput('receipts','Select Country to Display',country,selected=T))),
                    fluidRow(
                      valueBoxOutput('percent_change3.1'),
                      valueBoxOutput('percent_change3.2'),
                      valueBoxOutput('latest_receipt')
                    ))),
              fluidRow(
                column(6,offset = 0.5,
                       htmlOutput('receipts')),
                column(6,
                       htmlOutput('most_money2'))

              )
      ),
      tabItem(tabName = 'Export_Ratio',
              fluidRow(
                box(title='Tourism Impact',status='success',solidHeader = T,width = 12,
                    fluidRow(
                      column(width=4,offset=0.5,
                             selectizeInput('selected_year2','Select Year to Display',year,selected=T))))),

              fluidRow(
                column(6,offset = 0.5,
                       htmlOutput('impact')),
                column(6,
                       htmlOutput('dollar_per'))
              ),
              br(),
              fluidRow(
                column(6,offset = 0.5,
                       uiOutput('highest_ratio')),
                column(6,
                       uiOutput('most_expensive'))
              ),
              fluidRow(
                box(title='Tourism to Export Ratio',status='success',solidHeader = T,width = 12,
                    fluidRow(
                      column(width=4,offset=0.5,
                             selectizeInput('ratio_country','Select Country to Display',country,selected=T))
                    ))
              ),
              fluidRow(
                column(12,
                       htmlOutput('country_ratio'))
              )),
      tabItem(tabName = 'Country',
              tabBox(width=12,
                     tabPanel('Country',
                              fluidRow(
                                column(width = 4,offset =0.5,
                                       selectizeInput('selected_country','Select a Country',country,selected=T))),
                              fluidRow(
                                infoBoxOutput('country_abb'),
                                infoBoxOutput('region'),
                                infoBoxOutput('income')),
                              fluidRow(
                                column(width=12,offset=0.5,
                                       DT::dataTableOutput('table_country')))),
                     tabPanel('Region',
                              fluidRow(
                                column(4,offset=0.5,
                                       selectizeInput('selected_year','Select a Year',year,selected=T))),
                              fluidRow(
                                infoBoxOutput('most_region_arrival'),
                                infoBoxOutput('most_region_money'),
                                infoBoxOutput('ratio_export_region')),
                              fluidRow(
                                column(width=12,offset=0.5,
                                       DT::dataTableOutput('table_region')))),
                     tabPanel('Year',
                              fluidRow(
                                column(4,offset=0.5,
                                       selectizeInput('selected_year1','Select a Year',year,selected=T))),
                              fluidRow(
                                infoBoxOutput('most_visited'),
                                infoBoxOutput('most_money'),
                                infoBoxOutput('ratio_export')),
                              fluidRow(
                                column(width=12,offset=0.5,
                                       DT::dataTableOutput('table_year'))))
              ),
              fluidRow(
                column(12,
                       box(HTML("*arrivals: Number of tourists visited that year <br/> *total_receipt: Total amount that visitors spent traveling to country ($USD) <br/> *total_exports: All transactions composed of reseidents of a country and the rest of the world ($USD) <br/> 
                                *ratio_exports: Ratio of total_recipts (tourism industry) to a country's total exports (%) <br/> *dollar_per: Average receipt per tourist ($USD) <br> *receipt_transport: Portion of total_receipt used on transportation (flights, trains, etc.) ($USD) <br/> 
                                *receipt_travel: Portion of total_receipt spent in the country ($USD) <br/> *departures: Number of residence traveling from country that year <br/> *total_expenditures: Total amount residences spent traveling to a different country ($USD) <br/> 
                                *expenditures_transport: Portion of total_expenditures used on transportation (flights, trains, etc.) ($USD) <br/> *expenditures_travel: Potion of total_expenditures spent in the traveling country ($USD)"),
                           title = 'KEY',status='danger',solidHeader = TRUE,width=12)
                )))
              
        
    )
  )
))

