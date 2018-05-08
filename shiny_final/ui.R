
library(shiny)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  
  dashboardHeader(title = "Rise of populism \n in Europe",
                  titleWidth = 450
  ),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Electorate outcome",
      tabName = "part1",
  
      icon = icon("angle-double-right")
      
    ),
    menuItem(
      "The Italian case",
      tabName = "part2",
      icon = icon("angle-double-right")
    ),
    menuItem(
      "Main topics",
      tabName = "part3",
      icon = icon("angle-double-right")
    )
  )),
  dashboardBody(tabItems(
    
    tabItem(tabName = "part1",
            
            fluidRow(
              
              box(h1("Populist Index by country and year."), 
                  h3("The index is meant to give intuition about the main political trends every country has been through over the years"),
                  plotOutput("distPlot")),
              
              
              column(width = 4,
                     box(width = NULL, background = "light-blue",
                         
                         h2("Let's try to shed light on", align = "center"),
                         h2("the change in political trends ", align = "center"),
                         h2("in Europe over the years.", align = "center")
                         
                         
                     ),
                     box( width = NULL,radioButtons(inputId = "dist", "Elections outcome:",
                                                    c("1990/2000" = "p1",
                                                      "2000/2010" = "p2",
                                                      "2010/2015" = "p3",
                                                      "2015/2018" = "p4"))),
                     
                     
                     
                     
                     box(width = NULL, title = "Populist Index explanation", status = "danger", solidHeader = TRUE,
                         p("We calculated the index by giving a value to every party according to
                           its political orientation. The range starts from -2, indicating that a party is far-left leaning, and goes up to 2,
                           when indicating that a party is far-right leaning. The definition on the political orientation of the party has been
                           retrieved on Wikipedia.")
                         
                         
                         
                         )
                     
                     
                     
                     
                     
                     
                     
                     )),
            
            fluidRow(
              
              
              box( checkboxGroupInput("party", label = "Choose a party to display",
                                      choices = list("FIDESZ (Hungary) " = "FIDESZ",
                                                     "FN  (France) "= "FN",
                                                     "FP (Austria) "= "FP",
                                                     "League (Italy) "= "League",
                                                     "UKIP (England)"= "UKIP"),
                                      selected = c("FIDESZ","FN","FP","League","UKIP")
              )
              ),
              
              
              box(plotOutput("parties"))
            )
            
            
            
            ),
    
    
    
    tabItem(tabName = "part2",
            
            fluidRow(
              column(width = 6,
                     
                     
                    
                     
                     box(width = NULL,
                        
                     
                     box(width = NULL,title = "Data on Matteo Salvini's facebook posts",plotOutput("likes")
                     ),
                     
                     
                     actionButton("act", label = " Retrieve real time data from Salvini facebook's feed"
                     ),
                     sliderInput("DatesMerge",
                                 "Select the day from where on to retrieve the data for the above graph.",
                                 min = as.Date("2016-10-26","%Y-%m-%d"),
                                 max = as.Date("2018-04-29","%Y-%m-%d"),
                                 value=as.Date("2018-03-01"),
                                 timeFormat="%Y-%m-%d"),
                     sliderInput("frequence",
                                 "Select the minimum frequence words have to occur in order to be displayed",
                                 min = 1,
                                 max = 30,
                                 value=20
                     )
                     
                     ),
                     
                     box(width = NULL, collapsed = TRUE, title = "Data on Matteo Salvini's facebook posts", plotOutput("word")
                         
                         
                         
                     )
                     
                     
                     
              ),
              
              column(width = 6,
                     
                     
                     
                     
                     box( width = NULL,radioButtons(inputId = "reg", "Regression type:",
                                                    c("Regression on a lin-lin model" = "S3",
                                                      "Regression on a lin-lin model 
                                                      (filtered observations: the most liked post per day)" = "S5",
                                                      "Regression on a log-lin model" = "S4"
                                                    ))),
                     
                     
                     box(width=NULL, h4("We've collected from Facebook API all the posts of the representative of Lega party 
                             Matteo Salvini. We were able to find a correlation between the days passed from the italian elections of this year and
                             his influence on social media.  ")),
                     
                     
                     
                     box(width = NULL,title = "Regression on data up to 100 days from the election day",plotOutput("regression")
                     ),
                     
                     
                    
                     
                 
                         DT::dataTableOutput("mytable")
                     
                     
              )
              
              
              
                     )
            
            
            
            
            
            
            
            
    ),
    
    tabItem(tabName = "part3",
            
            fluidRow(
              column(width = 6,
                     
                     
                     box(width = NULL,title = "Different surveys regarding the perception of immigration in Europe", plotOutput("immigration"),
                         
                         selectInput("immigr", label = h3("Select survey's topic:"), 
                                     choices = list("Immigrants and jobs" = "immigrants_analysis_1.csv",
                                                    "Immigrants and services" = "immigrants_analysis_2.csv",
                                                    "Immigrants and crime" = "immigrants_analysis_3.csv"), 
                                     selected = "immigrants_analysis_1.csv")
                     ),
                     
                     box(width = NULL, title = "Perception on the European Union by country", plotOutput("topics"),
                         selectInput("Country", label = h3("Select a country:"), 
                                     choices = list("Italy" = "IT",
                                                    "Spain" = "ES",
                                                    "France" = "FR",
                                                     "Netherlands" = "NL",
                                                      "Poland" = "PL",
                                                       "Hungary" = "HU",
                                                       "Average" = "EU28"), 
                                     selected = "IT")
                         
                         )
                     
                     
                     
                     
                     
              ),
              
              column(width = 6,
                     
                     box(width = NULL, title = "Perception on the number of immigrants in Europe per 100 people", plotOutput("immigr_n"),
                         
                            sliderInput("imm_bins", "Select the number of bins",
                                        min = 5,
                                        max = 100,
                                        value = 55),
                            sliderInput("focus", "Pick the limit for the x-axis",
                                        min = 25,
                                        max = 100,
                                        value = 100)),
                     
                     box(width = NULL, h3( "What are people's believes?"), h4("These parties have been able to thrive
                                                                              because there are many people who are unhappy and blame
                                                                              Europe and immigration for it.")),
                     
                     box(width = NULL, title = "Real ration between Citizens and Non-citizens in Europe", plotOutput("real"))
                     
                     
                     
                     
                     )
              
    
              
              
              
                     )
            
            
            
            
            
            
            
            
    )
    
    
    
    
    )
    
    
    
  ))





