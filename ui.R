library(shiny)
library(shinythemes)

cancer.choices = as.character(levels(dat$SITE))
race.choices = as.character(levels(dat$RACE))
type.choices = as.character(levels(dat$EVENT_TYPE))
sex.choices = as.character(levels(dat$SEX))
rate.choices = c("COUNT", "CRUDE_RATE", "AGE_ADJUSTED_RATE")
names(rate.choices) <- c("Total Count", "Crude Rate", "Age Adjusted Rate")
state.choices = as.character(levels(dat$STATE))

shinyUI(navbarPage("Booz | Allen | Hamilton", theme = shinytheme("cosmo"),
  tabPanel("Analysis",
    titlePanel("Visualizing Cancer Data"),
    
    h4("Choose a Dataset"),
    
    navlistPanel(widths = c(2,10), tabPanel(("State"), fluidRow(
      column(12,  
        tabsetPanel(
            tabPanel("Map",
                     fluidPage(
                       fluidRow(
                       br(),
                       column(3, 
                            wellPanel(
                              selectInput("cancer.var", 
                                          label = "Choose a type of cancer...",
                                          choices = cancer.choices,
                                          selected = cancer.choices[1]
                              ),
                              selectInput("race.var", 
                                          label = "Choose a race...",
                                          choices = race.choices,
                                          selected = race.choices[1])
                              )
                      ),
                            
                      column(3,
                              wellPanel(  
                              selectInput("sex.var", 
                                          label = "Choose a gender...",
                                          choices = sex.choices,
                                          selected = sex.choices[1]),
                              
                              selectInput("rate.var", 
                                          label = "Choose a rate...",
                                          choices = rate.choices,
                                          selected = rate.choices[1])
                            )
                     ),

                     column(3, 
                            wellPanel(
                              radioButtons("type.var", 
                                           label = "Choose an event type...",
                                           choices = type.choices,
                                           selected = type.choices[1])
                            ) 
                     ),
                     column(3, 
                            wellPanel(
                              sliderInput("year.var", "Choose which years..", 1999, 2011, value = c(1999, 2011), sep ="")
                            ) 
                     )
                    ),
                      
                   fluidRow(plotOutput("map",  height="600px", width="1000px")),
                   fluidRow(
                     column(3,
                            wellPanel(
                              h3("Plots of Data"),
                              
                              selectInput("plot_type", "Choose a type of plot...",
                                          c("Line Graph", "Bar Chart")
                              ),
                              
                              selectizeInput("state.var", 
                                             label = "Choose a state...",
                                             choices = state.choices,
                                             selected = state.choices[1],
                                             multiple = TRUE),
                              
                              uiOutput("ui")
                            )
                      ),
                     
                     column(9, 
                            plotOutput("plot")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(6,
                       wellPanel(
                         h3("Download Map of USA"),
                         downloadButton('downloadMap', 'Download')
                       )
                     ),
                      column(6, 
                       wellPanel(
                         h3("Download Plot"),
                         downloadButton('downloadPlot', 'Download')
                       )
                     ) 
                   )
                )
            ),
            
            tabPanel("Regression", 
              fluidPage(
                fluidRow(
                  br(),
                  column(4,
                         wellPanel(
                           radioButtons("regression.event", label = "Choose Event Type...", 
                                        choices = c("Incidence", "Mortality"),
                                        selected = "Incidence"),
                           
                           br(),
                           
                           selectInput("regression.site", 
                                       label = "Choose a type of cancer...",
                                       choices = cancer.choices,
                                       selected = cancer.choices[1]
                           ),
                           
                           br(),
                           
                           radioButtons("regression.dependent", label = "Choose Dependent Variable...", 
                                              choices = rate.choices,
                                              selected = rate.choices[3])  
                         )
                  ), 
                  column(4,
                    wellPanel(
                      radioButtons("regression.factor", label = "Choose Independent Factor Variables...", 
                                         choices = list("None" = 1, "Race" = 2, "Sex" = 3),
                                         selected = 1)  
                    )
                  ),
                  column(4,
                         wellPanel(
                           radioButtons("regression.independent", label = "Choose Independent Continuous Variables...", 
                                              choices = list("Median Income" = "Median_Income",
                                                             "Percentage of Adults with HS Diploma" = "HS_Diploma",
                                                             "Percentage of Adults with Bachelors Degree" = "BA_Degree"),
                                              selected = "Median_Income")  
                         )
                  ) 
                ),
                plotOutput("regressionPlot")
              )         
            ),
            tabPanel("Table",
              br(), 
              dataTableOutput("table")
            )
          )
        )
    )),
    
    tabPanel("County"
             
    )
    
    )
  ),
  
  tabPanel("Upload", 
           shinyUI(fluidPage(
             fluidRow(
                 column(3,
                  wellPanel(      
                   fileInput('file1', 'Choose file to upload',
                             accept = c(
                               'text/csv',
                               'text/comma-separated-values',
                               'text/tab-separated-values',
                               'text/plain',
                               '.csv',
                               '.tsv'
                             )
                   ),
                   tags$hr(),
                   checkboxInput('header', 'Header', TRUE),
                   radioButtons('sep', 'Separator',
                                c(Comma=',',
                                  Semicolon=';',
                                  Tab='\t'),
                                ',')
                 )
                ),
               
               column(9, 
                  tabsetPanel(type = "tabs", 
                              tabPanel("Overview",
                                fluidRow(
                                  column(3,
                                    wellPanel(htmlOutput("dataText"))
                                  ),
                                  column(9,
                                    dataTableOutput('contents')
                                  )
                                )
                              ),
                              tabPanel("Summary", fluidRow(
                                column(12,
                                       tableOutput("summary"))
                                )),
                              tabPanel("Pre-Processing", 
                                       sidebarLayout(
                                         sidebarPanel(
                                           h6("Controls for pre-processing")
                                         ),                                       
                                         mainPanel(
                                           
                                         )
                                       )
                              ),
                              tabPanel("Explore"), 
                              tabPanel("Predict")
                  )    
               )
             )
           ))       
  ),
  
  navbarMenu("More", 
    tabPanel("About"),
    tabPanel("Info")
  )
  
))



