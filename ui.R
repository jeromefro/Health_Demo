library(shiny)

cancer.choices = as.character(levels(dat$SITE))
race.choices = as.character(levels(dat$RACE))
type.choices = as.character(levels(dat$EVENT_TYPE))
sex.choices = as.character(levels(dat$SEX))
rate.choices = c("COUNT", "CRUDE_RATE", "AGE_ADJUSTED_RATE")
names(rate.choices) <- c("Total Count", "Crude Rate", "Age Adjusted Rate")
state.choices = as.character(levels(dat$STATE))

shinyUI(navbarPage("Booz | Allen | Hamilton",
  tabPanel("State",
    titlePanel("Visualizing Cancer Data"),
    
    fluidRow(
      column(3,
        wellPanel(
          h3("Cancer Rates by State"),
          br(),
          
          selectInput("cancer.var", 
                      label = "Choose a type of cancer...",
                      choices = cancer.choices,
                      selected = cancer.choices[1]),
          
          selectInput("race.var", 
                      label = "Choose a race...",
                      choices = race.choices,
                      selected = race.choices[1]),	
          
          selectInput("sex.var", 
                       label = "Choose a gender...",
                       choices = sex.choices,
                       selected = sex.choices[1]),
          
          selectInput("rate.var", 
                       label = "Choose a rate...",
                       choices = rate.choices,
                       selected = rate.choices[1]),
          
          radioButtons("type.var", 
                       label = "Choose an event type...",
                       choices = type.choices,
                       selected = type.choices[1]),
          
          sliderInput("year.var", "Choose which years..", 1999, 2011, value = c(1999, 2011), sep =""),
          
          br(),
          helpText("Note: The data comes from the ", a("CDC", href="http://wonder.cdc.gov/", target="_blank"),
                   " and spans the years 1999 through 2011. Furthermore, the rates cited are per 10,000 people.")
      ),
      
        wellPanel(
          h3("Download Map of USA"),
          downloadButton('downloadMap', 'Download')
        ),      
      
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
        ),
      
        wellPanel(
          h3("Download Plot"),
          downloadButton('downloadPlot', 'Download')
        )
      ),
    
      column(9, 
          tabsetPanel(
            tabPanel("Map", plotOutput("map", height="600px", width="1000px"), 
                   plotOutput("plot")),
            tabPanel("Table", dataTableOutput("table"))
        )
      )
    )
  ),
  
  tabPanel("County"),
  
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



