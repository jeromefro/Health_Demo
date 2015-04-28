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
          h3("Variation over Time"),
          br(),
          
          selectizeInput("state.var", 
                         label = "Choose a state...",
                         choices = state.choices,
                         multiple = TRUE),
          br()
        ),
      
        wellPanel(
          h3("Download Selected Data"),
          br(),
          downloadButton('downloadData', 'Download'),  
          br()
        )
      
      ),
    
      column(9, 
          tabsetPanel(
            tabPanel("Map", plotOutput("map", height="600px", width="1000px"), 
                   plotOutput("line")),
            tabPanel("Table", dataTableOutput("table"))
        )
      )
    )
  ),
  
  tabPanel("County"),
  
  navbarMenu("More", 
    tabPanel("About"),
    tabPanel("Info")
  )
  
))



