library(shiny)

cancer.choices = as.character(levels(dat$SITE))
race.choices = as.character(levels(dat$RACE))
type.choices = as.character(levels(dat$EVENT_TYPE))
sex.choices = as.character(levels(dat$SEX))
rate.choices = c("COUNT", "CRUDE_RATE", "AGE_ADJUSTED_RATE")
state.choices = as.character(levels(dat$STATE))

shinyUI(fluidPage(
  titlePanel("Visualizing Cancer Data"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("This application was developed to explore cancer rates at the state level."),
      br(),
      helpText("The data comes from the ", a("CDC", href="http://wonder.cdc.gov/", target="_blank"),
               " and spans the years 1999 through 2011. Furthermore, the rates cited are per 10,000 people."),
      br(),
      
      selectInput("cancer.var", 
                  label = "Choose a type of cancer...",
                  choices = cancer.choices,
                  selected = cancer.choices[1]),
      
      sliderInput("year.var", "Year released", 1999, 2011, value = c(1999, 2011)),
      
      radioButtons("race.var", 
                  label = "Choose a race...",
                  choices = race.choices,
                  selected = race.choices[1]),	
      
      radioButtons("sex.var", 
                   label = "Choose a gender...",
                   choices = sex.choices,
                   selected = sex.choices[1]),
      
      radioButtons("type.var", 
                   label = "Choose an event type...",
                   choices = type.choices,
                   selected = type.choices[1]),
      
      radioButtons("rate.var", 
                   label = "Choose a rate...",
                   choices = rate.choices,
                   selected = rate.choices[1]),
      
      selectizeInput("state.var", 
                  label = "Choose a state...",
                  choices = state.choices,
                  multiple = TRUE)
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Map", plotOutput("map", height="400px", width="900px"), 
               plotOutput("line")),
      tabPanel("Table", dataTableOutput("table"))
    ))
    
  )
))