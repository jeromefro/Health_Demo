library(shiny)

cancer.choices = as.character(levels(dat$Leading.Cancer.Sites))
race.choices = as.character(levels(dat$Race))
sex.choices = as.character(levels(dat$Sex))
rate.choices = c("Age.Adjusted.Rate", "Crude.Rate")

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
      
      radioButtons("race.var", 
                  label = "Choose a race...",
                  choices = race.choices,
                  selected = race.choices[3]),	
      
      radioButtons("sex.var", 
                   label = "Choose a gender...",
                   choices = sex.choices,
                   selected = sex.choices[2]),
      
      radioButtons("rate.var", 
                   label = "Choose a rate...",
                   choices = rate.choices,
                   selected = rate.choices[2])
      
    ),
    
    mainPanel(tabsetPanel(
      tabPanel("Map", plotOutput("map", height="400px", width="900px")), 
      tabPanel("Table", dataTableOutput("table"))
    ))
    
  )
))