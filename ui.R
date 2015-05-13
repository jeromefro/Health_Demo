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
    tabPanel("Walkthrough",
      fluidPage(
        fluidRow(
          column(6,
            wellPanel(
              helpText("Over the past several years, there has accumulated a growing body of evidence that vitamin D plays a crucial role in preventing colon and other intestinal cancers. ",
                       "The connection between colon cancer and vitamin D was first conjectured several decades ago when researchers noticed significantly higher colon cancer mortality rates in the northern and northeastern United States as opposed to the southern United States. ",
                       "Since exposure to sunlight produces vitamin D the researchers hypothesized that the higher mortality rates may be associated with lower vitamin D levels. ",
                       "Using this shiny app, we can explore this connection ourselves, and also interactively examine other potential factors such as race and gender. ",
                       "For more information on the link between vitamin D and colon cancer, visit the ", 
                       a("John Hopkins Center for Colorectal Cancer.", href="http://www.hopkinscoloncancercenter.org/CMS/CMS_Page.aspx?CurrentUDV=59&CMS_Page_ID=AB041E4B-5568-46B9-8D12-BA3959A6F3F5")),
              hr(),
              helpText("In addition to the choropleth map and graphs on the Map Tab, the Regression Tab allows one to explore the linear relationships between a state's incidence and mortality rates for cancer and various factors such as median income. ",
                       "For example, while the rate of cancer incidence in a state is fairly constant over median income, there is a fairly significant negative relationship between cancer mortality rates and median income. ",
                       "Stratifying by gender one can see that the decline in mortality rates is steeper for men than women. ",
                       "In addition, when we group by race, one can see that statewide cancer mortality rates amongst white and black Americans decreases with median income, but, strangely, increases for Hispancic Americans.")
            )       
          )  
        )  
      )
    ),
    tabPanel("Info",
             fluidPage(
               fluidRow(
                 column(8,
                        wellPanel(
                          h3("What is R Shiny?"),
                          helpText("R Shiny is a package for R which facilitates the development of fully interactive web-based applications. ",
                                   "No previous knowledge of HTML, CSS, or Javascript is required. ",
                                   "Instead, code is written in R and translated into HTML, CSS, and Javacript automatically."),
                          h3("How does it work?"),
                          helpText("A shiny application requires only two files: ui.R and server.R"),
                          tags$ul("server.R contains the code for the back-end of the application, it takes the user's input and performs the necessary computations"),
                          tags$ul("ui.R contains the code for the front-end, it determines the layout of the application handles the input from the user, and displayes the output from server.R"),
                          h3("How do you deploy an application?"),
                          helpText("Applications can be deployed on one's own server or in the cloud. Shiny Server (free and open-source) and Shiny Server Pro can be used on one's own servers to deploy applications. Alternatively, applications can be deployed in the cloud, on servers hosted by R Studio, with shinyapps.io"),
                          h3("Examples"),
                          tags$ul(a("R Studio Galley", href="http://www.rstudio.com/products/shiny/shiny-user-showcase/")),
                          tags$ul(a("CDC Disease Monitor", href="https://gallery.shinyapps.io/CDCPlot/")),
                          tags$ul(a("Visualize Gene Networks", href = "http://glimmer.rstudio.com/qbrc/grn/")),
                          h3("Shiny and BAH"),
                          helpText("Members of SIG held a large conference call about a month ago to discuss R shiny.",
                                   "There is a lot of excitement about the package's potential within BAH and growing support for it, ",
                                   "including a ", a("website", href="http://shiny.bah.com/"), 
                                   " which includes tutorials and applications that have already been developed")
                        )       
                 )
               )  
             )       
    ),
    tabPanel("About",
             fluidPage(
               fluidRow(
                 column(8,
                        wellPanel(
                          h3("Data Sources"),
                          helpText("The state cancer data come from the CDC's ", 
                                   a("U.S. Cancer Statistics Incidence and Mortality Web-based Report.", href = "http://www.cdc.gov/cancer/npcr/uscs/download_data.htm", target = "_blank"),
                                   "The Center for Disease Control and Prevention (CDC) and the National Cancer Institute (NCI) ",
                                   "combined their data to create the statistics.",
                                   "In total, the report contains information on more than 1 million cases of invasive cancer that occured in the United States",
                                   " between the years 1999 and 2011."),
                          hr(),
                          helpText("The county cancer data comes from the ", 
                                   a("Community Health Status Indicators (CHSI) to Combat Obesity, Heart Disease and Cancer", href  = "http://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer", target = "_blank"),
                                   "which is produced by the CDC.",
                                   "The CHSI report contains over 200 measures for each of the 3,141 United States counties.")
                        )       
                 )
               )  
             )         
    )
  )
  
))



