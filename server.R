library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(maps)

load("./Data/usMap.rda")
load("./Data/dfState.rda")

shinyServer(
  function(input, output) {
    getCol = reactive({
      if (input$race.var == "All Races") {
        return("YlOrRd")
      } else if (input$race.var == "Asian/Pacific Islander"){
        return("Greens")
      } else if (input$race.var == "Black"){
        return("Reds")
      } else if (input$race.var == "White"){
        return("Blues")
      } else {
        return ("Oranges")
      }
    })
    
    getSubsetPlot = reactive({
      subset = dat %>% filter(SITE == input$cancer.var & 
                                EVENT_TYPE == input$type.var &
                                STATE != "United States" &
                                SEX == input$sex.var & 
                                RACE == input$race.var & 
                                (YEAR >= input$year.var[1] & YEAR <= input$year.var[2]))
      return(subset)
    })
    
    getSubsetLine = reactive({
      subset = dat %>% filter(SITE == input$cancer.var & 
                                STATE %in% input$state.var &
                                EVENT_TYPE == input$type.var &
                                SEX == input$sex.var & 
                                RACE == input$race.var) %>%
                        select_(.dots = c(input$rate.var, "YEAR", "EVENT_TYPE", "STATE"))
      names(subset)[1] <- "RATE"
      return(subset)
    })
    
    getSubsetBar = reactive({
      subset = dat %>% filter(SITE != "All Cancer Sites Combined" & 
                                STATE %in% input$state.var &
                                EVENT_TYPE == input$type.var &
                                SEX == input$sex.var & 
                                RACE == input$race.var &
                                YEAR == input$dynamic.var) %>%
        select_(.dots = c(input$rate.var, "SITE", "STATE"))
      names(subset)[1] <- "RATE"
      return(subset)
    })
    
    getMap = reactive({
      this.col = getCol()
      
      this.subset = getSubsetPlot() %>% select_(.dots = c(input$rate.var, "STATE"))
      
      
      usMap$rate = this.subset[,1][match(usMap$STATE_NAME, this.subset$STATE)]
      
      p = ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rate)) + 
        geom_polygon(color = "black", size = 0.6) + 
        scale_fill_gradientn(colours=brewer.pal(7, this.col)) +
        theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
        labs(title = "Title")
      
      return(p)
    })
    
    getPlot = reactive({
        if (input$plot_type == "Line Graph") {
          this.subset = getSubsetLine()
          
          p = ggplot(data = this.subset, aes(x = YEAR, y = RATE, color = STATE)) + 
            geom_point(size = 3) + geom_line() +
            theme(rect = element_blank()) + 
            labs(title = "Title")
          
          return(p)
        } else {
        this.subset = getSubsetBar()
        
        p = ggplot(data = this.subset, aes(x = SITE, y = RATE, fill = SITE)) + 
          geom_bar(stat = "identity") +
          theme(rect = element_blank()) + labs(title = "Title") + facet_wrap(~ STATE)
        
        return(p)
      }
      
    })
    
      
    output$map = renderPlot({
      p = getMap()
      print(p)
    })
    
    output$downloadMap <- downloadHandler(
      filename = 'map.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = getMap(), device = device)
      })
    
    output$ui <- renderUI({
      if (is.null(input$plot_type))
        return()
      
      switch(input$plot_type,
             "Line Graph" = return(),
             "Bar Chart" = selectInput("dynamic.var", label = h3("Choose a year..."), 
                                       choices = 1999:2011, 
                                       selected = 2011),
      )
    })
    
    output$plot = renderPlot({
        p = getPlot()
        print(p)
    })
    
    output$downloadMap <- downloadHandler(
      filename = 'plot.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        ggsave(file, plot = getMap(), device = device)
      })
    
    getSubsetRegression = reactive({
      s1 = dat %>% filter(YEAR == 2011 & STATE != "United States" & 
                                SITE == input$regression.site &
                                EVENT_TYPE == input$regression.event) %>%
                        select_(.dots = c(input$regression.dependent, "STATE", "RACE", "SEX"))
      
      if (input$regression.factor == 1) {
        s1 = s1 %>% filter(RACE == "All Races" & SEX == "Male and Female") %>%
          select_(.dots = c(input$regression.dependent, "STATE"))
      } else if (input$regression.factor == 2) {
        s1 = s1 %>% filter(RACE != "All Races" & SEX == "Male and Female") %>%
          select_(.dots = c(input$regression.dependent, "RACE", "STATE"))
      } else {
        s1 = s1 %>% filter(RACE == "All Races" & SEX != "Male and Female") %>%
          select_(.dots = c(input$regression.dependent, "SEX", "STATE"))
      }
      
      s2 = dfState %>% select_(.dots = c(input$regression.independent, "STATE"))
      
      s <- merge(s1, s2, by = "STATE")
      return(s)
      
    })
    
    output$regressionPlot = renderPlot({
      
      reg.subset <- getSubsetRegression()
      
      if (input$regression.factor == 1) {
        p = ggplot(data = reg.subset, aes_string(x = input$regression.independent, y = input$regression.dependent)) +
          stat_smooth(method = "lm") + geom_point() 
      } else if (input$regression.factor == 2) {
        p = ggplot(data = reg.subset, aes_string(x = input$regression.independent, y = input$regression.dependent, color="RACE")) +
          stat_smooth(method = "lm") + geom_point() 
      } else {
        p = ggplot(data = reg.subset, aes_string(x = input$regression.independent, y = input$regression.dependent, color="SEX")) +
          stat_smooth(method = "lm") + geom_point() 
      }
       
       print(p)
      
    })
    
    output$table = renderDataTable({
      getSubsetPlot()
    })
    
    userData <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header,
               sep = input$sep)
    })
    
    output$contents = renderDataTable({
      userData()
    })
    
    output$dataText = renderUI({
      str1 = paste("Size of Dataset: ", format(object.size(userData()), units = "Kb"))
      str2 = paste("Number of Observations: ", dim(userData())[1])
      str3 = paste("Number of Features: ", dim(userData())[2])
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
    output$summary = renderTable({
      summary(userData())
    })
    
  }
)
