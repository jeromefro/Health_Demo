library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

load("./Data/usMap.rda")

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
                                SEX == input$sex.var & 
                                RACE == input$race.var) %>%
                        select_(.dots = c(input$rate.var, "YEAR", "EVENT_TYPE", "STATE"))
      names(subset)[1] <- "RATE"
      return(subset)
    })
    
      
    output$map = renderPlot({
      this.col = getCol()
      
      this.subset = getSubsetPlot() %>% select_(.dots = c(input$rate.var, "STATE"))
      
      
      usMap$rate = this.subset[,1][match(usMap$STATE_NAME, this.subset$STATE)]
      
      p = ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rate)) + 
        geom_polygon(color = "black",  color = "gray40", size = 0.6) + 
        scale_fill_gradientn(colours=brewer.pal(7, this.col)) +
        theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
        labs(title = "Title")
      
      print(p)
    })
    
    output$line = renderPlot({
      if (length(input$state.var) > 0) {
        this.subset = getSubsetLine()
        
        p = ggplot(data = this.subset, aes(x = YEAR, y = RATE, color = interaction(STATE, EVENT_TYPE))) + 
          geom_point(size = 3) + geom_line() +
          theme(rect = element_blank())
        
        print(p)
      }
    })
    
    output$table = renderDataTable({
      dat
    })
    
  }
)
