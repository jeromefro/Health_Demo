library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

load("./Data/usMap.rda")

shinyServer(
  function(input, output) {
    getSubset = reactive({
      subset = select_(dat, 
                      .dots = c(input$rate.var, "Leading.Cancer.Sites", "Sex", "State", "Race"))
      subset = filter(subset, 
                      Leading.Cancer.Sites == input$cancer.var & 
                        Sex == input$sex.var &
                        Race == input$race.var)
      
      return(subset)
    })
    
    getCol = reactive({
      if(input$race.var == "Asian or Pacific Islander"){
        return("Greens")
      } else if(input$race.var == "Black or African American"){
        return("Reds")
      } else {
        return("Blues")
      }
    })
    
    output$map = renderPlot({
      this.subset = getSubset()
      usMap$rates = this.subset[,1][match(usMap$STATE_NAME, this.subset$State)]
      this.col = getCol()
      
      p = ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rates)) + 
        geom_polygon(color = "black",  color = "gray40", size = 0.6) + 
        scale_fill_gradientn(colours=brewer.pal(7, this.col)) +
        theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
        labs(title = "Title", fill=NULL)
      
      print(p)
    })
    
    output$table = renderDataTable({
      getSubset()
    })  
    
  }
)
