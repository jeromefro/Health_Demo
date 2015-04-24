library(dplyr)
library(RColorBrewer)
library(ggplot2)

subset = select(dat, 
                Crude.Rate, Leading.Cancer.Sites, Sex, State, Race)
subset = filter(subset, 
                Leading.Cancer.Sites == "Urinary Bladder, invasive and in situ" & 
                  Sex == "Male" &
                  Race == "Black or African American")

load("usMap.rda")

usMap$rate = subset[,1][match(usMap$STATE_NAME, subset$State)]

ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rate)) + 
  geom_polygon(color = "black",  color = "gray40", size = 0.6) + 
  scale_fill_gradientn(colours=brewer.pal(7,"Reds")) +
  theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
  labs(title = "Title", fill=NULL)
