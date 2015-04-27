library(dplyr)
library(RColorBrewer)
library(ggplot2)

load("./Data/dat.rda")

subset = dat %>% filter(SITE == "Melanomas of the Skin" & SEX == "Male and Female" & 
                          RACE == "All Races" & (YEAR >= 1999 & YEAR <= 2011) & 
                          STATE != "United States") %>%
                select(CRUDE_RATE, STATE)
        
load("./Data/usMap.rda")

usMap$rate = subset[,1][match(usMap$STATE_NAME, subset$STATE)]

ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rate)) + 
  geom_polygon(color = "black",  color = "gray40", size = 0.6) + 
  scale_fill_gradientn(colours=brewer.pal(7,"YlOrRd")) +
  theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
  labs(title = "Title", fill=NULL)




subset = dat %>% filter(SITE == "All Cancer Sites Combined" & 
                          STATE %in% c("New Jersey", "New York", "United States") &
                          SEX == "Male" & 
                          RACE == "White") %>%
                  select_(.dots = c("CRUDE_RATE", "YEAR", "STATE", "EVENT_TYPE"))


ggplot(data = subset, aes(x = YEAR, y = CRUDE_RATE, color = interaction(EVENT_TYPE, STATE))) + 
  geom_point(size = 3) + geom_line() +
  theme(rect = element_blank()) + labs(title = "Title")

library(rCharts)
rPlot(COUNT ~ YEAR | STATE, color = 'EVENT_TYPE', type = 'line', data = subset)

