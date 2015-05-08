library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(maps)
library(rgdal)

load("./Data/dat.rda")

subset = dat %>% filter(SITE == "Melanomas of the Skin" & SEX == "Male and Female" & 
                          RACE == "All Races" & (YEAR >= 1999 & YEAR <= 2011) & 
                          STATE != "United States") %>%
                select(CRUDE_RATE, STATE)
        
load("./Data/usMap.rda")

usMap$rate = subset[,1][match(usMap$STATE_NAME, subset$STATE)]

ggplot(data = usMap, aes(x = x_proj, y = y_proj, group = DRAWSEQ, fill = rate)) + 
  geom_polygon(color = "gray40", size = 0.6) + 
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







subset2 = dat %>% filter(STATE %in% c("New Jersey", "New York") &
                           SITE != "All Cancer Sites Combined" &
                           EVENT_TYPE == "Incidence" &
                           SEX %in% c("Male") & 
                           YEAR == 2011 &
                           RACE == "White") %>%
  select_(.dots = c("CRUDE_RATE", "STATE", "SITE"))

p <- ggplot(data = subset2, aes(x = SITE, y = CRUDE_RATE, fill = SITE)) + 
  geom_bar(stat = "identity") +
  theme(rect = element_blank()) + labs(title = "Title") + facet_wrap(~ STATE)

print(p)

ggsave(filename = "test.png", plot = p)

library(rCharts)
rPlot(COUNT ~ YEAR | STATE, color = 'EVENT_TYPE', type = 'line', data = subset)





load("./Data/chsi_Data.rda")
county_map <- map_data("county")
head(county_map)
county_map$StateCounty_Name <- paste(county_map$subregion, county_map$region, sep = ",")
head(county_map)

county_map$rate = chsi_Data[,24][match(county_map$StateCounty_Name, chsi_Data$StateCounty_Name)]

state_map <- map_data("state")

ggplot(data = county_map, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = rate), color = "black", size = 0.1) + 
  scale_fill_gradientn(colours=brewer.pal(9,"YlOrRd")) +
  theme(axis.line = element_blank(), panel.grid=element_blank(), rect = element_blank(), axis.title=element_blank(), axis.ticks=element_blank(), axis.text=element_blank()) +
  labs(title = "Title", fill=NULL)


geom_path( data = state_map , colour = "black", size = 0.3)















US.counties <- readOGR(dsn=".",layer="gz_2010_us_050_00_5m")
county.data <- US.counties@data
head(county.data)
str(county.data)
county.data$STATE <- as.character(as.numeric(as.character(county.data$STATE)))
county.data$COUNTY <- as.character(as.numeric(as.character(county.data$COUNTY)))
str(county.data)

county.data$StateCounty_FIPS <- paste(county.data$STATE, county.data$COUNTY, sep = "_")
str(county.data)

map.df <- fortify(US.counties)









load("./Data/dat.rda")
load("./Data/dfState.rda")
library(dplyr)

subset = dat %>% filter(YEAR == 2011 & STATE != "United States" & 
                           SITE == "All Cancer Sites Combined" &
                           EVENT_TYPE == "Mortality" & 
                           RACE == "All Races" & SEX != "Male and Female") %>%
                  select_(.dots = c("AGE_ADJUSTED_RATE", "STATE", "SITE", 
                                    "RACE", "SEX", "EVENT_TYPE"))

str(subset)

test = merge(subset, dfState, by = "STATE")
head(test)

formula = "AGE_ADJUSTED_RATE ~ Median_Income + R"

c <- ggplot(mtcars, aes(y=wt, x="mpg", colour=factor(cyl)))

summary(fit)

head(test)
ggplot(data = test, aes(x = Median_Income, y = AGE_ADJUSTED_RATE, color=factor(test[,5]))) +
  stat_smooth(method = "lm") + geom_point()





subset = dat %>% filter(YEAR == 2011 & STATE != "United States" & 
                          SITE == "All Cancer Sites Combined" &
                          EVENT_TYPE == "Mortality") %>%
  select_(.dots = c("AGE_ADJUSTED_RATE", "STATE", "RACE", "SEX"))

subset = subset %>% filter(RACE == "All Races" & SEX == "Male and Female") %>%
    select_(.dots = c("AGE_ADJUSTED_RATE", "STATE"))

subset = subset %>% filter(RACE != "All Races" & SEX == "Male and Female") %>%
    select_(.dots = c("AGE_ADJUSTED_RATE", "RACE", "STATE"))

subset2 = dfState %>% select_(.dots = c("Median_Income", "STATE"))

te <- merge(subset, subset2, by = "STATE")





regression.event = "Incidence"
regression.site = "All Cancer Sites Combined"
regression.dependent = "AGE_ADJUSTED_RATE"  
regression.factor = 2
regression.independent = "Median_Income"

s1 = dat %>% filter(YEAR == 2011 & STATE != "United States" & 
                      SITE == regression.site &
                      EVENT_TYPE == regression.event) %>%
  select_(.dots = c(regression.dependent, "STATE", "RACE", "SEX"))

if (regression.factor == 1) {
  s1 = s1 %>% filter(RACE == "All Races" & SEX == "Male and Female") %>%
    select_(.dots = c(regression.dependent, "STATE"))
} else if (regression.factor == 2) {
  s1 = s1 %>% filter(RACE != "All Races" & SEX == "Male and Female") %>%
    select_(.dots = c(regression.dependent, "RACE", "STATE"))
} else {
  s1 = s1 %>% filter(RACE == "All Races" & SEX != "Male and Female") %>%
    select_(.dots = c(regression.dependent, "SEX", "STATE"))
}

s2 = dfState %>% select_(.dots = c(regression.independent, "STATE"))

reg.subset <- merge(s1, s2, by = "STATE")

if (regression.factor == 1) {
  p = ggplot(data = reg.subset, aes_string(x = regression.independent, y = regression.dependent)) +
    stat_smooth(method = "lm") + geom_point() 
} else {
  p = ggplot(data = reg.subset, aes_string(x = regression.independent, y = regression.dependent, color="RACE")) +
    stat_smooth(method = "lm") + geom_point() 
}

print(p)
