# url: http://www.cdc.gov/cancer/npcr/uscs/download_data.htm

dat <- read.table("BYAREA.txt", header=TRUE, sep = "|", na.strings = "~")

head(dat)

library(dplyr)
names(dat)
dat = select(dat, AREA, AGE_ADJUSTED_RATE, COUNT, EVENT_TYPE, 
              POPULATION, RACE, SEX, SITE, YEAR, CRUDE_RATE)

head(dat)
str(dat)

levels(dat$AREA)

dat = filter(dat, AREA %in% c(state.name, "District of Columbia", "United States (comparable to ICD-O-2)"))
dat$AREA = factor(dat$AREA)
levels(dat$AREA)

levels(dat$AREA)[levels(dat$AREA) == "United States (comparable to ICD-O-2)"] <- "United States"
levels(dat$AREA)

str(dat)
dat$AGE_ADJUSTED_RATE <- as.numeric(as.character(dat$AGE_ADJUSTED_RATE))
dat$CRUDE_RATE <- as.numeric(as.character(dat$CRUDE_RATE))
dat$COUNT <- as.numeric(as.character(dat$COUNT))

levels(dat$YEAR)
dat = filter(dat, YEAR != "2007-2011")
dat$YEAR <- as.numeric(as.character(dat$YEAR))

str(dat)

levels(dat$RACE)

str(dat)

levels(dat$SITE)
dat = filter(dat, SITE %in% c("All Cancer Sites Combined", "Colon and Rectum", "Melanomas of the Skin", "Prostate", "Lung and Bronchus", "Female Breast"))
dat$SITE = factor(dat$SITE)
levels(dat$SITE)

str(dat)

names(dat)[1] <- "STATE"
names(dat)

dat$STATE <- relevel(dat$STATE, "United States")
levels(dat$STATE)
dat$SEX <- relevel(dat$SEX, "Male and Female")
levels(dat$SEX)

save(dat, file=".\\Data\\dat.rda")





load("all_us.rda")
head(all_us)

usMap <- select(all_us, DRAWSEQ, STATE_NAME, x_proj, y_proj)
str(usMap)
save(usMap, file=".\\Data\\usMap.rda")





# url: http://catalog.data.gov/dataset/community-health-status-indicators-chsi-to-combat-obesity-heart-disease-and-cancer

df <- read.csv("./CHSI/DEMOGRAPHICS.csv")
head(df)
str(df)
library(dplyr)
