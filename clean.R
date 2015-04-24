# url: http://wonder.cdc.gov/

dat <- read.table("cancer.txt", header=TRUE, sep = "\t")

head(dat)

library(dplyr)
dat = select(dat, Leading.Cancer.Sites, State, Sex, Race, 
              Age.Adjusted.Rate, Crude.Rate)

head(dat)
str(dat)

dat$Age.Adjusted.Rate <- as.numeric(as.character(dat$Age.Adjusted.Rate))
dat$Crude.Rate <- as.numeric(as.character(dat$Crude.Rate))

str(dat)

save(dat, file=".\\Data\\dat.rda")





load("all_us.rda")
head(all_us)

usMap <- select(all_us, DRAWSEQ, STATE_NAME, x_proj, y_proj)
str(usMap)
save(usMap, file=".\\Data\\usMap.rda")
