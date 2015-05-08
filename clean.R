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

# death measures are age-adjusted death rates per 100,000 population

df <- read.csv("./chsi_dataset/MEASURESOFBIRTHANDDEATH.csv")
head(df)
str(df)
library(dplyr)

cancerCols <- grepl("Cancer", names(df))
names(df)[1:8]
head(df[,1:8])
cancerCols[1:6] <- TRUE
df <- df[, cancerCols]

names(df)
mins <- grepl("Min", names(df))
maxs <- grepl("Max", names(df))
inds <- grepl("Ind", names(df))
keep <- !mins & !maxs & ! inds
df <- df[, keep]

names(df)
summary(df)
mean(df$Brst_Cancer <= 0)
mean(df$Col_Cancer <= 0)
mean(df$Lung_Cancer <= 0)

df$Brst_Cancer[df$Brst_Cancer < 0] <- NA
df$Col_Cancer[df$Col_Cancer < 0] <- NA
df$Lung_Cancer[df$Lung_Cancer < 0] <- NA

summary(df)
df$Total_Cancer <- rowSums(df[,7:9], na.rm=FALSE)

df.2 <- read.csv("./chsi_dataset/DEMOGRAPHICS.csv") 
names(df.2)
keep <- c("State_FIPS_Code", "County_FIPS_Code", "CHSI_County_Name",
          "CHSI_State_Name", "CHSI_State_Abbr", "Strata_ID_Number",
          "Population_Size", "Population_Density", "Poverty", "Age_19_Under",
          "Age_19_64", "Age_65_84", "Age_85_and_Over", "White", "Black",
          "Asian", "Hispanic")
df.2 <- df.2[,names(df.2) %in% keep]
names(df.2)

summary(df.2)
mean(df.2$Population_Density <= 0)
mean(df.2$Poverty <= 0)

df.2$Population_Density[df.2$Population_Density <= 0] <- NA
df.2$Poverty[df.2$Poverty <= 0] <- NA

summary(df.2)

df.3 <- read.csv("./chsi_dataset/RISKFACTORSANDACCESSTOCARE.csv")
str(df.3)
names(df.3)

keep <- c("State_FIPS_Code", "County_FIPS_Code", "CHSI_County_Name",
          "CHSI_State_Name", "CHSI_State_Abbr", "Strata_ID_Number",
          "No_Exercise", "Few_Fruit_Veg", "Obesity", "Smoker", 
          "Prim_Care_Phys_Rate")

df.3 <- df.3[, names(df.3) %in% keep]
names(df.3)
summary(df.3)

df.3$No_Exercise[df.3$No_Exercise <= 0] <- NA
df.3$Obesity[df.3$Obesity <= 0] <- NA
df.3$Few_Fruit_Veg[df.3$Few_Fruit_Veg <= 0] <- NA
df.3$Smoker[df.3$Smoker <= 0] <- NA
summary(df.3)

chsi_Data <- merge(df, df.2)
chsi_Data <- merge(chsi_Data, df.3)

str(chsi_Data)
summary(chsi_Data)

chsi_Data$State_FIPS_Code <- as.character(chsi_Data$State_FIPS_Code)
chsi_Data$County_FIPS_Code <- as.character(chsi_Data$County_FIPS_Code)
str(chsi_Data)
chsi_Data$StateCounty_FIPS <- paste(chsi_Data$State_FIPS_Code, chsi_Data$County_FIPS_Code, sep = "_")

chsi_Data$CHSI_County_Name <- as.character(chsi_Data$CHSI_County_Name)
chsi_Data$CHSI_County_Name <- tolower(chsi_Data$CHSI_County_Name)
chsi_Data$CHSI_State_Name <- as.character(chsi_Data$CHSI_State_Name)
chsi_Data$CHSI_State_Name <- tolower(chsi_Data$CHSI_State_Name)
str(chsi_Data)
chsi_Data$StateCounty_Name <- paste(chsi_Data$CHSI_County_Name, chsi_Data$CHSI_State_Name, sep=",")

str(chsi_Data)

save(chsi_Data, file="./Data/chsi_Data.rda")




library(dplyr)
df1 <- read.csv("~/Documents/stateIncome.csv")
head(df1)
tail(df1)
str(df1)

df1 <- df1 %>% filter(year == 2012)
df1 <- df1[,c("description", "mh_inc")]
df1
names(df1)
df1

df2 <- read.csv("~/Documents/stateEducation.csv")
head(df2)
df2 <- df2[,c(2,4,13,14)]
head(df2)
str(df2)
tail(df2)
df2 <- df2 %>% filter(year == 2012)
df2
names(df2)

dfState <- merge(df1, df2, by = "description")
head(dfState)
dfState <- dfState[,c(1,2,4,5)]
names(dfState)[1] <- "STATE"
names(dfState)
names(dfState)[2:4] <- c("Median_Income", "HS_Diploma", "BA_Degree")
names(dfState)

save(dfState, file = "./Data/dfState.rda")
