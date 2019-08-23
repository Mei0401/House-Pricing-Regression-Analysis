# package preperation
install.packages("tidyverse")
install.packages("rgdal")
install.packages("dplyr")
install.packages("corrplot")
library(tidyverse)
library(rgdal)
library(dplyr)
library(corrplot)
library(car)
options(scipen=999)


# Data Munging part

# save a polygon variable for a later spatial auto-correlation check
hamilton_polygon <- readOGR("https://raw.githubusercontent.com/gisUTM/GGR376/master/Lab_1/houseValues.geojson")

# get data part of geo-table
price_data <- as.data.frame(hamilton_polygon)

# get attribute table and change 1st column to CTUID
attributes <- read.csv("F:\\a1\\attributes.csv")
attributes <- mutate(attributes, CTUID = COL0)

# combine them to a same file
hamilton_data <- inner_join(price_data, attributes, by = "CTUID")

# remove N/A values
hamilton_data <- na.omit(hamilton_data)

# data managment
hamilton_data <- hamilton_data%>%
  mutate(CTUID = COL0)%>%
  mutate(pMarried = COL4/COL3)%>%
  mutate(pCanadianCitizen = COL7/COL6)%>%
  mutate(pCondo = COL9/COL8)%>%
  mutate(pOwner = COL11/COL10)%>%
  mutate(pUniversity = COL13/COL12)%>%
  mutate(pTimeToWork = (COL17+COL16)/COL15)

# chained the mutations
model_data <- hamilton_data%>%
  select(CTUID, houseValue, pPopulation = COL1, nAverageAge = COL2, pMarried, pCanadianCitizen, pCondo,
         pOwner, pUniversity, pTimeToWork, pMedianIncome = COL5 , pEmployment = COL14 )

# Graphical Analysis Pre-Check

# price check
ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = houseValue))+
  scale_x_log10() # log for a better distribution

# population check
ggplot(data = model_data, mapping = aes(x = pPopulation, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pPopulation)) # 6 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pPopulation))

# average age check
ggplot(data = model_data, mapping = aes(x = nAverageAge, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = nAverageAge)) # 4 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = nAverageAge)) # well distributed

# Married status check
ggplot(data = model_data, mapping = aes(x = pMarried, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F) # pretty strong linear related

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pMarried)) # 1 outlier

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pMarried))  # well distributed

# Canadian Citizen check
ggplot(data = model_data, mapping = aes(x = pCanadianCitizen, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pCanadianCitizen)) # 15 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pCanadianCitizen))

# Condo check
ggplot(data = model_data, mapping = aes(x = pCondo, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pCondo)) # 8 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pCondo))+
  scale_x_log10() # log for a better distribution

# Owner check
ggplot(data = model_data, mapping = aes(x = pOwner, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F) # seems good distributed

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pOwner))

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pOwner))

# University check
ggplot(data = model_data, mapping = aes(x = pUniversity, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F) # nice linear related

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pUniversity)) # 2 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pUniversity))

# Time to work check
ggplot(data = model_data, mapping = aes(x = pTimeToWork, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pTimeToWork)) # 6 outliers

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pTimeToWork)) # good normal distributed

# Median of income check
ggplot(data = model_data, mapping = aes(x = pMedianIncome, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F) # good linear related

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pMedianIncome))

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pMedianIncome)) # good normal distributed

# employment rate check
ggplot(data = model_data, mapping = aes(x = pEmployment, y = houseValue))+
  geom_point()+
  geom_smooth(method = "lm", se = F) # good linea related

ggplot(data = model_data)+
  geom_boxplot( mapping = aes(x = "", y = pEmployment)) # 1 outlier

ggplot(data = model_data)+
  geom_histogram(mapping = aes(x = pEmployment))+
  scale_x_log10() # log for better distributed, nice distribution

# Data transformations
trans_model_data <- model_data%>%
  mutate(logCondo = log(pCondo+1))%>%
  mutate(logEmploy = log(pEmployment+1))%>%
  mutate(logHouseValue = log(houseValue+1))%>%
  select(-pCondo,-pEmployment,-houseValue) # using log to replace original attributes
  
# Normality test
trans_model_data%>%
  select(-CTUID)%>%
  map(shapiro.test)

# Correlation test
cor_mat <- cor(trans_model_data%>%
                 select(-CTUID))
corrplot::corrplot(cor_mat, cl.pos = "b", tl.pos = "d")


# Model Fitting and model assumption assessment

# Start from the atrribute that has stronger relationship
model_1 <- lm(logHouseValue ~ pMedianIncome, data = trans_model_data)
summary(model_1)
mean(model_1$residuals) # fine
par(mfrow=c(2,2))
plot(model_1) # plot is flat, so no change on outliers

# Add pUniversity to model
model_2 <- lm(logHouseValue ~ pMedianIncome + pUniversity, data = trans_model_data)
summary(model_2) # improved
mean(model_2$residuals) # fine
par(mfrow=c(2,2))
plot(model_2) # plot is flat, so no change on outliers

# Add pMarried to model
model_3 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried, data = trans_model_data)
summary(model_3) # improved
mean(model_3$residuals) # fine
par(mfrow=c(2,2))
plot(model_3) # plot is flat, so no change on outliers

# Add logEmploy to model
model_4 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + logEmploy
                , data = trans_model_data)
summary(model_4) # not improved

# Add pOwner to model
model_5 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
              , data = trans_model_data)
summary(model_5) # improved
mean(model_5$residuals) # fine
par(mfrow=c(2,2))
plot(model_5) # plot is flat, so no change on outliers

# Add pPopulation to model
model_6 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
             + pPopulation , data = trans_model_data)
summary(model_6) # improved
mean(model_6$residuals) # fine
par(mfrow=c(2,2))
plot(model_6) # plot is flat, so no change on outliers

# Add logCondo to model
model_7 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
              + pPopulation + logCondo , data = trans_model_data)
summary(model_7) # new variable not sifgnificant, remove

# Add nAverageAge to model
model_8 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
              + pPopulation + nAverageAge , data = trans_model_data)
summary(model_8) # improved
mean(model_8$residuals) # fine
par(mfrow=c(2,2))
plot(model_8) # plot is flat, so no change on outliers

# Add pCanadianCitizen to model
model_9 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
              + pPopulation + nAverageAge + pCanadianCitizen, data = trans_model_data)
summary(model_9) # not improved

# Add pTimeToWork to model
model_10 <- lm(logHouseValue ~ pMedianIncome + pUniversity + pMarried + pOwner
              + pPopulation + nAverageAge + pTimeToWork , data = trans_model_data)
summary(model_10) # not improved

# for now best model is model_8
vif(model_8)

# remove 2 largest Multicollinearity variables
model_11 <- lm(logHouseValue ~  pUniversity + pOwner
               + pPopulation + nAverageAge , data = trans_model_data)
summary(model_11)
mean(model_11$residuals) # fine
par(mfrow=c(2,2))
plot(model_11) # have outlier 79,80

# remove outlier 79,80
trans_model_sub <- trans_model_data%>%
  slice(-c(79,80))

model_12 <- lm(logHouseValue ~  pUniversity + pOwner
               + pPopulation + nAverageAge , data = trans_model_sub)
summary(model_12)
mean(model_11$residuals) # fine
par(mfrow=c(2,2))
plot(model_12)  

# Add pTimeToWork
model_13 <- lm(logHouseValue ~ pUniversity  + pOwner
              + pPopulation + nAverageAge + pTimeToWork, data = trans_model_sub)
summary(model_13) # improved
mean(model_13$residuals) # fine
par(mfrow=c(2,2))
plot(model_13) # fine
vif(model_13) # fine







   
 