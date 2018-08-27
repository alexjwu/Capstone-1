
# Import Library ----------------------------------------------------------





library(readxl)
winemag_data_130k_v2 <- read_excel("winemag-data-130k-v2.xlsx")
View(winemag_data_130k_v2)
Wine_type <- read_excel("Wine type.xlsx")
Countrykey <- read_excel("Country key.xlsx")

library(dplyr)
library(tidyr)
library(ggplot2)


# Data Exploration ---------------------------------------------------------- 
DF <- winemag_data_130k_v2
DF %>% summary(price)

# Putting missing price as median price ---------------------------------------------------------- 
DF <- DF %>% mutate(`price` = ifelse(is.na(price), "25", price))

# Remove un-needed data ---------------------------------------------------------- 
DF3 <- DF[, c(1,2,5,6,7,13,14)]

WineList <- DF3

rm(DF, DF2, DF3, winemag_data_130k_v2)

WineList <- WineList %>% mutate('price' = as.numeric(price))

WineList <- WineList %>% mutate(`price_cat` = 
                                  ifelse(price >= 6 & price <= 10, "10",
                                  ifelse(price >= 11 & price <= 15, "15", 
                                  ifelse(price >= 16 & price <= 20, "20", 
                                  ifelse(price >= 21 & price <= 25, "25", 
                                  ifelse(price >= 26 & price <= 30, "30", 
                                  ifelse(price >= 1 & price <= 5, "5", 
                                  ifelse(price >= 31 & price <= 35, "35", 
                                  ifelse(price >= 36 & price <= 40, "40", 
                                  ifelse(price >= 41 & price <= 45, "45", 
                                  ifelse(price >= 46 & price <= 50, "50", 
                                  ifelse(price >= 51 & price <= 55, "55", 
                                  ifelse(price >= 56 & price <= 60, "60", 
                                  ifelse(price >= 61 & price <= 65, "65", 
                                  ifelse(price >= 66 & price <= 70, "70", 
                                  ifelse(price >= 71 & price <= 75, "75", 
                                  ifelse(price >= 76 & price <= 80, "80", 
                                  ifelse(price >= 81 & price <= 85, "85", 
                                  ifelse(price >= 86 & price <= 90, "90", 
                                  ifelse(price >= 91 & price <= 95, "95", 
                                  ifelse (price >= 96 & price <= 100, "100", "no")))))))))))))))))))))

# Data Engineering ---------------------------------------------------------- 
WineList <- left_join(WineList, Wine_type, by = "variety")

WineList <- left_join(WineList, Country_Key, by = "country")


# Plots ---------------------------------------------------------- 
ggplot(WineList, aes(points)) + geom_histogram()
ggplot(WineList, aes(price_cut)) + geom_histogram()
ggplot(WineList, aes(x=log(price))) + geom_histogram(aes(fill=country), alpha=0.40)
ggplot(WineList, aes(x = price, y = points, fill = Type)) + geom_point() + geom_smooth() +  scale_x_continuous(limits = c(5, 100)) + scale_y_continuous(limits = c(80, 100))
ggplot(WineList, aes(x=points)) + geom_density(aes(fill=region), alpha=0.40)