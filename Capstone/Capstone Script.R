
# Import Library ----------------------------------------------------------

library(readxl)
winemag_data_130k_v2 <- read_excel("winemag-data-130k-v2.xlsx")
View(winemag_data_130k_v2)
Wine_type <- read_excel("Wine type.xlsx")
Countrykey <- read_excel("Country key.xlsx")

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(stringr)
library(crunch)

# Data Exploration ---------------------------------------------------------- 
WineList <- winemag_data_130k_v2
WineList %>% summary(price)

# Putting missing price as median price ----------------------------------------------------------no double quotes
med_price <- median(WineList$price)
WineList <- WineList %>% mutate(price = ifelse(is.na(price), 25, price))

Regct <- count(WineList, vars = region, points)

WineList <- WineList %>% mutate('price' = as.numeric(price))


# Data Engineering ---------------------------------------------------------- 
WineList <- left_join(WineList, Wine_type, by = "variety")
WineList <- left_join(WineList, Country_Key, by = "country")

WineList <- WineList %>% mutate(varietal = word(WineList$variety,1))

WineList <- WineList %>% mutate(WesternEurope = ifelse(region == "Western Europe", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(NorthAmerica = ifelse(region == "North America", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(SouthAmerica = ifelse(region == "South America", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(Africa = ifelse(region == "Africa", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(MiddleEast = ifelse(region == "Middle East", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(SouthEEurope = ifelse(region == "South Eastern Europe", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(EasternEurope = ifelse(region == "Eastern Europe", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(SouthPacific = ifelse(region == "South Pacfic", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(CentralAsia = ifelse(region == "Asia", 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(NotA = ifelse(is.na(region) , 1, ifelse(is.na(Type), 0, 0)))

WineList <- WineList %>% mutate(White = ifelse(Type == "White" , 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(Red = ifelse(Type == "Red" , 1, ifelse(is.na(Type),0,0)))

WineList <- WineList %>% mutate("Mpoints" =  (points - mean(points)))
WineList <- WineList %>% mutate("SDpoints" =  (points - mean(points)) /sd(points))

WineList <- WineList %>% mutate("Mprice" =  (price - mean(price)))
WineList <- WineList %>% mutate("SDprice" =  (price - mean(price)) /sd(price))


# Corrplot ---------------------------------------------------------- 
M <- cor(WLR, use = "pairwise.or.complete")
corrplot(M, method = "number", type = "upper", col = "black")


# Plots ---------------------------------------------------------- 
# Price and Points by region
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + geom_point(aes(color=region), 
    size = 1) + geom_smooth(aes(colour=region), method="lm") + 
    scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

ggplot(WineList, aes(x=price, y=SDpoints)) + geom_point(aes(color=region),
    size = 1) + geom_smooth(aes(colour=region), method="lm") + 
    scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

ggplot(WineList, aes(x=SDpoints)) + geom_density(aes(fill=region),
                                                 alpha = 0.4)

ggplot(WineList, aes(x=Mprice, y=Mpoints)) + geom_point(aes(color=Type), 
    size = 1) + geom_smooth(aes(colour=Type), method="lm") + 
    scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

ggplot(WineList, aes(x=SDpoints, y=SDprice)) + geom_point(aes(colour=varietal), 
    size=2) + geom_smooth(aes(colour=varietal), se=FALSE, method="lm") + 
    scale_y_continuous(trans="log1p")


# Western Europe
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
    geom_point(aes(color=Type), size = 1, 
               data = subset(WineList, region %in% c("Western Europe"))) +
    geom_smooth(aes(colour=Type), method="lm", 
                data = subset(WineList, region %in% c("Western Europe")))
    + scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# North America
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("North America"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("North America")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# South America
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("South America"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("South America")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# Africa
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("Africa"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("Africa")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# Asia
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("Asia"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("Asia")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")


# Middle East
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("Middle East"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("Middle East")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# Eastern Europe
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("Eastern Europe"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("Eastern Europe")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# South Pacific
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("South Pacific"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("South Pacific")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")

# South Eastern Europe
ggplot(WineList, aes(x=Mprice, y=Mpoints)) + 
  geom_point(aes(color=Type), size = 1, 
             data = subset(WineList, region %in% c("South Eastern Europe"))) +
  geom_smooth(aes(colour=Type), method="lm", 
              data = subset(WineList, region %in% c("South Eastern Europe")))
+ scale_y_continuous(trans = "log1p") + scale_x_continuous(trans="log1p")
