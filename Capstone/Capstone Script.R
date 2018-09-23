
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


# Data Exploration ---------------------------------------------------------- 
DF <- winemag_data_130k_v2
DF %>% summary(price)

# Putting missing price as median price ----------------------------------------------------------no double quotes
DF <- DF %>% mutate(newprice= ifelse(is.na(price), 25, price))

Regct <- WineList %>% count(WineList, vars = region)

# Remove un-needed data ---------------------------------------------------------- Use column names not indices
DF3 <- DF[, c(1,2,5,6,7,13,14)]

WineList <- DF3

rm(DF, DF2, DF3, winemag_data_130k_v2)

WineList <- WineList %>% mutate('price' = as.numeric(price))


# Data Engineering ---------------------------------------------------------- 
WineList <- left_join(WineList, Wine_type, by = "variety")

WineList <- left_join(WineList, Country_Key, by = "country")

WineList <- WineList %>% mutate(WesternEurope = ifelse(region == "Western Europe", 1, 0))
WineList <- WineList %>% mutate(NorthAmerica = ifelse(region == "North America", 1, 0))
WineList <- WineList %>% mutate(SouthAmerica = ifelse(region == "South America", 1, 0))
WineList <- WineList %>% mutate(Africa = ifelse(region == "Africa", 1, 0))
WineList <- WineList %>% mutate(India = ifelse(region == "India", 1, 0))
WineList <- WineList %>% mutate(CentralAsia = ifelse(region == "Central Asia", 1, 0))
WineList <- WineList %>% mutate(MiddleEast = ifelse(region == "Middle East", 1, 0))
WineList <- WineList %>% mutate(SouthEEurope = ifelse(region == "South Eastern Europe", 1, 0))
WineList <- WineList %>% mutate(EasternEurope = ifelse(region == "Eastern Europe", 1, 0))
WineList <- WineList %>% mutate(SouthPacific = ifelse(region == "South Pacfic", 1, 0))
WineList <- WineList %>% mutate(CentralAsia = ifelse(region == "Central Asia", 1, 0))
WineList <- WineList %>% mutate(EastAsia = ifelse(region == "East Asia", 1, 0))
WineList <- WineList %>% mutate(NotA = ifelse(is.na(region) , 1, 0))

WineList <- WineList %>% mutate(White = ifelse(Type == "White" , 1, ifelse(is.na(Type), 0, 0)))
WineList <- WineList %>% mutate(Red = ifelse(Type == "Red" , 1, ifelse(is.na(Type),0,0)))

WineList <- WineList %>% mutate("10" = ifelse(price > 0 & price <= 10, 1, 0))
WineList <- WineList %>% mutate("20" = ifelse(price > 10 & price <= 20, 1, 0))
WineList <- WineList %>% mutate("30" = ifelse(price > 20 & price <= 30, 1, 0))
WineList <- WineList %>% mutate("40" = ifelse(price > 30 & price <= 40, 1, 0))
WineList <- WineList %>% mutate("50" = ifelse(price > 40 & price <= 50, 1, 0))
WineList <- WineList %>% mutate("60" = ifelse(price > 50 & price <= 60, 1, 0))
WineList <- WineList %>% mutate("70" = ifelse(price > 60 & price <= 70, 1, 0))
WineList <- WineList %>% mutate("80" = ifelse(price > 70 & price <= 80, 1, 0))
WineList <- WineList %>% mutate("90" = ifelse(price > 80 & price <= 90, 1, 0))
WineList <- WineList %>% mutate("100" = ifelse(price > 90 & price <= 100, 1, 0))
WineList <- WineList %>% mutate("100+" = ifelse(price > 100, 1, 0))


WineList <- WineList %>% mutate("price_cat" = ifelse(price > 0 & price <= 10, 1, 
  ifelse(price > 10 & price <= 20, 10, ifelse(price > 20 & price <= 30, 20, 
  ifelse(price > 30 & price <= 40, 30, ifelse(price > 40 & price <= 50, 40, 
  ifelse(price > 50 & price <= 60, 50, ifelse(price > 60 & price <= 70, 60, 
  ifelse(price > 70 & price <= 80, 70, ifelse(price > 80 & price <= 90, 80,
  ifelse(price > 90 & price <= 100, 90, ifelse(price > 100, 1000, 0))))))))))))


Grpby <- WineList %>% group_by(price_cat, WesternEurope, NorthAmerica, 
  SouthAmerica, Africa, India, CentralAsia, NotA, MiddleEast, SouthEEurope, 
  EasternEurope, SouthPacific, EastAsia, Red, White)

WLbyR<- Grpby %>% summarise(p = mean(points))

# New Table ---------------------------------------------------------- 
WLR <- WineList[, c(3,4,11,12,13,14,15,16,17,18,19,20,21,22)]
WLNA <- WineList[, c(3,4,11)]
WLV <- WineList [c(3,4,6)]

# Corrplot ---------------------------------------------------------- 
M <- cor(WLR, use = "complete.obs")
corrplot(M, method = "number", type = "upper", col = "black")

N <- cor(WLV)
# Data Exploration ---------------------------------------------------------- 
reg.pt.reg <- subset(WineList, select = c("points", "region"))
summary(reg.pt.reg)
cor(reg.pt.reg)

reg.pr.reg <- subset(WineList, select = c("price", "region"))
summary(reg.pr.reg)
cor(reg.pr.reg)

reg.pr.pt <- subset(WineList, select = c("price", "points"))
summary(reg.pr.pt)
cor(reg.pr.pt)

# Correlation ----------------------------------------------------------
sat.mod <- lm(points ~ price + region + Type + variety, data = WineList)
summary(sat.mod)
coef(summary(sat.mod))
anova(sat.mod)

sat.mod2 <- lm(points ~ price, data = WineList)
summary(sat.mod2)

sat.mod3 <- lm(points ~ region, data = WineList)
summary(sat.mod3)

sat.mod4 <- lm(points ~ region + price, data = WineList)
summary(sat.mod4)

# Plots ---------------------------------------------------------- 
# Julian example
ggplot(WineList, aes(x=points, y=price)) + geom_point(aes(colour=region), size=2) + geom_smooth(aes(colour=region), se=FALSE, method="lm") + facet_grid(region~.) + scale_y_continuous(trans="log1p")


ggplot(WineList, aes(x=price, y=points)) + geom_point() + geom_smooth(aes(colour=region), method="lm") + scale_y_continuous(trans="log1p") + scale_x_continuous(trans="log1p")

ggplot(WineList, aes(points)) + geom_histogram()

ggplot(WineList, aes(price_cut)) + geom_histogram()

ggplot(WineList, aes(x=log(price))) + geom_histogram(aes(fill=region), alpha=0.40)

ggplot(WineList, aes(x = price, y = points, color = Type)) + geom_jitter() + geom_smooth() + scale_x_continuous(limits = c(5, 100)) + scale_y_continuous(limits = c(80, 100))

ggplot(WineList, aes(x=points)) + geom_density(aes(fill=region), alpha=0.50)