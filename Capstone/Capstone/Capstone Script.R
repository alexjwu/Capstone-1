
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

# Do not use:

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

# Remove un-needed data ---------------------------------------------------------- Use column names not indices
DF3 <- DF[, c(1,2,5,6,7,13,14)]

WineList <- DF3

rm(df, Exm, Grpby, reg.pr.pt, reg.pr.reg, reg.pt.reg, Regct, sat.mod, sat.mod2, sat.mod3, sat.mod4, WLbyR, WLNA, WLV)

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

center_x <- function(x) { x - mean(x)}
z_score <- function(x) { (x - mean(x)) / sd(x)}



# Julian example
ggplot(WineList, aes(x=SDpoints, y=SDprice)) + geom_point(aes(colour=region), size=2) + geom_smooth(aes(colour=region), se=FALSE, method="lm") + facet_grid(region~.) + scale_y_continuous(trans="log1p")

ggplot(WineList, aes(x=SDpoints)) + geom_density(aes(fill=region),
                                                 alpha = 0.4)





ggplot(WineList, aes(points)) + geom_histogram()

ggplot(WineList, aes(price_cut)) + geom_histogram()

ggplot(WineList, aes(x=log(price))) + geom_histogram(aes(fill=region), alpha=0.40)

ggplot(WineList, aes(x = price, y = points, color = Type)) + geom_jitter() + geom_smooth() + scale_x_continuous(limits = c(5, 100)) + scale_y_continuous(limits = c(80, 100))

ggplot(WineList, aes(x=points)) + geom_density(aes(fill=region), alpha=0.50)