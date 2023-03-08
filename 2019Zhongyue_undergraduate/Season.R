#'@author Hang Cheng
#'@title Season
#'@Date 2023.03.06

# Preparing -----------------------------------------------------------------------------------
#Sys.setlocale(category = 'LC_ALL', locale = 'English_United States.1252')

# Import My function code, I can sent you this function file.
# This file includs all the package and function which may be used.
source('D:/Academic/academic/Assetpricing/Myfunction.R')

# Import Daily Stock Market Data --------------------------------------------------------------
# This data is RDS file. You can import your data. 
Marketret_day_stock <- readRDS('D:/Academic/academic/Assetpricing/Output/Marketret_day_stock.RDS')

# Add dummy variables of week days.
Marketret_day_stock[,':='(Tue = ifelse(weekdays(Day) == 'Tuesday',1,0),
                          Wed = ifelse(weekdays(Day) == 'Wednesday',1,0),
                          Thu = ifelse(weekdays(Day) == 'Thursday',1,0),
                          Fri = ifelse(weekdays(Day) == 'Friday',1,0))]

# Model

m1 <- lm(er ~ Tue + Wed + Thu + Fri, Marketret_day_stock[Day >= '2000-01-01' & Day <= '2011-12-31'])
m1nw <- coeftest(m1,vcov = NeweyWest(m1))
stargazer(m1nw,m1,
          title = 'Univariate Forecasting Regression NW Adjust',
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'tiny',no.space = T,align = F)


# Add dummy variables of 4
Marketret_day_stock[,':='(four = ifelse(day(Day) == 4 |day(Day) == 14 |day(Day) == 24 ,1,0))]

# Model

m1 <- lm(er ~ Tue + Wed + Thu + Fri + four, Marketret_day_stock[Day >= '2015-01-01' & Day <= '2021-12-31'])
m1nw <- coeftest(m1,vcov = NeweyWest(m1))
stargazer(m1nw,m1,
          title = 'Univariate Forecasting Regression NW Adjust',
          type = "text",report = ('vc*t'),omit.stat = c("ser","f"),
          style = "all",font.size = 'tiny',no.space = T,align = F)













