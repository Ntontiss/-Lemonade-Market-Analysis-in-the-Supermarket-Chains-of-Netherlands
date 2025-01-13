# Clear workspace ----
rm(list = ls())
#Open Lemonade Data
library(readr)
Lemonade<- read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/MARKET MODELS/Assignment 1/Lemonade Tidy format 2022.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Create proper date variable from week info ----
library(ISOweek)
Lemonade$Date <- ISOweek2date(sub("(wk) (\\d{2}) (\\d{2})","20\\3-W\\2-1", Lemonade$Week))

# Create quarter variable  ----
library(lubridate)
Lemonade$Quarter <- as.factor(quarter(Lemonade$Date))

#ADD VARIABLE WEEK
Lemonade$Week[Lemonade$Brand== "PrivateLabel"] <- c(1:208)
Lemonade$Week[Lemonade$Brand== "EuroShopper"] <- c(1:208)
Lemonade$Week[Lemonade$Brand== "KarvanCevitam"] <- c(1:208)
Lemonade$Week[Lemonade$Brand== "Raak"] <- c(1:208)
Lemonade$Week[Lemonade$Brand== "Slimpie"] <- c(1:208)
Lemonade$Week[Lemonade$Brand== "Teisseire"] <- c(1:208)

class(Lemonade$Week)
Lemonade$Week<-as.numeric(Lemonade$Week)

#CHECK FOR MISSINGS
colSums(is.na(Lemonade))
#REMOVE MISSINGS
Lemonade<-na.omit(Lemonade)
summary(Lemonade)
table(Lemonade$Chain)
table(Lemonade$Brand)

#DELETE THE EUROSHOPPER'S ZERO
library(dplyr)
Lemonade<-filter(Lemonade,UnitSales != 0.00)

#CHECK FOR MISSINGS
colSums(is.na(Lemonade))
Lemonade$Chain <- as.factor(Lemonade$Chain)
Lemonade$Brand <- as.factor(Lemonade$Brand)

# Download COVID data from the web ----
Covid_cases <- read.csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
# Data cleaning ----
## Only focus on data from the Netherlands ----
Covid_cases_Reduced <- Covid_cases[Covid_cases$Country.Region == "Netherlands",]

## Only focus on data from the main country ----
Covid_cases_Reduced_Further <- Covid_cases_Reduced[Covid_cases_Reduced$Province.State == "",]

## Transpose the dataframe and exclude the first four entries ----
Covid_cases_df <- data.frame(t(Covid_cases_Reduced_Further[,-c(1:4)]))

## Extract a date variable from the row names ----
Covid_cases_df$Date <- as.Date(row.names(Covid_cases_df),"X%m.%d.%y")

#only choose dates of 2020 on the data frame
Covid_cases_df_final <- Covid_cases_df[Covid_cases_df$Date >= "2020-01-22" & Covid_cases_df$Date <= "2020-12-31", ]

## Rename focal variable ----
names(Covid_cases_df_final)[names(Covid_cases_df_final)=="X201"] <- "CovidCases"

## Make a plot of the focal variable ----
plot(Covid_cases_df_final$Date,Covid_cases_df_final$CovidCases,type="l")

## Create a new variable, consisting of new cases ----
Covid_cases_df_final$NewCovidCases <- c(NA,diff(Covid_cases_df_final$CovidCases,1))

## Make a plot of the new variable ----
plot(Covid_cases_df_final$Date,Covid_cases_df_final$NewCovidCases,type="l")

## Add a week variable ----
library(lubridate)
Covid_cases_df_final$Week <- week(Covid_cases_df_final$Date)

## Calculate weekly averages ----
Weeks <- unique(Covid_cases_df_final$Week)
for (iWeek in Weeks) {
  Covid_cases_df_final$WeekAverage[Covid_cases_df_final$Week == iWeek] <- mean(Covid_cases_df_final$NewCovidCases[Covid_cases_df_final$Week == iWeek],na.rm = TRUE)  
}

## Make a plot to check whether we created the weekly average correctly
plot(Covid_cases_df_final$Date,Covid_cases_df_final$NewCovidCases,type="l")
lines(Covid_cases_df_final$Date,Covid_cases_df_final$WeekAverage,type="l", col="Red")

# Clean up after getting external data ----
rm(Covid_cases, Covid_cases_df, Covid_cases_Reduced, Covid_cases_Reduced_Further, iWeek, Weeks)
Covid_cases_weekly_2020 <- aggregate(NewCovidCases~Week, data = Covid_cases_df_final, mean)
rm(Covid_cases_df_final)
Covid_cases_weekly_2020$Week <- c(160:209)

#MERGE COVID WITH LEMONADE
Lemonade <- full_join(Lemonade, Covid_cases_weekly_2020, by = "Week")
Lemonade <- Lemonade[-7569,]
Lemonade$NewCovidCases<-round(Lemonade$NewCovidCases, digits = 0)
Lemonade$NewCovidCases[is.na(Lemonade$NewCovidCases)]<-0
rm(Covid_cases_weekly_2020)

#add holiday
holidays <- read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/MARKET MODELS/Assignment 1/Holidays.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
summary(holidays)

## Add a week variable ----
library(lubridate)
#round dates down to week
holidays$Week <- floor_date(holidays$Date, "week")

#find max dummy by week
holiday_weekly <- holidays %>%
  group_by(Week) %>%
  summarize(Holiday = max(Holiday_dummy)) %>%
  print(n = 209)
holiday_weekly$Week <- c(1:209)
holiday_weekly <- holiday_weekly[-209,]
Lemonade <- full_join(Lemonade, holiday_weekly, by = "Week")
rm(holidays,holiday_weekly)

#add temperature
#Temperature *in 0.1 degrees of Celsius
avgtemp <-read_delim("~/Library/Mobile Documents/com~apple~CloudDocs/MARKET MODELS/Assignment 1/averagetemp.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(avgtemp)
avgtemp$YYYYMMDD <- as.character(avgtemp$YYYYMMDD)
avgtemp$Date <- as.Date(avgtemp$YYYYMMDD, "%Y%m%d")
#round dates down to week
avgtemp$Week <- floor_date(avgtemp$Date, "week")
#find avgtemp by week
avgtemp_weekly <- avgtemp %>%
  group_by(Week) %>%
  summarize(Temperature = mean(TX)) %>%
  print(n = 209)

avgtemp_weekly <- avgtemp_weekly[-(210:307),]
avgtemp_weekly$Week <- c(1:209)
avgtemp_weekly <- avgtemp_weekly[-209,]
Lemonade <- full_join(Lemonade, avgtemp_weekly, by = "Week")
rm(avgtemp,avgtemp_weekly)

#SUMMARY LEMONADE
summary(Lemonade)
table(Lemonade$Chain)
table(Lemonade$Brand)

#SUMMARY LEMONADE PER SALES/ CHAIN
summary(Lemonade$UnitSales[Lemonade$Chain=="Albert Heijn"])
summary(Lemonade$UnitSales[Lemonade$Chain=="Jumbo"])
summary(Lemonade$UnitSales[Lemonade$Chain=="Plus"])
summary(Lemonade$UnitSales[Lemonade$Chain=="Coop"])
summary(Lemonade$UnitSales[Lemonade$Chain=="Deen"])
summary(Lemonade$UnitSales[Lemonade$Chain=="Hoogvliet"])
summary(Lemonade$UnitSales[Lemonade$Chain=="TotalOnlineSales"])

#SUMMARY LEMONADE PER SALES/ BRAND
summary(Lemonade$UnitSales[Lemonade$Brand=="PrivateLabel"])
summary(Lemonade$UnitSales[Lemonade$Brand=="EuroShopper"])
summary(Lemonade$UnitSales[Lemonade$Brand=="KarvanCevitam"])
summary(Lemonade$UnitSales[Lemonade$Brand=="Raak"])
summary(Lemonade$UnitSales[Lemonade$Brand=="Slimpie"])
summary(Lemonade$UnitSales[Lemonade$Brand=="Teisseire"])

#STANDARD DEVIATION
#Brand
sd(Lemonade$UnitSales[Lemonade$Brand=="PrivateLabel"])
sd(Lemonade$UnitSales[Lemonade$Brand=="EuroShopper"])
sd(Lemonade$UnitSales[Lemonade$Brand=="KarvanCevitam"])
sd(Lemonade$UnitSales[Lemonade$Brand=="Raak"])
sd(Lemonade$UnitSales[Lemonade$Brand=="Slimpie"])
sd(Lemonade$UnitSales[Lemonade$Brand=="Teisseire"])
#Chain
sd(Lemonade$UnitSales[Lemonade$Chain=="Albert Heijn"])
sd(Lemonade$UnitSales[Lemonade$Chain=="Jumbo"])
sd(Lemonade$UnitSales[Lemonade$Chain=="Plus"])
sd(Lemonade$UnitSales[Lemonade$Chain=="Coop"])
sd(Lemonade$UnitSales[Lemonade$Chain=="Deen"])
sd(Lemonade$UnitSales[Lemonade$Chain=="Hoogvliet"])
sd(Lemonade$UnitSales[Lemonade$Chain=="TotalOnlineSales"])

#
sd(Lemonade$UnitSales)
sd(Lemonade$PricePU)
sd(Lemonade$PricePL)
sd(Lemonade$BasePricePU)
sd(Lemonade$BasePricePL)
sd(Lemonade$FeatOnly)
sd(Lemonade$DispOnly)
sd(Lemonade$FeatDisp)

#BOXPLOT PER BASE UNIT PRICE / CHAIN
boxplot(Lemonade$BasePricePU[Lemonade$BasePricePU>0]~Lemonade$Chain[Lemonade$BasePricePU>0],col=c("dodgerblue", "orange", "firebrick2", "dodgerblue", "gold","yellowgreen", "grey") ,ylab = "Prices (\u20AC)", xlab = NULL, main = "Prices per chain" )
#BOXPLOT PER UNIT PRICE / BRAND
boxplot(Lemonade$PricePU[Lemonade$PricePU>0]~Lemonade$Brand[Lemonade$PricePU>0],col="firebrick1",ylab = "Prices (\u20AC)", main = "Prices per brand", xlab = NULL)

#PLOT PER CHAIN
library(ggplot2)
Lemonade %>% ggplot(aes(Week, UnitSales, col = Chain)) + facet_wrap( ~ Chain) +
  geom_point() + theme_minimal()

#PLOT PER BRAND
Lemonade %>% ggplot(aes(Week, UnitSales, col = Brand)) + facet_wrap( ~ Brand) +
  geom_point() + theme_minimal()

#PLOT PER CHAIN & PER BRAND
Lemonade %>% ggplot(aes(Week, UnitSales, col = Brand)) + facet_wrap( Chain~ Brand) +
  geom_point() + theme_minimal()

#CORRELATION
library(corrplot)
data<-(Lemonade[,4:11])
cor(data) 
cord<-cor(data) 
corrplot(cord,order = "hclust", method = "number")

#SALES LEVELS DIFFER PER QUARTER
BrandNames <- levels(unique(Lemonade$Brand))
ChainNames <- levels(unique(Lemonade$Chain))
ChainColors <- c("dodgerblue", "orange", "firebrick2", "dodgerblue","gold","yellowgreen","grey","red")
names(ChainColors) <- c("Albert Heijn", "Coop", "Deen", "Hoogvliet", "Jumbo", "Plus", "TotalOnlineSales")

#MAKE DATAFRAME WITH ONLY ALBERT HEIJN
AHLemonade <- Lemonade[Lemonade$Chain =="Albert Heijn",] 
AH_LemonadeKV <-AHLemonade[AHLemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = AH_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Albert Heijn",col=ChainColors["Albert Heijn"])

minUnitSales <- min(AH_LemonadeKV$UnitSales/1000,na.rm = TRUE)
maxUnitSales <- max(AH_LemonadeKV$UnitSales/1000,na.rm = TRUE)

plot(AH_LemonadeKV$Date[AH_LemonadeKV$Chain=="Albert Heijn"],AH_LemonadeKV$UnitSales[AH_LemonadeKV$Chain=="Albert Heijn"]/1000,col=ChainColors["Albert Heijn"],ylab="Unit sales per week (x 1000)",xlab = "Time", main = "Development of weekly unit sales of Karvan Cevitam over time",type = "l", ylim = c(minUnitSales,maxUnitSales))
for (i in 2:length(ChainNames)) {
  lines(AH_LemonadeKV$Date[AH_LemonadeKV$Chain==ChainNames[i]],AH_LemonadeKV$UnitSales[AH_LemonadeKV$Chain==ChainNames[i]]/1000,col=ChainColors[ChainNames[i]])
}

legend("topleft",inset = c(.005,0.01),ChainNames,col=ChainColors, lty = rep(1,6))

minBasePrice <- min(AH_LemonadeKV$BasePricePU,na.rm = TRUE)   # this results in 0, which is not very useful
minBasePrice <- min(AH_LemonadeKV$BasePricePU[AH_LemonadeKV$BasePricePU>0],na.rm = TRUE)
maxBasePrice <- max(AH_LemonadeKV$BasePricePU,na.rm = TRUE)

plot(AH_LemonadeKV$Date[AH_LemonadeKV$Chain=="Albert Heijn"],AH_LemonadeKV$BasePricePU[AH_LemonadeKV$Chain=="Albert Heijn"],col=ChainColors["Albert Heijn"],ylab="Base price per unit (\u20AC)",xlab = "Time", main = "Development of base price of Karvan Cevitam over time",type = "l", ylim = c(minBasePrice,maxBasePrice))
for (i in 2:length(ChainNames)) {
  lines(AH_LemonadeKV$Date[AH_LemonadeKV$Chain==ChainNames[i]],AH_LemonadeKV$BasePricePU[AH_LemonadeKV$Chain==ChainNames[i]],col=ChainColors[ChainNames[i]])
}

legend("bottomleft",inset = c(.005,0.01),ChainNames,col=ChainColors, lty = rep(1,6))

#MAKE DATAFRAME WITH ONLY Jumbo
Jumbo_Lemonade <- Lemonade[Lemonade$Chain =="Jumbo",]  
Ju_LemonadeKV <-Jumbo_Lemonade[Jumbo_Lemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = Ju_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Jumbo",col=ChainColors["Jumbo"])

#MAKE DATAFRAME WITH ONLY Coop
Coop_Lemonade <- Lemonade[Lemonade$Chain =="Coop",]  
CO_LemonadeKV <-Coop_Lemonade[Coop_Lemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = CO_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Coop",col=ChainColors["Coop"])

#MAKE DATAFRAME WITH ONLY DEEN
Deen_Lemonade <- Lemonade[Lemonade$Chain =="Deen",]  
DE_LemonadeKV <-Deen_Lemonade[Deen_Lemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = DE_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Deen",col=ChainColors["Deen"])

#MAKE DATAFRAME WITH ONLY Hoogvliet
Hoogvliet_Lemonade <- Lemonade[Lemonade$Chain =="Hoogvliet",]
HO_LemonadeKV <-Hoogvliet_Lemonade[Hoogvliet_Lemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = HO_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Hoogvliet",col=ChainColors["Hoogvliet"])

#MAKE DATAFRAME WITH ONLY TOTAL ONLINE SALES
TotalOnlineSales_Lemonade <- Lemonade[Lemonade$Chain =="TotalOnlineSales",]
TOS_LemonadeKV <-TotalOnlineSales_Lemonade[TotalOnlineSales_Lemonade$Brand == "KarvanCevitam",]
boxplot(UnitSales/1000~Quarter, data = TOS_LemonadeKV, xlab="Quarter", ylab = "Unit sales (x 1000)", main = "Weekly KarvanCevitam sales per quarter at Online Sales",col=ChainColors["Online Sales"])

# Stuff for creating a smooth and consistent version of BasePricePU (I called it BasePricePU_smooth) ----

#Check for inconsistiencies:
Lemonade$Promotion <- Lemonade$BasePricePU - Lemonade$PricePU
percentagefalse <- (sum(Lemonade$Promotion < 0, na.rm=TRUE)/nrow(Lemonade))*100 # starts with ~21% inconsistent values

Lemonade$BasePricePU_Smooth <- rep(0,nrow(Lemonade)) #create a new variable in the data frame - a smoothed version of BasePricePU - initially filled up with zeros

#Now that smoothing is done, replace any remaining smoothed BasePricePU values by PricePU values if they are lower than PricePU
Lemonade$BasePricePU_Smooth <- ifelse(Lemonade$BasePricePU_Smooth > Lemonade$PricePU, Lemonade$BasePricePU_Smooth, Lemonade$PricePU)
#Now check again if there are inconsistencies
Lemonade$Promotion<- Lemonade$BasePricePU_Smooth - Lemonade$PricePU
percentagefalse <- (sum(Lemonade$Promotion < 0, na.rm=TRUE)/nrow(Lemonade))*100 # now no inconsistencies anymore

#Deal with the feat, display, featdisplay problem:
Lemonade$advertisements <- Lemonade$FeatOnly + Lemonade$DispOnly + Lemonade$FeatDisp
Lemonade$NewFeatOnly <- ifelse(Lemonade$advertisements > 100, 100 * Lemonade$FeatOnly/Lemonade$advertisements,Lemonade$FeatOnly)
Lemonade$NewDispOnly <- ifelse(Lemonade$advertisements > 100, 100 * Lemonade$DispOnly/Lemonade$advertisements,Lemonade$DispOnly)
Lemonade$NewFeatDisp <- ifelse(Lemonade$advertisements > 100, 100 * Lemonade$FeatDisp/Lemonade$advertisements,Lemonade$FeatDisp)

#CREATE PRICE INDEX
Lemonade$Price_Index<- Lemonade$PricePU/Lemonade$BasePricePU
#PRICE INDEX THAT AFFECTS
Lemonade$Price_Index_before <- c(NA,Lemonade$Price_Index[1:nrow(Lemonade)-1])
Lemonade$Price_Index_before[Lemonade$Week == "1"] <- NA
#SALES THAT AFFECT 
Lemonade$UnitSales_lag <- c(NA,Lemonade$UnitSales[1:nrow(Lemonade)-1])
Lemonade$UnitSales_lag [Lemonade$Week == "1"] <- NA

#COMPETITORS PRICE
Avg1<-Lemonade %>%  filter(Brand != "KarvanCevitam")%>%
  group_by(Week) %>%
  summarise(across(starts_with('PricePU'), mean))
Lemonade <- full_join(Lemonade,Avg1 , by = "Week")

colnames(Lemonade)[26] ="AVG_Comp"
colnames(Lemonade)[5] ="PricePU"
rm(Avg1)

#CHECKING FOR OUTLIERS
#UNIT SALES
boxplot(Lemonade$UnitSales) #OUTLIERS
hist(Lemonade$UnitSales)
boxplot.stats(Lemonade$UnitSales)$out

#PRICE PU
boxplot(Lemonade$PricePU) #NO OUTLIERS
hist(Lemonade$PricePU)
boxplot.stats(Lemonade$PricePU)$out

#PRICE PL
boxplot(Lemonade$PricePL) #TWO OUTLIERS
hist(Lemonade$PricePL)
boxplot.stats(Lemonade$PricePL)$out
#WE HAVE ONLY 2 OUTLIERS

#BASE PRICE PL
boxplot(Lemonade$BasePricePL) #NO OUTLIERS
hist(Lemonade$BasePricePL)
boxplot.stats(Lemonade$BasePricePL)$out

#BASE PRICE PU
boxplot(Lemonade$BasePricePU) #NO OUTLIERS
hist(Lemonade$BasePricePU)
boxplot.stats(Lemonade$BasePricePU)$out

#BASE PRICE PU
boxplot(Lemonade$BasePricePU) #NO OUTLIERS
hist(Lemonade$BasePricePU)
boxplot.stats(Lemonade$BasePricePU)$out

#Covid
boxplot(Lemonade$NewCovidCases) #OUTLIERS
hist(Lemonade$NewCovidCases)
boxplot.stats(Lemonade$NewCovidCases)$out

#Temperature
boxplot(Lemonade$Temperature) # no OUTLIERS
hist(Lemonade$Temperature)
boxplot.stats(Lemonade$Temperature)$out

#----Deal with outliers 
#Mahalanobis pricepl
x <- matrix(rnorm(Lemonade$UnitSales, Lemonade$PricePL), ncol = 2)
Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)

# Plot Mahalanobis distance for each point ---
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=7568, p=2",lwd=2) ; rug(D2)

# Plot Chi-square distribution with df = 2 ---
xrange <- seq(0,12,by=.001)
lines(xrange,dchisq(xrange,2),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance ---
qqplot(qchisq(ppoints(7568), df = 2), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
rm(x,  Sx, D2, xrange)
# Checking the QQ-plot one observation do deviate. 

#That's why we use log_PricePl to deal with these outliers.
Lemonade$log_PricePL <- log(Lemonade$PricePL)
boxplot(Lemonade$log_PricePL) # Outliers

## Mahalanobis PricePl logged ---
x <- matrix(rnorm(Lemonade$UnitSales,Lemonade$log_PricePL), ncol = 2)
Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)

# Plot Mahalanobis distance for each point ---
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=7568, p=2",lwd=2) ; rug(D2)

# Plot Chi-square distribution with df = 2 ---
xrange <- seq(0,12,by=.001)
lines(xrange,dchisq(xrange,2),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance ---
qqplot(qchisq(ppoints(7568), df = 2), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
rm(x,  Sx, D2, xrange) # Clean up after calculating
# Checking the QQ-plot the observations do not  deviate. 

#MAHALANOBIS DISTANCE
## Mahalanobis Covid ---
x <- matrix(rnorm(Lemonade$UnitSales, Lemonade$NewCovidCases), ncol = 2)
Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)

# Plot Mahalanobis distance for each point ---
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=7568, p=2",lwd=2) ; rug(D2)

# Plot Chi-square distribution with df = 2 ---
xrange <- seq(0,12,by=.001)
lines(xrange,dchisq(xrange,2),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance ---
qqplot(qchisq(ppoints(7568), df = 2), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
rm(x,  Sx, D2, xrange) # Clean up after calculating
# Checking the QQ-plot the observations show one outlier

Lemonade$log_NewCovidCases <- log(Lemonade$NewCovidCases)
boxplot(Lemonade$log_NewCovidCases) # Outliers
# Introduces inf thus we use log1p function as Mahalanobis cannot deal with inf/NA

Lemonade$log_NewCovidCases1 <- log1p(Lemonade$NewCovidCases)
boxplot(Lemonade$log_NewCovidCases1) # Less outliers
hist(Lemonade$log_NewCovidCases1) # Shows a normal distribution
## Mahalanobis Covid logged ---
x <- matrix(rnorm(Lemonade$UnitSales, Lemonade$log_NewCovidCases1), ncol = 2)
Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)

# Plot Mahalanobis distance for each point ---
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, n=7568, p=2",lwd=2) ; rug(D2)

# Plot Chi-square distribution with df = 2 ---
xrange <- seq(0,12,by=.001)
lines(xrange,dchisq(xrange,2),col="dodgerblue",lwd=2)

# Make Q-Q plot to assess Mahalanobis distance ---
qqplot(qchisq(ppoints(7568), df = 2), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')
rm(x,  Sx, D2, xrange) # Clean up after calculating
# Checking the QQ-plot one observation do deviate. 

Lemonade <- Lemonade[ -c(28,29) ] # Delete variable

#ANOVA CHECK FOR SEASONALITY#
anova_out<-aov(UnitSales~ Quarter, data= Lemonade)
summary(anova_out)

##fluctuations of the sales is price
ModelBrandsChains <- lm(UnitSales~ PricePU, data= Lemonade)
summary(ModelBrandsChains)

by(Lemonade, Lemonade[,c("Chain", "Brand")], function(x) summary(lm(UnitSales~PricePU,data = x)))

# Stuff for lecture 3 ----
MultiplicativeModel <- lm(log(UnitSales)~log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+log(Temperature)+NewCovidCases+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag),data=Lemonade)
summary(MultiplicativeModel)
MultiplicativeModel$coefficients

## Apply the anti-log transformation to alpha_hat_star ----
alpha_hat_star <- summary(MultiplicativeModel)$coefficients[1,1]
sd_alpha_hat_star <- summary(MultiplicativeModel)$coefficients[1,2]
alpha_hat <- exp(alpha_hat_star) * exp(-0.5*(sd_alpha_hat_star^2))
sprintf("alpha_hat = %.2f", alpha_hat )

## Apply the anti-log transformation to beta2 ----
beta2_hat_star<- MultiplicativeModel$coefficients[2]
sd_beta2_hat_star <- summary(MultiplicativeModel)$coefficients[2,2]
beta2_hat <- exp(beta2_hat_star) * exp(-0.5*(sd_beta2_hat_star^2))
sprintf("beta2_hat = %.2f", beta2_hat )

## Apply the anti-log transformation to beta3 ----
beta3_hat_star<- MultiplicativeModel$coefficients[3]
sd_beta3_hat_star <- summary(MultiplicativeModel)$coefficients[3,2]
beta3_hat <- exp(beta3_hat_star) * exp(-0.5*(sd_beta3_hat_star^2))
sprintf("beta3_hat = %.2f", beta3_hat )

## For beta4,beta5,beta6,beta7 and beta9 the anti-log transformation is not needed! ----
sprintf("beta4_hat = %.6f", MultiplicativeModel$coefficients[4])
sprintf("beta5_hat = %.6f", MultiplicativeModel$coefficients[5])
sprintf("beta6_hat = %.6f", MultiplicativeModel$coefficients[6])
sprintf("beta7_hat = %.6f", MultiplicativeModel$coefficients[7])
sprintf("beta9_hat = %.6f", MultiplicativeModel$coefficients[9])

## Apply the anti-log transformation to beta8 ----
beta8_hat_star<- MultiplicativeModel$coefficients[8]
sd_beta8_hat_star <- summary(MultiplicativeModel)$coefficients[8,2]
beta8_hat <- exp(beta8_hat_star) * exp(-0.5*(sd_beta8_hat_star^2))
sprintf("beta8_hat = %.2f", beta8_hat )

## Apply the anti-log transformation to beta10 ----
beta10_hat_star<- MultiplicativeModel$coefficients[10]
sd_beta10_hat_star <- summary(MultiplicativeModel)$coefficients[10,2]
beta10_hat <- exp(beta10_hat_star) * exp(-0.5*(sd_beta10_hat_star^2))
sprintf("beta10_hat = %.2f", beta10_hat )

## Apply the anti-log transformation to beta11 ----
beta11_hat_star<- MultiplicativeModel$coefficients[11]
sd_beta11_hat_star <- summary(MultiplicativeModel)$coefficients[11,2]
beta11_hat <- exp(beta11_hat_star) * exp(-0.5*(sd_beta11_hat_star^2))
sprintf("beta11_hat = %.2f", beta11_hat )

## Apply the anti-log transformation to beta12 ----
beta12_hat_star<- MultiplicativeModel$coefficients[12]
sd_beta12_hat_star <- summary(MultiplicativeModel)$coefficients[12,2]
beta12_hat <- exp(beta12_hat_star) * exp(-0.5*(sd_beta12_hat_star^2))
sprintf("beta12_hat = %.2f", beta12_hat )

## Obtain the standard deviation of the residuals ----
sd_residuals<-summary(MultiplicativeModel)$sigma

## Calculate the fitted values ----
LemonadeFit<-exp(MultiplicativeModel$fitted.values)*exp(1/2*sd_residuals^2)

# Stuff for lecture 4 ----
plot(Lemonade$Date,Lemonade$UnitSales,type="p",pch=21,bg="dodgerblue",col="dodgerblue",xlab = "Weeks",ylab="Sales of Lemonade (units)",main = "Sales of Lemonade")
lines(Lemonade$Date,Lemonade$UnitSales,lwd=2,col="dodgerblue")

# Estimating a linear model for Lemonade
LinearModel <- lm(UnitSales~PricePU+PricePU+Price_Index+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+Temperature+NewCovidCases+AVG_Comp+Price_Index_before+UnitSales_lag,data=Lemonade)
summary(LinearModel)
LinearModel$coefficients

#ESTIMATE A POOLED MODEL
PooledModel<- lm(log(UnitSales) ~ log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+
                   log(Temperature+273)+NewCovidCases+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag), 
                 data = Lemonade) 
summary(PooledModel)
summary(aov(PooledModel))

#Estimating Partially pooled version of the model:
#First create dummies for the chains:
Lemonade$D_AH<- rep(0,nrow(Lemonade))
Lemonade$D_Jumbo<- rep(0,nrow(Lemonade))
Lemonade$D_Plus<- rep(0,nrow(Lemonade))
Lemonade$D_Coop<- rep(0,nrow(Lemonade))
Lemonade$D_Deen<- rep(0,nrow(Lemonade))
Lemonade$D_HV<- rep(0,nrow(Lemonade))
Lemonade$D_TOS<- rep(0,nrow(Lemonade))

Lemonade$D_AH[Lemonade$Chain=="Albert Heijn"]<- 1
Lemonade$D_Jumbo[Lemonade$Chain=="Jumbo"]<- 1
Lemonade$D_Plus[Lemonade$Chain=="Plus"]<- 1
Lemonade$D_Coop[Lemonade$Chain=="Coop"]<- 1
Lemonade$D_Deen[Lemonade$Chain=="Deen"]<- 1
Lemonade$D_HV[Lemonade$Chain=="Hoogvliet"]<- 1
Lemonade$D_TOS[Lemonade$Chain=="TotalOnlineSales"]<- 1

#Estimate partially pooled model with 5 dummies, with intercept
PartiallyPooledModel <- lm(log(UnitSales) ~D_AH + D_Jumbo + D_Plus + D_Coop + D_Deen + D_HV + D_TOS +
                             log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+
                            log(Temperature+273)+NewCovidCases+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag), 
                                data = Lemonade)   
summary(PartiallyPooledModel)
summary(aov(PartiallyPooledModel))

# Chow test:
install.packages("qpcR")
library(qpcR)

#dfpooled:
PooledModel$df.residual
#dfunpooled
PartiallyPooledModel$df.residual
# F-statistic:
anovaPooledModel<-anova(PooledModel)
SSRA <- anovaPooledModel$`Sum Sq`[12]
anovaPartiallyPooledModel<-anova(PartiallyPooledModel)
SSRB<- anovaPartiallyPooledModel$`Sum Sq`[18]

F <- ((SSRA - SSRB)/
        (PooledModel$df.residual-PartiallyPooledModel$df.residual))/(SSRB/PartiallyPooledModel$df.residual)

#calculate the p-value
pf(F,PooledModel$df.residual-PartiallyPooledModel$df.residual,PartiallyPooledModel$df.residual) #INSIGNIFICANT 

# Please use a part of the data for calibration, and save a part for validation
Lemonade_Calibrate <- Lemonade[Lemonade$Date < "2020-08-01",]
Lemonade_Validate <- Lemonade[Lemonade$Date >= "2020-08-01",]

length(unique(Lemonade_Calibrate$Date))
length(unique(Lemonade_Validate$Date))

MultiplicativeCalib <- lm(log(UnitSales)~log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+log(Temperature)+NewCovidCases+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag),data=Lemonade_Calibrate)
MultiplicativeValid <- lm(log(UnitSales)~log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+log(Temperature)+NewCovidCases+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag),data=Lemonade_Validate)

#VIF scores:
library(car)
vif(MultiplicativeCalib) #vif in the calibrate data
#we dont have multicollinearity 
vif(MultiplicativeValid) #vif in the whole validation data
vif(MultiplicativeModel) #vif in the whole lemonade data

## TOLERANCE & VARIANCE INFLATION FACTOR (VIF)
install.packages("olsrr")
library("olsrr")
ols_vif_tol(MultiplicativeCalib)
ols_vif_tol(MultiplicativeValid)
ols_vif_tol(MultiplicativeModel)

# Stuff for lecture 6 ---- 
#AUTOCORRELATION
durbinWatsonTest(MultiplicativeCalib$residuals)
#positive autocorrelation the difference is small
#the upper and lower limit from DW table is 1.643 and 1.896
#we are on the green line NO AUTOCORRELATION
library(lmtest)
dwtest(MultiplicativeCalib)

#NON-NORMALITY
hist(MultiplicativeCalib$residuals,probability = TRUE)
curve(dnorm(x, mean=mean(MultiplicativeCalib$residuals), sd=sd(MultiplicativeCalib$residuals)), add=TRUE, col="red")

res_std <- rstandard(MultiplicativeCalib)
qqnorm(res_std,ylab="Standardized Residuals",xlab="Normal Scores")
qqline(res_std,col="red")
#IT'S NOT NORMALLY DISTRIBUTED BUT IT DOES NOT DEVIATE MUCH FROM THE LINE
#normality is not violated
#NORMALITY TEST
library(nortest)
lillie.test(MultiplicativeCalib$residuals)
#reject ho hypothesis
#resolve non normality
library(boot)
lmbootstrap <- function(formula,data){ 
  boot.run <- function(data, indices){
    data <- data[indices,] # select obs. in bootstrap sample
    mod <- lm(formula, data=data)
    coefficients(mod) # return coefficient vector
  }
  boot_aux <- boot(data,boot.run, 1999)
  Coefficient <- names(boot_aux$t0)
  boot_out <- data.frame(Coefficient)
  for (i in 1:length(boot_aux$t0)) {
    boot_out$Estimate[i] <- boot_aux$t0[i]
    boot_out$Std.Error[i] <- sd(boot_aux$t[,i])    
    boot_out$Bias[i] <- mean(boot_aux$t[,i])-boot_aux$t0[i]
    boot_out$p_bootstrap[i] <- dt(mean(boot_aux$t[,i])/sd(boot_aux$t[,i]),boot_aux$R-dim(boot_aux$t)[2])
  }
  return(boot_out)
}

lmbootstrap(log(UnitSales)~log(PricePU)+log(Price_Index)+NewFeatOnly+NewDispOnly+NewFeatDisp+Holiday+NewCovidCases+
                  log(Temperature+273)+log(AVG_Comp)+log(Price_Index_before)+log(UnitSales_lag), 
                data = Lemonade_Calibrate)

# The last column in the output above contain the bootstrapped p-values
# For comparison to the original p-values: compare to the p-values of the regular regression:


#CHECK FOR heteroskedasticity
PromotionalWeeks <- which(Lemonade$PricePU < 2.00)
NonPromotionalWeeks <- which(Lemonade$PricePU >= 2.00)
NonPromotionalWeeks <- NonPromotionalWeeks[2:length(NonPromotionalWeeks)]
length(PromotionalWeeks)
PromotionalModel <-lm(log(UnitSales)[PromotionalWeeks]~log(PricePU)[PromotionalWeeks]+log(Price_Index)[PromotionalWeeks]+NewFeatOnly[PromotionalWeeks]+NewDispOnly[PromotionalWeeks]+NewFeatDisp[PromotionalWeeks]+Holiday[PromotionalWeeks]+NewCovidCases[PromotionalWeeks]+
     log(Temperature)[PromotionalWeeks]+log(AVG_Comp)[PromotionalWeeks]+log(Price_Index_before)[PromotionalWeeks]+log(UnitSales_lag)[PromotionalWeeks], 
   data = Lemonade_Calibrate) 
summary(PromotionalModel)
anovaPromotionalModel<-anova(PromotionalModel)
SSR1 <- anovaPromotionalModel$`Sum Sq`[12]

NonPromotionalModel <-lm(log(UnitSales)[NonPromotionalWeeks]~log(PricePU)[NonPromotionalWeeks]+log(Price_Index)[NonPromotionalWeeks]+NewFeatOnly[NonPromotionalWeeks]+NewDispOnly[NonPromotionalWeeks]+NewFeatDisp[NonPromotionalWeeks]+Holiday[NonPromotionalWeeks]+NewCovidCases[NonPromotionalWeeks]+
                           log(Temperature)[NonPromotionalWeeks]+log(AVG_Comp)[NonPromotionalWeeks]+log(Price_Index_before)[NonPromotionalWeeks]+log(UnitSales_lag)[NonPromotionalWeeks], 
                         data = Lemonade_Calibrate)
summary(NonPromotionalModel)
anovaNonPromotionalModel<-anova(NonPromotionalModel)
SSR2 <- anovaNonPromotionalModel$`Sum Sq`[12]
#THE VARIANCE IS HIGHER IN THE NON PROMOTION MODEL

length(PromotionalWeeks)
length(NonPromotionalWeeks)
Fvalue <- (SSR1/(22-12))/(SSR2/(54-12))
pf(Fvalue,22-12,54-12) #INSIGNIFICANT heteroskedasticity!

#Lecture 7
ModelPredictions <- predict(MultiplicativeModel,newdata = Lemonade_Validate[758:763,])

APE <- sum(Lemonade_Validate$UnitSales[758:763]-ModelPredictions)/(6)
ASPE <- sum((Lemonade_Validate$UnitSales[758:763]-ModelPredictions)^2)/(6)
RASPE <- sqrt(ASPE)
summary(aov(MultiplicativeCalib))

MAPE <- (1/6)*sum(abs( (Lemonade_Validate$UnitSales[758:763]-ModelPredictions) /
                         (Lemonade_Validate$UnitSales[758:763]) ) )

RAE <- sum(abs(Lemonade_Validate$UnitSales[758:763]-ModelPredictions)) / 
  sum(abs(Lemonade_Validate$UnitSales[758:763]-Lemonade_Validate$UnitSales[757:762]))

TheilsU <- sqrt( sum((Lemonade_Validate$UnitSales[758:763]-ModelPredictions)^2) / 
                   sum((Lemonade_Validate$UnitSales[758:763]-Lemonade_Validate$UnitSales[757:762])^2) )


