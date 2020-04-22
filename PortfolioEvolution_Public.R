library(packrat)
#library(quantmod)
library(tidyverse)
library(data.table)
#library("xlsx")
library(dplyr)
#library(BatchGetSymbols)
library("ggplot2")
library(grDevices)
library(hrbrthemes) #Themes for ggplot2
library(magrittr)  # %>%
library(xts)    # as.xts
library(Rmisc)   # Multiplot
library(readxl) 
library(writexl) 


#Variables names are snake_case underscore and Data Frame names are snake_case with first letters in capital
#Data Frames Summary:
#Portfolio_Daily: keeps records as are imported from Degiro
#Portfolio_Evolution: shows the changes per day of the Portfolio and main indexes (or ETFs) to benchmark. Weekends are excluded
#Portfolio_vs_Benchmarks: Portfolio vs Benchmarks on different time frames
#Cashflow: keeps records of deposits and withdrawals from/to your Investment account/s. Each movement must be added manually to this data frame

rm(list = ls(all.names = TRUE))
setwd('c:/Pathtoyourfolder/')
#load("Portfolio Evolution.RData")

commarep <- function(c) {sub(",",".",c)}
'%!in%' <- function(x,y)!('%in%'(x,y))

cat("\f")  

#IMPORT Degiro PORTFOLIO CSV
#Copy the link to Degiro CSV Portfolio Export and run the script. IMPORTANT: you must be logged in Degiro.

# For batch import of several days:

#degiro_csv_link <- readline(prompt="Paste Degiro's CSV link: ")  
#degiro_csv_fromDate <- readline(prompt="Enter From Date in YYYY-MM-DD Format: ") 
#degiro_csv_toDate_pos <- readline(prompt="Enter To Date in YYYY-MM-DD Format: ") 
#days = as.integer(as.Date(degiro_csv_toDate_pos) - as.Date(degiro_csv_fromDate))+1
#Portfolio_Daily <- data.frame(Date = as.character(), Product = as.character(), ISIN = as.character(), Amount = as.numeric(), Closing = as.numeric(), Localvalue = as.character(), ValueinEUR = as.character())
#degiro_csv_toDate <- degiro_csv_fromDate
#for (i in 1:days){ 
#  degiro_csv_link <- paste0(substr(degiro_csv_link,1, nchar(degiro_csv_link)-14),substr(degiro_csv_toDate,9, 10),"%2F",substr(degiro_csv_toDate,6, 7),"%2F",substr(degiro_csv_toDate,1, 4))
#  CSV_Degiro <- fread(degiro_csv_link)
#  CSV_Degiro$Date <- degiro_csv_toDate
#  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
#  #Append to Portfolio Daily history
#  Portfolio_Daily <- rbind(Portfolio_Daily,CSV_Degiro,use.names=FALSE)
#  degiro_csv_toDate <- as.character(as.Date(degiro_csv_toDate) %m+% days(1))
#  #Delete CSV Data Frame
#  rm(CSV_Degiro)
#}
#END BATCH IMPORT
###################

# One Day import:
degiro_csv_link <- readClipboard()
degiro_csv_toDate_pos <- as.integer(gregexpr("toDate" ,degiro_csv_link))
degiro_csv_toDate <- as.Date(paste0(substr(degiro_csv_link,degiro_csv_toDate_pos + 17, degiro_csv_toDate_pos + 21),"-",substr(degiro_csv_link,degiro_csv_toDate_pos + 12, degiro_csv_toDate_pos + 13),"-",substr(degiro_csv_link,degiro_csv_toDate_pos + 7, degiro_csv_toDate_pos + 8)))

# Gererate link to Account Statment Import
degiro_csv_fromDate <- degiro_csv_toDate - 8
degiro_acc_stat_link <- sub('positionReport','cashAccountReport',degiro_csv_link)
degiro_acc_stat_link <- sub('&country=IE&lang=en',paste0('&country=IE&lang=en&fromDate=',substr(degiro_csv_fromDate,9,10),'%2F',substr(degiro_csv_fromDate,6,7),'%2F',substr(degiro_csv_fromDate,1,4)),degiro_acc_stat_link)

# Download Account Statment CSV
Degiro_acc_stat <- fread(degiro_acc_stat_link)

# Cleaning Acc Stat
Degiro_acc_stat <- Degiro_acc_stat[,c(1,4:11)]
names(Degiro_acc_stat)[6:9] <- c("Change Cur.", "Change", "Balance Cur.", "Balance")
rm(degiro_acc_stat_link,degiro_csv_fromDate)

#Open Local file with Portfolio's daily movements
Portfolio_Daily <- read_excel("Portfolio_Daily.xlsx")
Portfolio_Daily$Date <- as.Date(Portfolio_Daily$Date,format = "%Y-%m-%d")

#Check if Degiro CSV's hasn't been already imported to our Portfolio Daily dataframe...
if (degiro_csv_toDate %!in% Portfolio_Daily$Date) 
{
  #...if not, start with CSV file import 
  CSV_Degiro <- fread(degiro_csv_link)
  CSV_Degiro$Date <- degiro_csv_toDate
  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
  names(CSV_Degiro) <- c("Date","Product","ISIN","Amount","Closing","Localvalue","ValueinEUR")
  #Append to Portfolio Daily history
  Portfolio_Daily <- rbind(Portfolio_Daily,CSV_Degiro,use.names=FALSE)
  Portfolio_Daily <- Portfolio_Daily[order(-Portfolio_Daily$Date),]
  #names(Portfolio_Daily) <- c("Date","Product","ISIN","Amount","Closing","Localvalue","ValueinEUR")
  #Save updated Portfolio Daily to local drive in Excel format (work Directory)
  write_xlsx (Portfolio_Daily, "Portfolio_Daily.xlsx")
  #Delete CSV Data Frame  
  rm(CSV_Degiro)
}
#END ONE DAY IMPORT

#Add imported day Portfolio value to Portfolio Evolution 
Portfolio_Evolution <- data.frame(aggregate(as.numeric(commarep(Portfolio_Daily$ValueinEUR)),list(Portfolio_Daily$Date),sum,na.rm=TRUE))
names(Portfolio_Evolution)[1:2] <- c("Date","Portfolio")
Portfolio_Evolution$Date <- as.Date(Portfolio_Evolution$Date)
Portfolio_Evolution$HPR <- 0 #Add Holding Return Period column, which is basically the Portfolio gains/losses minus deposits and withdrawals
#Search for movements in Degiro Account   
Cashflow <- read_excel("Cashflow.xlsx")
Cashflow_Degiro <- data.frame(subset(Cashflow[c(1,3)],Cashflow$Account == "Degiro"))
Cashflow_Degiro$Date <- as.Date(Cashflow_Degiro$Date)
Portfolio_Evolution <- merge(x = Portfolio_Evolution, y = Cashflow_Degiro, by = "Date", all.x = TRUE)
names(Portfolio_Evolution)[4] <- "CashFlow"
Portfolio_Evolution$CashFlow[is.na(Portfolio_Evolution$CashFlow)] <- 0
#Calculations on Portfolio_Evolution...
Portfolio_Evolution$HPR <-round(((Portfolio_Evolution$Portfolio/(shift(Portfolio_Evolution$Portfolio)+Portfolio_Evolution$CashFlow))-1)*100,2) #Holding Period Return
Portfolio_Evolution$CashFlow_Acc <- cumsum(Portfolio_Evolution$CashFlow) #Cumulative CashFlow
Portfolio_Evolution$Earnings_Acc <- Portfolio_Evolution$Portfolio - Portfolio_Evolution$CashFlow_Acc #Cumulative Earnings 
#Portfolio_Evolution$Earnings_Acc_Perc <- round((Portfolio_Evolution$Earnings_Acc / Portfolio_Evolution$CashFlow_Acc)*100,2) #Cumulative Earnings (%)
Portfolio_Evolution$HPR[1] <- 0 
Portfolio_Evolution$HPR_Acc <- cumsum(Portfolio_Evolution$HPR)

# New Feed data connection (https://eodhistoricaldata.com/)  # Control Panel: https://eodhistoricaldata.com/cp/settings
EOD_HD_API <- "YOUR API ID"
tickers <- c('IWDA.AS','XD9U.XETRA','STOXX.INDX','XQUI.MI','TOF.AS','F703.XETRA')
first_date = as.Date('2018-01-02')   #You can use another date
last_date = Sys.Date() - 1

# Links to EOD API (One ticker download at the time, maximum 20 per day)
EOD_link <- vector()
for (a in 1:length(tickers)){
  EOD_link[a] <- paste0('https://eodhistoricaldata.com/api/eod/',tickers[a],'?from=',first_date,'&to=',last_date,'&api_token=',EOD_HD_API,'&period=d&order=a')
}
rm(a)

# Function to download and adapt new feed to old feed
Adapt_data <- function(df,position){
  df <- data.frame(fread(EOD_link[position]))[,c(1,6)]
  df <- data.frame(cbind(df,tickers[position]))
  df <- df[,c(1,3,2)]
  df <- df[!df$Adjusted_close == 0,]
  df$Adjusted_close <- as.numeric(df$Adjusted_close) 
  df[,4] <- ((df$Adjusted_close/shift(df$Adjusted_close))-1) 
  df$Date <- as.Date(df$Date) 
  return(df)
}

WORLD_Dev <- Adapt_data(WORLD_Dev,1) #MSCI World Index ETF
USA <- Adapt_data(USA,2)              #USA ETF
STOXX600 <- Adapt_data(STOXX600,3)     #STOXX600 Index
Xtrackers <- Adapt_data(Xtrackers,4)     #The Portfolio index tracks a globally diversified portfolio consisting of equities and bond indices. The tactical allocation may change up to 8 times per year. Equity share: minimum 30%, maximum 70%. Bond share: minimum 30%, maximum 70%.
VanEck_Off <- Adapt_data(VanEck_Off,5)     #The aim of the VanEck  Multi-Asset Growth Allocation UCITS ETF is to follow the Multi-Asset Growth Allocation Index as closely as possible. This is a composite index made up in the ratios indicated here: - 60% Solactive Global Equity Index - 10% GPR Global 100 Index - 15% Markit iBoxx EUR Liquid Corporates Index - 15% Markit iBoxx EUR Liquid Sovereign Diversified 1-10 Index
ComStage_Off <- Adapt_data(ComStage_Off,6)  #The ComStage  Offensiv index tracks a diversified ETF portfolio. The initial allocation is made up of the following asset classes: 80% of global equities spread across geographies and across sectors, 10% of bonds and 10% commodities. Annually, the index is rebalanced.

names(WORLD_Dev) <- c("Date","Ticker","WORLD_Dev_Price","WORLD_Dev_Change")
names(USA) <- c("Date","Ticker","USA_Price","USA_Change")
names(STOXX600) <- c("Date","Ticker","STOXX600_Price","STOXX600_Change")
names(Xtrackers) <- c("Date","Ticker","Xtrackers_Price","Xtrackers_Change")
names(VanEck_Off) <- c("Date","Ticker","VanEck_Off_Price","VanEck_Off_Change")
names(ComStage_Off) <- c("Date","Ticker","ComStage_Off_Price","ComStage_Off_Change")

Portfolio_Evolution <- Reduce(function(x, y) merge(x, y, by="Date", all=TRUE), list(Portfolio_Evolution,WORLD_Dev,USA,STOXX600,Xtrackers,VanEck_Off,ComStage_Off))

rm(degiro_csv_link,degiro_csv_toDate,l.out,WORLD_Dev,Xtrackers,USA,STOXX600,VanEck_Off,ComStage_Off) #Delete Data Frames that I'm not going to use anymore

#Last makeup...
Portfolio_Evolution <- Portfolio_Evolution[c(1:7,10,13,16,19,22,25,9,12,15,18,21,24)]

Portfolio_Evolution <- cbind(Portfolio_Evolution[c(1:7)],round(Portfolio_Evolution[,8:13]*100,2),round(Portfolio_Evolution[,14:19],2))
Portfolio_Evolution <- Portfolio_Evolution[!chron::is.weekend(as.Date(Portfolio_Evolution$Date, "%d/%m/%Y")), ]
names(Portfolio_Evolution)[8:13] <- c("WORLD_Dev_Change","USA_Change","STOXX600_Change","Xtrackers_Change","VanEck_Off_Change","ComStage_Off_Change")
names(Portfolio_Evolution)[14:19] <- c("WORLD_Dev_Price","USA_Price","STOXX600_Price","Xtrackers_Price","VanEck_Off_Price","ComStage_Off_Price")

#Plotting:
last_degiro_deposit <- as.Date(Cashflow_Degiro[nrow(Cashflow_Degiro),1])
last_x_days <- as.numeric(Sys.Date() - last_degiro_deposit) #Since how many days ago I'm going plot evolution graph
plot_startdate <- Sys.Date() - last_x_days

# Cumulative Change
Evo_Comp <- (Portfolio_Evolution[-1,c(1,2,14:19)])
Evo_Comp <- xts(Evo_Comp[,-1], order.by=Evo_Comp[,1])
Evo_Comp_filter <- Evo_Comp[index(Evo_Comp) > last_degiro_deposit]
Evo_Comp_return = apply(Evo_Comp_filter, 1, function(x) {round(((x / Evo_Comp_filter[1,])-1)*100)}) %>% 
  t %>% as.xts
Evo_Comp_return <- as.data.table(Evo_Comp_return)
Evo_Comp_return$index <- as.Date(Evo_Comp_return$index)
names(Evo_Comp_return)[1] <- "Date"
Cum_Plot <- ggplot(Evo_Comp_return,aes(Date)) +
  geom_line(aes(y = Portfolio, colour = "0_My Portfolio"),size=1.5) + 
  geom_line(aes(y = WORLD_Dev_Price, colour = "WORLD_Dev"),size=0.4) +         
  geom_line(aes(y = USA_Price, colour = "USA"),size=0.4) + 
  geom_line(aes(y = STOXX600_Price, colour = "STOXX600"),size=0.4) +
  geom_line(aes(y = Xtrackers_Price, colour = "Xtrackers"),size=0.4) + 
  geom_line(aes(y = VanEck_Off_Price, colour = "VanEck_Off"),size=0.4) +
  geom_line(aes(y = ComStage_Off_Price, colour = "ComStage_Off"),size=0.4) +    
  theme_ft_rc(plot_title_size = 14,axis_title_face = "bold",axis_title_just = "m",axis_title_size = 10)+
  ggtitle("Cumulative Change since last Degiro deposit")+
  labs(y="Change (%)")+
  labs(colour = "Portfolios and Indexes") +
  xlim(as.Date(c(last_degiro_deposit, Sys.Date()), format="%d/%m/%Y"))
#        ylim(-10,10))

Daily_Plot <- ggplot(Portfolio_Evolution,aes(Date)) +
  geom_line(aes(y = HPR, colour = "0_My Portfolio"),size=1.5) + 
  geom_line(aes(y = WORLD_Dev_Change, colour = "WORLD_Dev"),size=0.4) + 
  geom_line(aes(y = USA_Change, colour = "USA"),size=0.4) + 
  geom_line(aes(y = STOXX600_Change, colour = "STOXX600"),size=0.4) +
  geom_line(aes(y = Xtrackers_Change, colour = "Xtrackers"),size=0.4) + 
  geom_line(aes(y = VanEck_Off_Change, colour = "VanEck_Off"),size=0.4) +
  geom_line(aes(y = ComStage_Off_Change, colour = "ComStage_Off"),size=0.4) +    
  theme_ft_rc(plot_title_size = 14,axis_title_face = "bold",axis_title_just = "m",axis_title_size = 10)+
  ggtitle("Daily Changes")+
  labs(y="Change (%)")+
  labs(colour = "Portfolios and Indexes") +
  xlim(as.Date(c(plot_startdate, Sys.Date()), format="%d/%m/%Y")) 
#  ylim(-5,5)

dev.new()
multiplot(Cum_Plot,Daily_Plot)


#Preparing Portfolio vs Benchmarks evolution report to present on the Console:
Days <- list(7,14,30,45,90,180,365) #Different time frames to show on the report
rm(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp)
Portfolio_vs_Benchmarks <- data.frame(Days = as.character(), Portfolio = as.numeric(), WORLD_Dev_Change = as.numeric(), USA_Change = as.numeric(), STOXX600_Change = as.numeric(), Xtrackers_Change = as.numeric(), VanEck_Off_Change = as.numeric(), ComStage_Off_Change = as.numeric())

#Last Day:
Portfolio_vs_Benchmarks_Temp <- data.frame(Portfolio=as.numeric()) #Temporary Data Frame
Portfolio_vs_Benchmarks_Temp[1,] <- 0
Portfolio_vs_Benchmarks_Temp$Portfolio <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(3)]

#Check if I have last day values
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(8)])) {
  Portfolio_vs_Benchmarks_Temp$WORLD_Dev_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(8)]
} else {
  Portfolio_vs_Benchmarks_Temp$WORLD_Dev_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(9)])) {
  Portfolio_vs_Benchmarks_Temp$USA_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(9)]
} else {
  Portfolio_vs_Benchmarks_Temp$USA_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(10)])) {
  Portfolio_vs_Benchmarks_Temp$STOXX600_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(10)]
} else {
  Portfolio_vs_Benchmarks_Temp$STOXX600_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(11)])) {
  Portfolio_vs_Benchmarks_Temp$Xtrackers_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(11)]
} else {
  Portfolio_vs_Benchmarks_Temp$Xtrackers_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(12)])) {
  Portfolio_vs_Benchmarks_Temp$VanEck_Off_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(12)]
} else {
  Portfolio_vs_Benchmarks_Temp$VanEck_Off_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(13)])) {
  Portfolio_vs_Benchmarks_Temp$ComStage_Off_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(13)]
} else {
  Portfolio_vs_Benchmarks_Temp$ComStage_Off_Change <- 'S/D'}

Portfolio_vs_Benchmarks_Temp$Days <- "Last"
Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp[,c(8,1:7)] 
Portfolio_vs_Benchmarks <- data.frame(rbind(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp,use.names=FALSE))
Portfolio_vs_Benchmarks <- Portfolio_vs_Benchmarks[-nrow(Portfolio_vs_Benchmarks),]

#More time frames:
for (i in Days){
  #i = 7
  Portfolio_vs_Benchmarks_Temp <- subset(Portfolio_Evolution[c(1,3,8:13)],Date>=Sys.Date()-i)
  names(Portfolio_vs_Benchmarks_Temp)[2] <- "Portfolio"
  Portfolio_vs_Benchmarks_Temp <- data.frame(t(colSums(Portfolio_vs_Benchmarks_Temp[,-1],na.rm = TRUE)))
  Portfolio_vs_Benchmarks_Temp$Days <- i
  Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp[,c(8,1:7)] 
  Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp %>% mutate_at(vars(-Days), funs(round(., 2)))
  Portfolio_vs_Benchmarks <- data.frame(rbind(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp,use.names=FALSE))
  Portfolio_vs_Benchmarks <- Portfolio_vs_Benchmarks[-nrow(Portfolio_vs_Benchmarks),]
}

#YTD:
Portfolio_vs_Benchmarks_Temp <- subset(Portfolio_Evolution[c(1,3,8:13)],Date>=as.Date(format(Sys.Date(), "%Y-01-01")))
names(Portfolio_vs_Benchmarks_Temp)[2] <- "Portfolio"
Portfolio_vs_Benchmarks_Temp <- data.frame(t(colSums(Portfolio_vs_Benchmarks_Temp[,-1],na.rm = TRUE)))
Portfolio_vs_Benchmarks_Temp$Days <- "YTD"
Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp[,c(8,1:7)] 
Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp %>% mutate_at(vars(-Days), funs(round(., 2)))
Portfolio_vs_Benchmarks <- data.frame(rbind(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp,use.names=FALSE))
Portfolio_vs_Benchmarks <- Portfolio_vs_Benchmarks[-nrow(Portfolio_vs_Benchmarks),]

#Whole period:
Portfolio_vs_Benchmarks_Temp <- data.frame(t(colSums(Portfolio_Evolution[,c(3,8:13)],na.rm = TRUE)))
names(Portfolio_vs_Benchmarks_Temp)[1] <- "Portfolio"
Portfolio_vs_Benchmarks_Temp$Days <- "Whole Period"
Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp[,c(8,1:7)] 
Portfolio_vs_Benchmarks_Temp <- Portfolio_vs_Benchmarks_Temp %>% mutate_at(vars(-Days), funs(round(., 2)))
Portfolio_vs_Benchmarks <- data.frame(rbind(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp,use.names=FALSE))
Portfolio_vs_Benchmarks <- Portfolio_vs_Benchmarks[-nrow(Portfolio_vs_Benchmarks),]
names(Portfolio_vs_Benchmarks) <- c("Days","Portfolio","WORLD_Dev","USA","STOXX600","Xtrackers","VanEck_Off","ComStage_Off")
rm(Days,i,Portfolio_vs_Benchmarks_Temp,Evo_Comp,Evo_Comp_filter)
#cat("\f")
#cat (paste0("Evolution during last n days: "))
#print.data.frame(Portfolio_vs_Benchmarks,quote=FALSE,row.names = FALSE)
#print.data.frame(Degiro_acc_stat,quote=FALSE,row.names = FALSE)
view(Portfolio_vs_Benchmarks,title = "Evolution")
view(Degiro_acc_stat, title = 'Account Statement - One Week')

save.image("Portfolio Evolution.RData")

packrat::clean()