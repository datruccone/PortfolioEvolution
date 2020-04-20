library(packrat)
#library(quantmod)
library(tidyverse)
library(data.table)
#library("xlsx")
library(dplyr)
library(BatchGetSymbols)
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


setwd('c:/Path/to/your/Workspace/')
#load("Portfolio Evolution.RData")

commarep <- function(c) {sub(",",".",c)}
'%!in%' <- function(x,y)!('%in%'(x,y))

cat("\f")  

#IMPORT Degiro PORTFOLIO CSV
#Copy the link to Degiro CSV Portfolio Export and run the script. IMPORTANT: you must be logged in Degiro.

# For batch import of several days:

#degiro_csv_link <- readline(prompt="Paste Degiro's CSV link: ")  
#degiro_csv_fromDate <- readline(prompt="Enter From Date in YYYY-MM-DD Format: ") 
#degiro_csv_toDate <- readline(prompt="Enter To Date in YYYY-MM-DD Format: ") 
#days = as.integer(as.Date(degiro_csv_toDate) - as.Date(degiro_csv_fromDate))+1
#Portfolio_Daily <- data.frame(Date = as.character(), Product = as.character(), ISIN = as.character(), Amount = as.numeric(), Closing = as.numeric(), Localvalue = as.character(), ValueinEUR = as.character())
#degiro_csv_date <- degiro_csv_fromDate
#for (i in 1:days){ 
#  degiro_csv_link <- paste0(substr(degiro_csv_link,1, nchar(degiro_csv_link)-14),substr(degiro_csv_date,9, 10),"%2F",substr(degiro_csv_date,6, 7),"%2F",substr(degiro_csv_date,1, 4))
#  CSV_Degiro <- fread(degiro_csv_link)
#  CSV_Degiro$Date <- degiro_csv_date
#  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
#  #Append to Portfolio Daily history
#  Portfolio_Daily <- rbind(Portfolio_Daily,CSV_Degiro,use.names=FALSE)
#  degiro_csv_date <- as.character(as.Date(degiro_csv_date) %m+% days(1))
#  #Delete CSV Data Frame
#  rm(CSV_Degiro)
#}
#END BATCH IMPORT
###################

# One Day import:
degiro_csv_link <- readClipboard()
degiro_csv_toDate <- as.integer(gregexpr("toDate" ,degiro_csv_link))
degiro_csv_date <- as.Date(paste0(substr(degiro_csv_link,degiro_csv_toDate + 17, degiro_csv_toDate + 21),"-",substr(degiro_csv_link,degiro_csv_toDate + 12, degiro_csv_toDate + 13),"-",substr(degiro_csv_link,degiro_csv_toDate + 7, degiro_csv_toDate + 8)))

#Check if Degiro CSV's hasn't been already imported to our Portfolio Daily dataframe...
if (degiro_csv_date %!in% Portfolio_Daily$Date) 
{
  #...if not, start with CSV file import 
  CSV_Degiro <- fread(degiro_csv_link)
  CSV_Degiro$Date <- degiro_csv_date
  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
  names(CSV_Degiro) <- c("Date","Product","ISIN","Amount","Closing","Localvalue","ValueinEUR")
  #Append to Portfolio Daily history
  Portfolio_Daily <- read_excel("Portfolio_Daily.xlsx")
  Portfolio_Daily$Date <- as.Date(Portfolio_Daily$Date,format = "%Y-%m-%d")
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

#Indexes Import, this piece of code is based on: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
l.out <- BatchGetSymbols(tickers = c('WLD.PA','IUES.AS','^STOXX50E','XQUI.MI','TOF.AS','F703.DE'), 
                         first.date = '2018-7-24',   #You can use another date
                         last.date = Sys.Date(), 
                         freq.data = 'daily',
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()

#Filtering and cleaning Markets data:
Indexes <- l.out$df.tickers
WORLD_Dev <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "WLD.PA"))      #MSCI World Index ETF in EUR (because the Portfolio is EUR, so I'm matching same currencies)
WORLD_Dev[,4] <- ((WORLD_Dev$price.adjusted/shift(WORLD_Dev$price.adjusted))-1) 
SP500 <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "IUES.AS"))         #SP500 ETF in EUR (because the Portfolio is EUR, so I'm matching same currencies)
SP500[,4] <- ((SP500$price.adjusted/shift(SP500$price.adjusted))-1) 
STOXX50 <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "^STOXX50E"))     #STOXX50 Index
STOXX50[,4] <- ((STOXX50$price.adjusted/shift(STOXX50$price.adjusted))-1) 
Xtrackers <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "XQUI.MI"))     #The Portfolio index tracks a globally diversified portfolio consisting of equities and bond indices. The tactical allocation may change up to 8 times per year. Equity share: minimum 30%, maximum 70%. Bond share: minimum 30%, maximum 70%.
Xtrackers[,4] <- ((Xtrackers$price.adjusted/shift(Xtrackers$price.adjusted))-1) 
VanEck_Off <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "TOF.AS"))     #The aim of the VanEck Vectors™ Multi-Asset Growth Allocation UCITS ETF is to follow the Multi-Asset Growth Allocation Index as closely as possible. This is a composite index made up in the ratios indicated here: - 60% Solactive Global Equity Index - 10% GPR Global 100 Index - 15% Markit iBoxx EUR Liquid Corporates Index - 15% Markit iBoxx EUR Liquid Sovereign Diversified 1-10 Index
VanEck_Off[,4] <- ((VanEck_Off$price.adjusted/shift(VanEck_Off$price.adjusted))-1) 
ComStage_Off <- data.frame(subset(l.out$df.tickers[c(7,8,6)],l.out$df.tickers$ticker == "F703.DE"))  #The ComStage Vermögensstrategie Offensiv index tracks a diversified ETF portfolio. The initial allocation is made up of the following asset classes: 80% of global equities spread across geographies and across sectors, 10% of bonds and 10% commodities. Annually, the index is rebalanced.
ComStage_Off[,4] <- ((ComStage_Off$price.adjusted/shift(ComStage_Off$price.adjusted))-1) 

names(WORLD_Dev) <- c("Date","Ticker","WORLD_Dev_Price","WORLD_Dev_Change")
names(SP500) <- c("Date","Ticker","SP500_Price","SP500_Change")
names(STOXX50) <- c("Date","Ticker","STOXX50_Price","STOXX50_Change")
names(Xtrackers) <- c("Date","Ticker","Xtrackers_Price","Xtrackers_Change")
names(VanEck_Off) <- c("Date","Ticker","VanEck_Off_Price","VanEck_Off_Change")
names(ComStage_Off) <- c("Date","Ticker","ComStage_Off_Price","ComStage_Off_Change")

Portfolio_Evolution <- Reduce(function(x, y) merge(x, y, by="Date", all=TRUE), list(Portfolio_Evolution,WORLD_Dev,SP500,STOXX50,Xtrackers,VanEck_Off,ComStage_Off))

rm(degiro_csv_link,degiro_csv_toDate,l.out,WORLD_Dev,Xtrackers,SP500,STOXX50,VanEck_Off,ComStage_Off) #Delete Data Frames that I'm not going to use anymore

#Last makeup...
Portfolio_Evolution <- Portfolio_Evolution[c(1:7,10,13,16,19,22,25,9,12,15,18,21,24)]


Portfolio_Evolution <- cbind(Portfolio_Evolution[c(1:7)],round(Portfolio_Evolution[,8:13]*100,2),round(Portfolio_Evolution[,14:19],2))
Portfolio_Evolution <- Portfolio_Evolution[!chron::is.weekend(as.Date(Portfolio_Evolution$Date, "%d/%m/%Y")), ]
names(Portfolio_Evolution)[8:13] <- c("WORLD_Dev_Change","SP500_Change","STOXX50_Change","Xtrackers_Change","VanEck_Off_Change","ComStage_Off_Change")
names(Portfolio_Evolution)[14:19] <- c("WORLD_Dev_Price","SP500_Price","STOXX50_Price","Xtrackers_Price","VanEck_Off_Price","ComStage_Off_Price")

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
  geom_line(aes(y = SP500_Price, colour = "SP500"),size=0.4) + 
  geom_line(aes(y = STOXX50_Price, colour = "STOXX50"),size=0.4) +
  geom_line(aes(y = Xtrackers_Price, colour = "Xtrackers"),size=0.4) + 
  geom_line(aes(y = VanEck_Off_Price, colour = "VanEck_Off"),size=0.4) +
  geom_line(aes(y = ComStage_Off_Price, colour = "ComStage_Off"),size=0.4) +    
  theme_ft_rc(plot_title_size = 14,axis_title_face = "bold",axis_title_just = "m",axis_title_size = 10)+
  ggtitle("Cumulative Change since last Degiro deposit")+
  labs(y="Change (%)")+
  labs(colour = "Portfolios and Indexes") +
  xlim(as.Date(c(last_degiro_deposit, Sys.Date()), format="%d/%m/%Y"))
#        ylim(-10,10))
####

Daily_Plot <- ggplot(Portfolio_Evolution,aes(Date)) +
  geom_line(aes(y = HPR, colour = "0_My Portfolio"),size=1.5) + 
  geom_line(aes(y = WORLD_Dev_Change, colour = "WORLD_Dev"),size=0.4) + 
  geom_line(aes(y = SP500_Change, colour = "SP500"),size=0.4) + 
  geom_line(aes(y = STOXX50_Change, colour = "STOXX50"),size=0.4) +
  geom_line(aes(y = Xtrackers_Change, colour = "Xtrackers"),size=0.4) + 
  geom_line(aes(y = VanEck_Off_Change, colour = "VanEck_Off"),size=0.4) +
  geom_line(aes(y = ComStage_Off_Change, colour = "ComStage_Off"),size=0.4) +    
  theme_ft_rc(plot_title_size = 14,axis_title_face = "bold",axis_title_just = "m",axis_title_size = 10)+
  ggtitle("Daily Changes")+
  labs(y="Change (%)")+
  labs(colour = "Portfolios and Indexes") +
  xlim(as.Date(c(plot_startdate, Sys.Date()), format="%d/%m/%Y")) 
#  ylim(-5,5)

#dev.new()
multiplot(Cum_Plot,Daily_Plot)


#Preparing Portfolio vs Benchmarks evolution report to present on the Console:
Days <- list(7,14,30,45,90,180,365) #Different time frames to show on the report
rm(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp)
Portfolio_vs_Benchmarks <- data.frame(Days = as.character(), Portfolio = as.numeric(), WORLD_Dev_Change = as.numeric(), SP500_Change = as.numeric(), STOXX50_Change = as.numeric(), Xtrackers_Change = as.numeric(), VanEck_Off_Change = as.numeric(), ComStage_Off_Change = as.numeric())

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
  Portfolio_vs_Benchmarks_Temp$SP500_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(9)]
} else {
  Portfolio_vs_Benchmarks_Temp$SP500_Change <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(10)])) {
  Portfolio_vs_Benchmarks_Temp$STOXX50_Change <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(10)]
} else {
  Portfolio_vs_Benchmarks_Temp$STOXX50_Change <- 'S/D'}
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
names(Portfolio_vs_Benchmarks) <- c("Days","Portfolio","WORLD_Dev","SP500","STOXX50","Xtrackers","VanEck_Off","ComStage_Off")
rm(Days,i,Portfolio_vs_Benchmarks_Temp,Evo_Comp,Evo_Comp_filter)
#cat("\f")
cat (paste0("Evolution during last n days: "))
print.data.frame(Portfolio_vs_Benchmarks,quote=FALSE,row.names = FALSE)
#view(Portfolio_vs_Benchmarks,"Evolution")

save.image("Portfolio Evolution.RData")

packrat::clean()