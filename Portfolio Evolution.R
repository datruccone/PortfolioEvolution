library(packrat)
#library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library(dplyr)
library(BatchGetSymbols)
library("ggplot2")
library(grDevices)

#Variables names are snake_case underscore and Data Frame names are snake_case with first letters in capital
#Data Frames Summary:
#Portfolio_Daily: keeps records as are imported from Degiro
#Portfolio_Evolution: shows the changes per day of the Portfolio and main indexes (or ETFs) to benchmark. Weekends are excluded
#Portfolio_vs_Benchmarks: Portfolio vs Benchmarks on different time frames
#Cashflow: keeps records of deposits and withdrawals from/to your Investment account/s. Each movement must be added manually to this data frame


setwd('c:/Path/to/your/wd')
cat("\f")  

#IMPORT Degiro PORTFOLIO CSV
#Copy the link to Degiro CSV Portfolio Export and run the script. IMPORTANT: you must be logged in Degiro.

degiro_csv_link <- readClipboard()
degiro_csv_toDate <- as.integer(gregexpr("toDate" ,degiro_csv_link))
degiro_csv_date <- as.Date(paste0(substr(degiro_csv_link,degiro_csv_toDate + 17, degiro_csv_toDate + 21),"-",substr(degiro_csv_link,degiro_csv_toDate + 12, degiro_csv_toDate + 13),"-",substr(degiro_csv_link,degiro_csv_toDate + 7, degiro_csv_toDate + 8)))

'%!in%' <- function(x,y)!('%in%'(x,y))

#Check if Degiro CSV's hasn't been already imported to our Portfolio Daily dataframe...
if (degiro_csv_date %!in% Portfolio_Daily$Date) 
{
  #...if not, start with CSV file import 
  CSV_Degiro <- fread(degiro_csv_link)
  CSV_Degiro$Date <- degiro_csv_date
  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
  #Append to Portfolio Daily history
  Portfolio_Daily <- rbind(Portfolio_Daily,CSV_Degiro,use.names=FALSE)
  Portfolio_Daily$Date <- as.Date(Portfolio_Daily$Date)
  Portfolio_Daily <- Portfolio_Daily[order(-Portfolio_Daily$Date),]
  #Save updated Portfolio Daily to local drive in Excel format (work Directory)
  write.xlsx2 (Portfolio_Daily, "Portfolio_Daily.xlsx", sheetName = "Degiro",col.names = TRUE, row.names = FALSE, append = FALSE)
  #Delete CSV Data Frame
  rm(CSV_Degiro)
  #END IMPORT
}

#Workarround: For some unknown reason if I don't reload the Portfolio Daily DF from Excel file the following aggregate doesn't work.
Portfolio_Daily <- data.frame(read.xlsx('Portfolio_Daily.xlsx',sheetName = "Degiro"))
Portfolio_Daily$Date <- as.Date(Portfolio_Daily$Date)
#Add imported day Portfolio value to Portfolio Evolution 
Portfolio_Evolution <- data.frame(aggregate(Portfolio_Daily[7],list(Portfolio_Daily$Date),sum))
names(Portfolio_Evolution)[1:2] <- c("Date","Portfolio")
Portfolio_Evolution$Date <- as.Date(Portfolio_Evolution$Date)
Portfolio_Evolution$HPR <- 0 #Add Holding Return Period column, which is basically the Portfolio gains/losses minus deposits and withdrawals
#Search for movements in Degiro Account   
Cashflow_Degiro <- data.frame(subset(Cashflow[c(1,3)],Cashflow$Account == "Degiro"))
Cashflow_Degiro$Date <- as.Date(Cashflow_Degiro$Date)
Portfolio_Evolution <- merge(x = Portfolio_Evolution, y = Cashflow_Degiro, by = "Date", all.x = TRUE)
names(Portfolio_Evolution)[4] <- "CashFlow"
Portfolio_Evolution$CashFlow[is.na(Portfolio_Evolution$CashFlow)] <- 0
#Calculations on Portfolio_Evolution...
Portfolio_Evolution$HPR <-round(((Portfolio_Evolution$Portfolio/(shift(Portfolio_Evolution$Portfolio)+Portfolio_Evolution$CashFlow))-1)*100,2) #Holding Period Return
Portfolio_Evolution$CashFlow_Acc <- cumsum(Portfolio_Evolution$CashFlow) #Cumulative CashFlow
Portfolio_Evolution$Earnings_Acc <- Portfolio_Evolution$Portfolio - Portfolio_Evolution$CashFlow_Acc #Cumulative Earnings 
Portfolio_Evolution$Earnings_Acc_Perc <- round((Portfolio_Evolution$Earnings_Acc / Portfolio_Evolution$CashFlow_Acc)*100,2) #Cumulative Earnings (%)

#Indexes Import, this piece of code is based on: https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
l.out <- BatchGetSymbols(tickers = c('WLD.PA','IUES.AS','^STOXX50E','XQUI.MI','TOF.AS','F703.DE'), 
                         first.date = '2018-7-24',   #You can use another date
                         last.date = Sys.Date(), 
                         freq.data = 'daily',
                         cache.folder = file.path(tempdir(), 
                                                  'BGS_Cache') ) # cache in tempdir()

#Filtering and cleaning Markets data:
Indexes <- l.out$df.tickers
WORLD_Dev <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "WLD.PA"))      #MSCI World Index ETF in EUR (because the Portfolio is EUR, so I'm matching same currencies)
SP500 <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "IUES.AS"))         #SP500 ETF in EUR (because the Portfolio is EUR, so I'm matching same currencies)
STOXX50 <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "^STOXX50E"))     #STOXX50 Index
Xtrackers <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "XQUI.MI"))     #The Portfolio index tracks a globally diversified portfolio consisting of equities and bond indices. The tactical allocation may change up to 8 times per year. Equity share: minimum 30%, maximum 70%. Bond share: minimum 30%, maximum 70%.
VanEck_Off <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "TOF.AS"))     #The aim of the VanEck Vectors™ Multi-Asset Growth Allocation UCITS ETF is to follow the Multi-Asset Growth Allocation Index as closely as possible. This is a composite index made up in the ratios indicated here: - 60% Solactive Global Equity Index - 10% GPR Global 100 Index - 15% Markit iBoxx EUR Liquid Corporates Index - 15% Markit iBoxx EUR Liquid Sovereign Diversified 1-10 Index
ComStage_Off <- data.frame(subset(l.out$df.tickers[7:9],l.out$df.tickers$ticker == "F703.DE"))  #The ComStage Vermögensstrategie Offensiv index tracks a diversified ETF portfolio. The initial allocation is made up of the following asset classes: 80% of global equities spread across geographies and across sectors, 10% of bonds and 10% commodities. Annually, the index is rebalanced.

names(WORLD_Dev)[1:3] <- c("Date","Ticker","WORLD_Dev")
names(SP500)[1:3] <- c("Date","Ticker","SP500")
names(STOXX50)[1:3] <- c("Date","Ticker","STOXX50")
names(Xtrackers)[1:3] <- c("Date","Ticker","Xtrackers")
names(VanEck_Off)[1:3] <- c("Date","Ticker","VanEck_Off")
names(ComStage_Off)[1:3] <- c("Date","Ticker","ComStage_Off")

Portfolio_Evolution <- Reduce(function(x, y) merge(x, y, by="Date", all=TRUE), list(Portfolio_Evolution,WORLD_Dev,SP500,STOXX50,Xtrackers,VanEck_Off,ComStage_Off))

rm(degiro_csv_link,degiro_csv_toDate,l.out,WORLD_Dev,Xtrackers,SP500,STOXX50,VanEck_Off,ComStage_Off) #Delete Data Frames that I'm not going to use anymore

#Last makeup...
Portfolio_Evolution <- Portfolio_Evolution[c(1:7,9,11,13,15,17,19)]
Portfolio_Evolution <- cbind(Portfolio_Evolution[c(1:7)],round(Portfolio_Evolution[,8:13]*100,2))
Portfolio_Evolution <- Portfolio_Evolution[!chron::is.weekend(as.Date(Portfolio_Evolution$Date, "%d/%m/%Y")), ]
names(Portfolio_Evolution)[8:13] <- c("WORLD_Dev","SP500","STOXX50","Xtrackers","VanEck_Off","ComStage_Off")

#Plotting:
last_x_days <- 15 #Since how many days ago I'm going plot evolution graph
plot_startdate <- Sys.Date() - last_x_days
print(ggplot(Portfolio_Evolution, aes(Date)) + 
        theme_dark() +
        theme(plot.background = element_rect(fill = "#BFD5E3"))+
        ggtitle("Daily Changes")+
        labs(y="Change (%)")+
        labs(colour = "Portfolios and Indexes") +
        geom_line(aes(y = Portfolio_Evolution$HPR, colour = "0_My Portfolio"),size=1.5) + 
        geom_line(aes(y = Portfolio_Evolution$WORLD_Dev, colour = "WORLD_Dev"),size=0.4) + 
        geom_line(aes(y = Portfolio_Evolution$SP500, colour = "SP500"),size=0.4) + 
        geom_line(aes(y = Portfolio_Evolution$STOXX50, colour = "STOXX50"),size=0.4) +
        geom_line(aes(y = Portfolio_Evolution$Xtrackers, colour = "Xtrackers"),size=0.4) + 
        geom_line(aes(y = Portfolio_Evolution$VanEck_Off, colour = "VanEck_Off"),size=0.4) +
        geom_line(aes(y = Portfolio_Evolution$ComStage_Off, colour = "ComStage_Off"),size=0.4) +    
        #  xlim(as.Date(c(plot_startdate, plot_enddate), format="%d/%m/%Y")) +
        ylim(-2.5,2.5)+
        scale_x_date(date_labels="%e/%m",date_breaks  ="1 day",limits = c(plot_startdate, NA)))

#Preparing Portfolio vs Benchmarks evolution report to present on the Console:
Days <- list(7,14,30,45,90,180,365) #Different time frames to show on the report
rm(Portfolio_vs_Benchmarks,Portfolio_vs_Benchmarks_Temp)
Portfolio_vs_Benchmarks <- data.frame(Days = as.character(), Portfolio = as.numeric(), WORLD_Dev = as.numeric(), SP500 = as.numeric(), STOXX50 = as.numeric(), Xtrackers = as.numeric(), VanEck_Off = as.numeric(), ComStage_Off = as.numeric())

#Last Day:
Portfolio_vs_Benchmarks_Temp <- data.frame(Portfolio=as.numeric()) #Temporary Data Frame
Portfolio_vs_Benchmarks_Temp[1,] <- 0
Portfolio_vs_Benchmarks_Temp$Portfolio <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(3)]

#Check if I have last day values
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(8)])) {
  Portfolio_vs_Benchmarks_Temp$WORLD_Dev <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(8)]
} else {
  Portfolio_vs_Benchmarks_Temp$WORLD_Dev <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(9)])) {
  Portfolio_vs_Benchmarks_Temp$SP500 <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(9)]
} else {
  Portfolio_vs_Benchmarks_Temp$SP500 <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(10)])) {
  Portfolio_vs_Benchmarks_Temp$STOXX50 <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(10)]
} else {
  Portfolio_vs_Benchmarks_Temp$STOXX50 <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(11)])) {
  Portfolio_vs_Benchmarks_Temp$Xtrackers <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(11)]
} else {
  Portfolio_vs_Benchmarks_Temp$Xtrackers <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(12)])) {
  Portfolio_vs_Benchmarks_Temp$VanEck_Off <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(12)]
} else {
  Portfolio_vs_Benchmarks_Temp$VanEck_Off <- 'S/D'}
if (!is.na(Portfolio_Evolution[nrow(Portfolio_Evolution)-1,c(13)])) {
  Portfolio_vs_Benchmarks_Temp$ComStage_Off <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(13)]
} else {
  Portfolio_vs_Benchmarks_Temp$ComStage_Off <- 'S/D'}

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

rm(Days,i,Portfolio_vs_Benchmarks_Temp)
#cat("\f")
cat (paste0("Evolution during last n days: "))
print.data.frame(Portfolio_vs_Benchmarks,quote=FALSE,row.names = FALSE)
#view(Portfolio_vs_Benchmarks,"Evolution")

save.image()

packrat::clean()