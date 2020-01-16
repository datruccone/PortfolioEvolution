library(packrat)
library(quantmod)
library(tidyverse)
library(data.table)
library("xlsx")
library(dplyr)

#Depositios en Degiro desde el 24/07/2018 al 08/01/2020:
Depositos <- 86700

setwd('c:/Users/David/Documents/GitHub/Acciones/Portfolio Evolution/')

# Portfolio_Daily <- data.frame(read_xlsx('Portfolio_Daily.xlsx'))
# Portfolio_Daily$Fecha <- as.Date(Portfolio_Daily$Fecha) 
cat("\f")  

#IMPORT DEGIRO PORTFOLIO CSV
#Enlace al archivo CSV de Degiro...

CSV_Link <- readClipboard()  #IMPORTANTE: hay que estar logueado a DeGiro
inicio <- as.integer(gregexpr("toDate" ,CSV_Link))
CSV_Degiro_Fecha <- as.Date(paste0(substr(CSV_Link,inicio + 17, inicio + 21),"-",substr(CSV_Link,inicio + 12, inicio + 13),"-",substr(CSV_Link,inicio + 7, inicio + 8)))

'%!in%' <- function(x,y)!('%in%'(x,y))
if (CSV_Degiro_Fecha %!in% Portfolio_Daily$Fecha) 
{
  CSV_Degiro <- fread(CSV_Link)
  CSV_Degiro$Fecha <- CSV_Degiro_Fecha
  CSV_Degiro <- CSV_Degiro[,c(7,1:6)]
  Portfolio_Daily <- rbind(Portfolio_Daily,CSV_Degiro,use.names=FALSE)
  Portfolio_Daily$Fecha <- as.Date(Portfolio_Daily$Fecha)
  Portfolio_Daily <- Portfolio_Daily[order(-Portfolio_Daily$Fecha),]
  write.xlsx2 (Portfolio_Daily, "Portfolio_Daily.xlsx", sheetName = "DeGiro",col.names = TRUE, row.names = FALSE, append = FALSE)
  rm(CSV_Degiro)
  #FIN IMPORT
}
#No se porque mierda si no haga este truquito de recargar la tabla desde excel com el read.xlsx 1 despues el aggregate no funciona  
Portfolio_Daily <- data.frame(read.xlsx('Portfolio_Daily.xlsx',sheetName = "DeGiro"))
Portfolio_Daily$Fecha <- as.Date(Portfolio_Daily$Fecha)

Portfolio_Evolution <- data.frame(aggregate(Portfolio_Daily[7],list(Portfolio_Daily$Fecha),sum))
names(Portfolio_Evolution)[1:2] <- c("Fecha","Portfolio")
Portfolio_Evolution$Portfolio_Change <- round(Delt(Portfolio_Evolution$Portfolio,type='arithmetic')*100,2)
Creditos_Degiro <- data.frame(subset(Movimientos[c(1,3)],Movimientos$Cuenta == "DeGiro"))
Portfolio_Evolution <- merge(x = Portfolio_Evolution, y = Creditos_Degiro, by = "Fecha", all.x = TRUE)
Portfolio_Evolution$Credito[is.na(Portfolio_Evolution$Credito)] <- 0
Portfolio_Evolution$Credito_Acc <- cumsum(Portfolio_Evolution$Credito)
Portfolio_Evolution$Ganancia_Acc <- Portfolio_Evolution$Portfolio - Portfolio_Evolution$Credito_Acc
Portfolio_Evolution$Ganancia_Acc_Perc <- round((Portfolio_Evolution$Ganancia_Acc / Portfolio_Evolution$Credito_Acc)*100,2)
Portfolio_Evolution$Ganancia_Acc_Change <- round(Delt(Portfolio_Evolution$Ganancia_Acc,type='arithmetic')*100,2)
Portfolio_Evolution$Ganancia_Acc_Change[1:2] <- 0


#https://cran.r-project.org/web/packages/BatchGetSymbols/vignettes/BatchGetSymbols-vignette.html
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
library(BatchGetSymbols)
  
l.out <- BatchGetSymbols(tickers = c('ACWI','ACWX','^RUT','^GSPC','^GDAXI','^STOXX50E'), 
                           first.date = '2018-7-24',
                           last.date = Sys.Date(), 
                           freq.data = 'daily',
                           cache.folder = file.path(tempdir(), 
                                                    'BGS_Cache') ) # cache in tempdir()
Indices <- l.out$df.tickers
  
ACWI <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "ACWI"))
ACWX <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "ACWX"))
RUT <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "^RUT"))
GSPC <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "^GSPC"))
GDAXI <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "^GDAXI"))
STOXX50E <- data.frame(subset(l.out$df.tickers,l.out$df.tickers$ticker == "^STOXX50E"))
  
names(ACWI)[7:9] <- c("Fecha","Ticker","ACWI")
names(ACWX)[7:9] <- c("Fecha","Ticker","ACWX")
names(RUT)[7:9] <- c("Fecha","Ticker","RUT")
names(GSPC)[7:9] <- c("Fecha","Ticker","GSPC")
names(GDAXI)[7:9] <- c("Fecha","Ticker","GDAXI")
names(STOXX50E)[7:9] <- c("Fecha","Ticker","STOXX50E")
  
Portfolio_Evolution <- Reduce(function(x, y) merge(x, y, by="Fecha", all=TRUE), list(Portfolio_Evolution,ACWI,ACWX,GSPC,STOXX50E,GDAXI,RUT))
rm(l.out,ACWI,ACWX,RUT,GSPC,GDAXI,STOXX50E)

Portfolio_Evolution <- Portfolio_Evolution[c(1:8,16,25,34,43,52,61)]
Portfolio_Evolution <- cbind(Portfolio_Evolution[c(1:8)],round(Portfolio_Evolution[,9:14]*100,2))
Portfolio_Evolution <- Portfolio_Evolution[!chron::is.weekend(as.Date(Portfolio_Evolution$Fecha, "%d/%m/%Y")), ]

LastxDays <- 15

rm(CSV_Link,inicio)

library("ggplot2")
library(grDevices)

dev.new(4)
dev.set(4)

plot_startdate <- Sys.Date() - LastxDays
print(ggplot(Portfolio_Evolution, aes(Fecha)) + 
  theme_dark() +
  theme(plot.background = element_rect(fill = "#BFD5E3"))+
  geom_line(aes(y = `Portfolio_Change`, colour = "Portfolio"),size=1.5) + 
  geom_line(aes(y = `ACWI`, colour = "ACWI"),size=0.4) + 
  geom_line(aes(y = `ACWX`, colour = "ACWX"),size=0.4) + 
  geom_line(aes(y = `GSPC`, colour = "SP500"),size=0.4) + 
  geom_line(aes(y = `STOXX50E`, colour = "STOXX50"),size=0.4) +
  geom_line(aes(y = `GDAXI`, colour = "DAX"),size=0.4) +
  geom_line(aes(y = `RUT`, colour = "RUS2000"),size=0.4) +
#  xlim(as.Date(c(plot_startdate, plot_enddate), format="%d/%m/%Y")) +
  ylim(-2.5,2.5)+
  scale_x_date(date_labels="%e/%m",date_breaks  ="1 day",limits = c(plot_startdate, NA)))


Days <- list(7,14,30,45,90)
rm(Portfolio_Sum,Portfolio_Sum.temp)
Portfolio_Sum <- data.frame(Days = as.character(), Change = as.numeric(), ACWI = as.numeric(), ACWX = as.numeric(), GSPC = as.numeric(), STOXX50E = as.numeric(), GDAXI = as.numeric(), RUT = as.numeric())

#Last Day:
Portfolio_Sum.temp <- Portfolio_Evolution[nrow(Portfolio_Evolution),c(3,9:14)]
Portfolio_Sum.temp$Days <- "Last"
Portfolio_Sum.temp <- Portfolio_Sum.temp[,c(8,1:7)] 
Portfolio_Sum <- data.frame(rbind(Portfolio_Sum,Portfolio_Sum.temp,use.names=FALSE))
Portfolio_Sum <- Portfolio_Sum[-nrow(Portfolio_Sum),]

#Rest:
for (i in Days){
#i = 7
  Portfolio_Sum.temp <- subset(Portfolio_Evolution[c(1,3,9:14)],Fecha>=Sys.Date()-i)
  Portfolio_Sum.temp <- data.frame(t(colSums(Portfolio_Sum.temp[,-1],na.rm = TRUE)))
  Portfolio_Sum.temp$Days <- i
  Portfolio_Sum.temp <- Portfolio_Sum.temp[,c(8,1:7)] 
  Portfolio_Sum <- data.frame(rbind(Portfolio_Sum,Portfolio_Sum.temp,use.names=FALSE))
  Portfolio_Sum <- Portfolio_Sum[-nrow(Portfolio_Sum),]
}

#Since First Degiro Day (24/07/2018):
Portfolio_Sum.temp <- data.frame(t(colSums(Portfolio_Evolution[,c(-1:-2,-4:-8)],na.rm = TRUE)))
Portfolio_Sum.temp$Days <- "All"
Portfolio_Sum.temp$Portfolio_Change <- round(((Portfolio_Evolution[nrow(Portfolio_Evolution),2] / Depositos)-1)*100,2)
Portfolio_Sum.temp <- Portfolio_Sum.temp[,c(8,1:7)] 
Portfolio_Sum <- data.frame(rbind(Portfolio_Sum,Portfolio_Sum.temp,use.names=FALSE))
Portfolio_Sum <- Portfolio_Sum[-nrow(Portfolio_Sum),]

names(Portfolio_Sum)[2:8] <- c("Portf.","ACWI","ACWX","SP500","STOXX50","DAX","RUS2000")
rm(Days,i,Portfolio_Sum.temp)
cat("\f")
cat (paste0("Sumatoria de los ultimos n dias: "))
print.data.frame(Portfolio_Sum,quote=FALSE,row.names = FALSE)

save.image()

packrat::clean()
