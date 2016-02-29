# 2. faza: Uvoz podatkov


# Funkcije, ki uvozijo podatke


library(xlsx)


uvozi.pod<-function(){
  return(read.table("podatki/ObservationData_zugbzmd.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))
}
uvozi.vstop2014 <- function(){
  return(read.table("podatki/inbound1.csv",sep =";",as.is = TRUE,header=TRUE,
                    fileEncoding = "utf-8"))
}
uvozi.slo <- function(){
  return(read.table("podatki/slovenija10.csv",sep =",",as.is = TRUE,header=TRUE,
                    fileEncoding = "utf-8"))
}
library(rjson)
require(dplyr)
require(jsonlite)
require(RCurl)
require(httr)
require(XML)
require(rvest)
require(ggplot2)
require(plyr)
require(gsubfn)
library(reshape2)
library(tidyr)

a<-"http://data.worldbank.org/indicator/ST.INT.ARVL"
stran <- read_html(a)

drzava <- stran %>%
 html_nodes(xpath = "//table[@class='views-table sticky-enabled cols-6']//tbody//tr[@class='odd' or @class='even']//td[@class='views-field views-field-country-value']//a") %>% 
  html_text()

leto2012 <- stran %>%
  html_nodes(xpath = "//table[@class='views-table sticky-enabled cols-6']//tbody//tr[@class='odd' or @class='even']//td[@class='views-field views-field-wbapi-data-value-2012 wbapi-data-value']") %>%
  html_text()
  
leto2013 <- stran %>%
  html_nodes(xpath = "//table[@class='views-table sticky-enabled cols-6']//tbody//tr[@class='odd' or @class='even']//td[@class='views-field views-field-wbapi-data-value-2013 wbapi-data-value wbapi-data-value-last']") %>%
  html_text()
  
vstop <- data.frame(Drzava=drzava, t=leto2012, z=leto2013)

names(vstop)[2] <- "2012"
names(vstop)[3] <- "2013"
vstop$`2012`[vstop$`2012` == vstop$`2012`[1]] <- NA
vstop$`2013`[vstop$`2013` == vstop$`2013`[1]] <- NA

# Zapišimo podatke v razpredelnico.

vstop2014<- uvozi.vstop2014()
pod <- uvozi.pod()
slo10 <-uvozi.slo()
#Urejanje tabel

vstop2014  <- vstop2014[c("Destination","X2014")]
names(vstop2014)[1] <- "Drzava"
names(vstop2014)[2] <- "2014"

vstop2014$`2014`[vstop2014$`2014`==".."] <- NA

pod$Unit <- NULL
slo10$country<- NULL
slo10$measure<-NULL
slo10$Unit<-NULL

slo10$Date[slo10$Date=="1/1/2006 12:00:00 AM"] <- 2006
slo10$Date[slo10$Date=="1/1/2007 12:00:00 AM"] <- 2007
slo10$Date[slo10$Date=="1/1/2008 12:00:00 AM"] <- 2008
slo10$Date[slo10$Date=="1/1/2009 12:00:00 AM"] <- 2009
slo10$Date[slo10$Date=="1/1/2010 12:00:00 AM"] <- 2010
slo10$Date[slo10$Date=="1/1/2011 12:00:00 AM"] <- 2011
slo10$Date[slo10$Date=="1/1/2012 12:00:00 AM"] <- 2012
slo10$Date[slo10$Date=="1/1/2013 12:00:00 AM"] <- 2013
slo10$Date[slo10$Date=="1/1/2014 12:00:00 AM"] <- 2014
slo10$Date[slo10$Date=="1/1/2015 12:00:00 AM"] <- 2015
slo10$Date[slo10$Date=="1/1/2016 12:00:00 AM"] <- NA
slo10 <- slo10[!is.na(slo10$Date),]

vstop2014$`2014` <- vstop2014$`2014` %>% gsub(",", "", .) %>% gsub(" ","", .) %>% as.numeric()
vstop2014$`2014` <- vstop2014$`2014` *1000
vstop2014 <- vstop2014[-c(1,2,10,20,42,60,61,70,103),]
vstop$`2012` <- vstop$`2012` %>% gsub("^(\n)","",.) %>% gsub(",", "", .) %>% gsub(" ","", .) %>% as.numeric()
vstop$`2013` <- vstop$`2013` %>% gsub("^(\n)","",.) %>% gsub(",", "", .) %>% gsub(" ","", .) %>% as.numeric()
vstop2014$`Drzava`[vstop2014$`Drzava`=="Bosnia & Herzg."] <- "Bosnia and Herzegovina"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Brunei"] <- "Brunei Darussalam"
vstop2014$`Drzava`[vstop2014$`Drzava`=="FYR Macedonia"] <- "Macedonia, FYR"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Hong Kong (China)"] <- "Hong Kong SAR, China"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Iran"] <- "Iran, Islamic Rep."
vstop2014$`Drzava`[vstop2014$`Drzava`=="Korea (DPRK)"] <- "Korea, Dem. Rep."
vstop2014$`Drzava`[vstop2014$`Drzava`=="Korea (ROK)"] <- "Korea, Rep."
vstop2014$`Drzava`[vstop2014$`Drzava`=="Kyrgyzstan"] <- "Kyrgyz Republic"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Laos"] <- "Lao PDR"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Macao (China)"] <- "Macao SAR, China"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Marshall Is"] <- "Marshall Islands."
vstop2014$`Drzava`[vstop2014$`Drzava`=="Micronesia FSM"] <- "Micronesia, Fed. Sts."
vstop2014$`Drzava`[vstop2014$`Drzava`=="N. Mariana Is"] <- "Northern Mariana Islands"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Rep. Moldova"] <- "Moldova"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Slovakia"] <- "Slovak Republic"
vstop2014$`Drzava`[vstop2014$`Drzava`=="Solomon Is"] <- "Solomon Islands"
vstop2014$`2014`[vstop2014$`2014`=="837000"]<- 83700000
vstop2014$`2014`[vstop2014$`2014`=="16000"]<- 16000000
vstop <- vstop[!is.na(vstop$`2012`),]
vstop <- vstop[!is.na(vstop$`2013`),]
vstop2 <- merge(vstop,vstop2014)
vstop2 <- vstop2[!is.na(vstop2$`2014`),]
vstop2 <- vstop2 %>% gather(Leto,"Stevilo turistov",-Drzava,na.rm = TRUE)
pod$Date[pod$Date=="1/1/2013 12:00:00 AM"]<- 2013
pod$Date[pod$Date=="1/1/2014 12:00:00 AM"]<- 2014
pod$Date[pod$Date=="1/1/2012 12:00:00 AM"]<- 2012

pod <- pod[!is.na(pod$Date),]

b <- pod[pod$variable=="Travel & Tourism Direct Contribution to GDP",]
b$variable <- NULL
names(b)[1] <- "Drzava"
names(b)[3] <- "Leto"
names(b)[4] <- "Direktni doprinos BDP-ju"
names(b)[2] <- "enota"

c <- pod[pod$variable=="Travel & Tourism Direct Contribution to Employment",]
c$variable <- NULL
names(c)[1] <- "Drzava"
names(c)[4] <- "Direkten doprinos zaposlenosti"
names(c)[3] <- "Leto"
names(c)[2] <- "enota"

d <- pod[pod$variable=="Visitor Exports",]
d$variable <- NULL
names(d)[1] <- "Drzava"
names(d)[4] <- "Potrosnja turistov"
names(d)[3] <- "Leto"
names(d)[2] <- "enota"

e <- pod[pod$variable=="Capital Investment",]
e$variable <- NULL
names(e)[1] <- "Drzava"
names(e)[4] <- "Delez investicij v turizem"
names(e)[3] <- "Leto"
names(e)[2] <- "enota"

f <- pod[pod$variable=="Travel & Tourism Total Contribution to GDP",]
f$variable <- NULL
names(f)[1] <- "Drzava"
names(f)[3] <- "Leto"
names(f)[4] <- "Totalni doprinos BDP-ju"
names(f)[2] <- "enota"

g <- pod[pod$variable=="Travel & Tourism Total Contribution to Employment",]
g$variable <- NULL
names(g)[1] <- "Drzava"
names(g)[4] <- "Totalni doprinos zaposlenosti"
names(g)[3] <- "Leto"
names(g)[2] <- "enota"

h<- slo10[slo10$variable=="Travel & Tourism Total Contribution to GDP",]
h$variable<-NULL
names(h)[2]<-"Totalni doprinos BDP-ju"
names(h)[1]<-"Leto"

i<- slo10[slo10$variable=="Travel & Tourism Total Contribution to Employment",]
i$variable<-NULL
names(i)[2]<-"Totalni doprinos zaposlenosti"
names(i)[1]<-"Leto"

slo10 <- inner_join(h,i)
tabela <- inner_join(b,f)
tabela <- inner_join(tabela,e)
tabela <- inner_join(tabela,d)
zaposlenost <- inner_join(c,g)

Slovenija <- filter(tabela, tabela$Drzava == "Slovenia")
slozaposlenost<-filter(zaposlenost,zaposlenost$Drzava=="Slovenia")
Slovenija_vstop <- filter(vstop2, vstop2$Drzava == "Slovenia")
popularne_drzave<- filter(tabela,tabela$Drzava=="United States" | tabela$Drzava=="China" | tabela$Drzava=="Austria" | tabela$Drzava=="Slovenia" | tabela$Drzava=="France" | tabela$Drzava=="Croatia" | tabela$Drzava=="Italy" | tabela$Drzava=="Greece" | tabela$Drzava=="Spain" | tabela$Drzava=="UK")


tabela$`Direktni doprinos BDP-ju` <- tabela$`Direktni doprinos BDP-ju`  %>% as.numeric()
tabela$`Totalni doprinos BDP-ju` <- tabela$`Totalni doprinos BDP-ju`  %>% as.numeric()
tabela$`Potrosnja turistov` <- tabela$`Potrosnja turistov`  %>% as.numeric()
tabela$`Delez investicij v turizem` <- tabela$`Delez investicij v turizem`  %>% as.numeric()

tabela <- tabela[-c(1182,1181,1180,1194,1193,1192,1200,1199,1198,1188,1187,1186,156,155,154,684,683,682,288,287,286,708,707,706,1206,1205,1204,444,443,442,852,851,850,564,563,562,810,809,808,756,755,754,924,648,647,646,923,922,720,719,718,717,716,715,783,782,781,1132),]
tabela$Drzava[tabela$Drzava=="G20"] <- NA
tabela$Drzava[tabela$Drzava=="OECD"] <- NA
tabela$Drzava[tabela$Drzava=="APEC"] <- NA
tabela$Drzava[tabela$Drzava=="World"] <- NA
tabela$Drzava[tabela$Drzava=="Mediterranean"] <- NA
tabela$Drzava[tabela$Drzava=="World"] <- NA
tabela$Drzava[tabela$Drzava=="World"] <- NA
tabela$Drzava[tabela$Drzava=="World"] <- NA
tabela <- tabela[!is.na(tabela$Drzava),]

f$Drzava[f$Drzava=="G20"] <- NA
f$Drzava[f$Drzava=="OECD"] <- NA
f$Drzava[f$Drzava=="APEC"] <- NA
f$Drzava[f$Drzava=="World"] <- NA
f$Drzava[f$Drzava=="Mediterranean"] <- NA
f$Drzava[f$Drzava=="World"] <- NA
f <- f[!is.na(f$Drzava),]

procenti<- filter(tabela,tabela$enota=="% share")
procenti<-procenti[-c(361,362,363,8,7,9,70,71,72),]
vrednost<- filter(tabela,tabela$enota=="US$ bn")
zaposlenostp<-filter(zaposlenost,zaposlenost$enota=="% share")
zaposlenostv<-filter(zaposlenost,zaposlenost$enota=="US$ bn")

najvec.BDPtotalni <- filter(procenti,procenti$Drzava=="Vanuatu" | procenti$Drzava=="Maldives" | procenti$Drzava=="Aruba" | procenti$Drzava=="Antigua and Barbuda" | procenti$Drzava=="Seychelles")
najvec.BDPdirektni<- filter(procenti,procenti$`Direktni doprinos BDP-ju`>16.8)


najvec.potrosnjep <- filter(tabela,tabela$Drzava=="Anguilla" | tabela$Drzava=="Cape Verde" | tabela$Drzava=="Gambia" | tabela$Drzava=="Antigua and Barbuda" | tabela$Drzava=="Bahamas")
najvec.investicij <- filter(vrednost,vrednost$Drzava=="United States" | vrednost$Drzava== "China" | vrednost$Drzava=="India" | vrednost$Drzava== "Japan" | vrednost$Drzava=="Brazil" )
najvec.potrosnjev <- filter(vrednost,vrednost$`Potrosnja turistov` > 44.480)
najvec.zaposlenostd <- filter(zaposlenostp, zaposlenostp$Drzava=="UK Virgin Islands" | zaposlenostp$Drzava=="Aruba" | zaposlenostp$Drzava=="Bahamas" | zaposlenostp$Drzava=="Seychelles" | zaposlenostp$Drzava=="Macau")
najvec.zaposlenostt <- filter(zaposlenostp, zaposlenostp$Drzava=="Antigua and Barbuda" | zaposlenostp$Drzava=="Aruba" | zaposlenostp$Drzava=="Bahamas" | zaposlenostp$Drzava=="Seychelles" | zaposlenostp$Drzava=="Vanuatu")
najvec.vstop <-filter(vstop2,vstop2$`Stevilo turistov` > 27437000)

