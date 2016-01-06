# 2. faza: Uvoz podatkov


# Funkcije, ki uvozijo podatke


library(xlsx)

uvozi.podatki <- function() {
  return(read.table("podatki/vsi.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))
}

uvozi.vstop2014 <- function(){
  return(read.table("podatki/inbound1.csv",sep =";",as.is = TRUE,header=TRUE,
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
require(gsubfn)
library(reshape2)
library(tidyr)

r<- GET("http://knoema.com/api/1.0/data/WTTC2015?Time=2012-2014&country=1000000,1000010,1000020,1000030,1000040,1000050,1000060,1000070,1000080,1000090,1000100,1000110,1000120,1000130,1000140,1000150,1000160,1000170,1000180,1000190,1000200,1000210,1000220,1000230,1000240,1000250,1000260,1000270,1000280,1000290,1000300,1000310,1000320,1000330,1000340,1000350,1000360,1000370,1000380,1000390,1000400,1000410,1000420,1000430,1000440,1000450,1000460,1000470,1000480,1000490,1000500,1000510,1000520,1000530,1000540,1000550,1000560,1000570,1000580,1000590,1000600,1000610,1000620,1000630,1000640,1000650,1000660,1000670,1000680,1000690,1000700,1000710,1000720,1000730,1000740,1000750,1000760,1000770,1000780,1000790,1000800,1000810,1000820,1000830,1000840,1000850,1000860,1000870,1000880,1000890,1000900,1000910,1000920,1000930,1000940,1000950,1000960,1000970,1000980,1000990,1001000,1001010,1001020,1001030,1001040,1001050,1001060,1001070,1001080,1001090,1001100,1001110,1001120,1001130,1001140,1001150,1001160,1001170,1001180,1001190,1001200,1001210,1001220,1001230,1001240,1001250,1001260,1001270,1001280,1001290,1001300,1001310,1001320,1001330,1001340,1001350,1001360,1001370,1001380,1001390,1001400,1001410,1001420,1001430,1001440,1001450,1001460,1001470,1001480,1001490,1001500,1001510,1001520,1001530,1001540,1001550,1001560,1001570,1001580,1001590,1001600,1001610,1001620,1001630,1001640,1001650,1001660,1001670,1001680,1001690,1001700,1001710,1001720,1001730,1001740,1001750,1001760,1001770,1001780,1001790,1001800,1001810,1001820,1001830,1001840,1001850,1001860,1001870,1001880,1001890,1001900,1001910,1001920,1001930,1001940,1001950,1001970,1001980,1001990,1002000,1002010&variable=1000040,1000100&measure=1000010&Frequencies=A")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))
investicije_potrosnja<-data$data
investicije_potrosnja$Frequency<- NULL
investicije_potrosnja$Scale <- NULL
investicije_potrosnja$RegionId <- NULL
investicije_potrosnja$measure <- NULL
investicije_potrosnja$Unit <- NULL


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

podatki <- uvozi.podatki()
vstop2014<- uvozi.vstop2014()

#Urejanje tabel

vstop2014  <- vstop2014[c("Destination","X2014")]
names(vstop2014)[1] <- "Drzava"
names(vstop2014)[2] <- "2014"

vstop2014$`2014`[vstop2014$`2014`==".."] <- NA

podatki$measure <- NULL
podatki$Unit <- NULL

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
vstop <- merge(vstop,vstop2014)
vstop$`2014`[vstop$`2014`=="837000"]<- 83700000
vstop$`2014`[vstop$`2014`=="16000"]<- 16000000
vstop <- vstop[!is.na(vstop$`2012`),]
vstop <- vstop[!is.na(vstop$`2013`),]
vstop <- vstop[!is.na(vstop$`2014`),]
vstop <- vstop %>% gather(Leto,"Stevilo turistov",-Drzava,na.rm = TRUE)

podatki$Date[podatki$Date=="1/1/2013 12:00:00 AM"]<- 2013
podatki$Date[podatki$Date=="1/1/2014 12:00:00 AM"]<- 2014
podatki$Date[podatki$Date=="1/1/2015 12:00:00 AM"]<- NA
podatki$Date[podatki$Date=="1/1/2012 12:00:00 AM"]<- 2012

investicije_potrosnja$Time[investicije_potrosnja$Time=="2012-01-01T00:00:00Z"] <-2012
investicije_potrosnja$Time[investicije_potrosnja$Time=="2013-01-01T00:00:00Z"] <-2013
investicije_potrosnja$Time[investicije_potrosnja$Time=="2014-01-01T00:00:00Z"] <-2014


podatki <- podatki[!is.na(podatki$Date),]

b <- podatki[podatki$variable=="Travel & Tourism Direct Contribution to GDP",]
b$variable <- NULL
names(b)[1] <- "Drzava"
names(b)[2] <- "Leto"
names(b)[3] <- "Delez BDP-ja ki ga predstavlja turizem (%)"

c <- podatki[podatki$variable=="Travel & Tourism Direct Contribution to Employment",]
c$variable <- NULL
names(c)[1] <- "Drzava"
names(c)[3] <- "Število zaposlenih v turizmu(%)"
names(c)[2] <- "Leto"

d <- podatki[podatki$variable=="Visitor Exports",]
d$variable <- NULL
names(d)[1] <- "Drzava"
names(d)[3] <- "Potrosnja turistov(%)"
names(d)[2] <- "Leto"

e <- podatki[podatki$variable=="Capital Investment",]
e$variable <- NULL
names(e)[1] <- "Drzava"
names(e)[3] <- "Delez investicij v turizem (%)"
names(e)[2] <- "Leto"

f <- investicije_potrosnja[investicije_potrosnja$variable=="Visitor Exports",]
f$variable <- NULL
names(f)[1] <- "Drzava"
names(f)[3] <- "Potrosnja turistov(US $ miljarde)"
names(f)[2] <- "Leto"

g <- investicije_potrosnja[investicije_potrosnja$variable=="Capital Investment",]
g$variable <- NULL
names(g)[1] <- "Drzava"
names(g)[3] <- "Vrednost investicij v turizem(US $ miljarde)"
names(g)[2] <- "Leto"

tabela <- inner_join(b,c)
tabela <- inner_join(tabela,d)
tabela <- inner_join(tabela,e)
tabela <- inner_join(tabela,f)
tabela <- inner_join(tabela,g)


Slovenija <- filter(tabela, tabela$Drzava == "Slovenia")
Slovenija_vstop <- filter(vstop, vstop$Drzava == "Slovenia")
popularne_drzave<- filter(tabela,tabela$Drzava=="United States" | tabela$Drzava=="China" | tabela$Drzava=="Austria" | tabela$Drzava=="Slovenia" | tabela$Drzava=="France" | tabela$Drzava=="Croatia" | tabela$Drzava=="Italy" | tabela$Drzava=="Greece" | tabela$Drzava=="Spain" | tabela$Drzava=="UK")


tabela$`Delez BDP-ja ki ga predstavlja turizem (%)` <- tabela$`Delez BDP-ja ki ga predstavlja turizem (%)`  %>% as.numeric()
tabela$`Potrosnja turistov(%)` <- tabela$`Potrosnja turistov(%)`  %>% as.numeric()
tabela$`Delez investicij v turizem (%)` <- tabela$`Delez investicij v turizem (%)`  %>% as.numeric()
tabela$`Potrosnja turistov(US $ miljarde)` <- tabela$`Potrosnja turistov(US $ miljarde)`  %>% as.numeric()
tabela$`Vrednost investicij v turizem(US $ miljarde)` <- tabela$`Vrednost investicij v turizem(US $ miljarde)`  %>% as.numeric()
tabela$`Število zaposlenih v turizmu(%)` <- tabela$`Število zaposlenih v turizmu(%)`  %>% as.numeric()

tabela <- tabela[-c(364,365,366,591,590,589,597,596,595,594,593,592,600,599,598,342,601,602,603,222,221,220,426,425,424,405,404,403,282,281,280,341,340,354,353,352,75,74,73,141,140,139),]

najvec.BDP <- filter(tabela,tabela$`Delez BDP-ja ki ga predstavlja turizem (%)` > 20.2)
najvec.potrosnje <- filter(tabela,tabela$Drzava=="Anguilla" | tabela$Drzava=="Cape Verde" | tabela$Drzava=="Gambia" | tabela$Drzava=="Antigua and Barbuda" | tabela$Drzava=="Bahamas")
najvec.zaposlenost <- filter(tabela, tabela$Drzava=="UK Virgin Islands" | tabela$Drzava=="Aruba" | tabela$Drzava=="Bahamas" | tabela$Drzava=="Seychelles" | tabela$Drzava=="Maldives")
najvec.investicij <- filter(tabela,tabela$Drzava=="United States" | tabela$Drzava== "China" | tabela$Drzava=="India" | tabela$Drzava== "Japan" | tabela$Drzava=="Brazil" )
najvec.potrosnjev <- filter(tabela,tabela$`Potrosnja turistov(US $ miljarde)` > 45)
najmanj.BDP <- filter(tabela, tabela$Drzava=="Uzbekistan" | tabela$Drzava=="Canada" | tabela$Drzava=="Moldova" | tabela$Drzava=="Democratic Republic of Congo" | tabela$Drzava=="Suriname")
najvec.vstop <-filter(vstop,vstop$`Stevilo turistov` > 27437000)
