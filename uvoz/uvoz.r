# 2. faza: Uvoz podatkov


# Funkcije, ki uvozijo podatke
=======

library(xlsx)

uvozi.zaposlenost <- function() {
  return(read.table("podatki/brezposlenost.csv", sep = ";", as.is = TRUE,header = TRUE,
                      fileEncoding = "utf-8"))
}

uvozi.podatke<- function() {
  return(read.table("podatki/podatki.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))}

uvozi.vstop <- function() {
  return(read.table("podatki/vstop.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))
}


uvozi.BDP <- function() {
  return(read.xlsx(file = "podatki/bdp.xlsx", sheetName = "ObservationData",header=TRUE))}

library(rjson)
investicije <- fromJSON(file="potrosnja_in_investicije.json",method = "C",unexpected.escape = "skip")

require(dplyr)
require(jsonlite)
library(RCurl)
require(httr)
r<-GET("http://knoema.com/api/1.0/data/WTTC2015?Time=2015&country=1000000,1000010,1000020,1000030,1000040,1000050,1000060,1000070,1000080,1000090,1000100,1000110,1000120,1000130,1000140,1000150,1000160,1000170,1000180,1000190,1000200,1000210,1000220,1000230,1000240,1000250,1000260,1000270,1000280,1000290,1000300,1000310,1000320,1000330,1000340,1000350,1000360,1000370,1000380,1000390,1000400,1000410,1000420,1000430,1000440,1000450,1000460,1000470,1000480,1000490,1000500,1000510,1000520,1000530,1000540,1000550,1000560,1000570,1000580,1000590,1000600,1000610,1000620,1000630,1000640,1000650,1000660,1000670,1000680,1000690,1000700,1000710,1000720,1000730,1000740,1000750,1000760,1000770,1000780,1000790,1000800,1000810,1000820,1000830,1000840,1000850,1000860,1000870,1000880,1000890,1000900,1000910,1000920,1000930,1000940,1000950,1000960,1000970,1000980,1000990,1001000,1001010,1001020,1001030,1001040,1001050,1001060,1001070,1001080,1001090,1001100,1001110,1001120,1001130,1001140,1001150,1001160,1001170,1001180,1001190,1001200,1001210,1001220,1001230,1001240,1001250,1001260,1001270,1001280,1001290,1001300,1001310,1001320,1001330,1001340,1001350,1001360,1001370,1001380,1001390,1001400,1001410,1001420,1001430,1001440,1001450,1001460,1001470,1001480,1001490,1001500,1001510,1001520,1001530,1001540,1001550,1001560,1001570,1001580,1001590,1001600,1001610,1001620,1001630,1001640,1001650,1001660,1001670,1001680,1001690,1001700,1001710,1001720,1001730,1001740,1001750,1001760,1001770,1001780,1001790,1001800,1001810,1001820,1001830,1001840,1001850,1001860,1001870,1001880,1001890,1001900,1001910,1001920,1001930,1001940,1001950,1001970,1001980,1001990,1002000,1002010&variable=1000100,1000060&measure=1000050,1000010&Frequencies=A")
text <- content(r, "text")
data <- fromJSON(content(r, "text"))
data$dataset %>% names() %>% print()



# Zapišimo podatke v razpredelnico.
zaposlenost <- uvozi.zaposlenost()

vstop <- uvozi.vstop()
BDP<- uvozi.BDP()

podatki <- uvozi.podatke()


#Urejanje tabel
vstop$variable <- NULL
vstop$Unit <- NULL
View(vstop)

zaposlenost<- zaposlenost[c("Country.Name","X2015")]
BDP<- BDP[c("Country.Name","X2015")]

names(vstop)[1] <- "Drzava"
names(vstop)[2] <- "Datum"
names(vstop)[3] <- "Število tujcev, ki so vstopili v državo"
Encoding(names(vstop))<-"WINDOWS-1250"

names(zaposlenost)[1] <- "Drzava"
names(zaposlenost)[2] <- "Število zaposlenih v turizmu(%)"
Encoding(names(zaposlenost))<-"WINDOWS-1250"

names(BDP)[1] <- "Drzava"
names(BDP)[2] <- "Delez BDP-ja ki ga predstavlja turizem (%)"
Encoding(names(BDP))<-"WINDOWS-1250"

#Filtriranje vrstic v tabeli vstop glede na leto
vstop2006 <- vstop[vstop$Datum == "1/1/2006 12:00:00 AM",]
vstop2007 <- vstop[vstop$Datum == "1/1/2007 12:00:00 AM",]
vstop2008 <- vstop[vstop$Datum == "1/1/2008 12:00:00 AM",]
vstop2009 <- vstop[vstop$Datum == "1/1/2009 12:00:00 AM",]
vstop2010 <- vstop[vstop$Datum == "1/1/2010 12:00:00 AM",]
vstop2011 <- vstop[vstop$Datum == "1/1/2011 12:00:00 AM",]
vstop2012 <- vstop[vstop$Datum == "1/1/2012 12:00:00 AM",]

tabela<-merge(BDP,zaposlenost,by.x="Drzava",by.y="Drzava",all=TRUE)
