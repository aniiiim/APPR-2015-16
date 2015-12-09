# 2. faza: Uvoz podatkov

<<<<<<< HEAD
# Funkcije, ki uvozijo podatke
=======
# Funkcija, ki uvozi podatke iz datoteke druzine.csv
library(xlsx)
>>>>>>> 75c97f41cc672a5b3132623eacd82ba1fa7f36d3
uvozi.zaposlenost <- function() {
  return(read.table("podatki/brezposlenost.csv", sep = ";", as.is = TRUE,header = TRUE,
                      fileEncoding = "utf-8"))
}


uvozi.vstop <- function() {
  return(read.table("podatki/vstop.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))
}


uvozi.BDP <- function() {
  return(read.xlsx(file = "podatki/bdp.xlsx", sheetName = "ObservationData",header=TRUE))}


# Zapišimo podatke v razpredelnico.
zaposlenost <- uvozi.zaposlenost()

vstop <- uvozi.vstop()
BDP<- uvozi.BDP()


#Urejanje tabel
vstop$variable <- NULL
vstop$Unit <- NULL
View(vstop)

zaposlenost2<- zaposlenost[c("Country.Name","X2015")]
bdp2<- BDP[c("Country.Name","X2015")]

names(vstop)[1] <- "Drzava"
names(vstop)[2] <- "Datum"
names(vstop)[3] <- "Število tujcev, ki so vstopili v državo"
Encoding(names(vstop))<-"WINDOWS-1250"

names(zaposlenost2)[1] <- "Drzava"
names(zaposlenost2)[2] <- "Število zaposlenih v turizmu(%)"
Encoding(names(zaposlenost2))<-"WINDOWS-1250"

names(bdp2)[1] <- "Drzava"
names(bdp2)[2] <- "Delez BDP-ja ki ga predstavlja turizem (%)"
Encoding(names(zaposlenost2))<-"WINDOWS-1250"

#Filtriranje vrstic v tabeli vstop glede na leto
vstop2006 <- vstop[vstop$Datum == "1/1/2006 12:00:00 AM",]
vstop2007 <- vstop[vstop$Datum == "1/1/2007 12:00:00 AM",]
vstop2008 <- vstop[vstop$Datum == "1/1/2008 12:00:00 AM",]
vstop2009 <- vstop[vstop$Datum == "1/1/2009 12:00:00 AM",]
vstop2010 <- vstop[vstop$Datum == "1/1/2010 12:00:00 AM",]
vstop2011 <- vstop[vstop$Datum == "1/1/2011 12:00:00 AM",]
vstop2012 <- vstop[vstop$Datum == "1/1/2012 12:00:00 AM",]
