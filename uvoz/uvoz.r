# 2. faza: Uvoz podatkov

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.zaposlenost <- function() {
  return(read.table("podatki/brezposlenost.csv", sep = ";", as.is = TRUE,header = TRUE,
                      fileEncoding = "utf-8"))
}


uvozi.vstop <- function() {
  return(read.table("podatki/vstop.csv", sep = ",", as.is = TRUE,header = TRUE,
                    fileEncoding = "utf-8"))
}


uvozi.BDP <- read.xlsx(file = "podatki/bdp.xlsx", sheetName = "ObservationData",header=TRUE)


# Zapišimo podatke v razpredelnico druzine.
zaposlenost <- uvozi.zaposlenost()

vstop <- uvozi.vstop()

BDP <- uvozi.BDP()

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
#Urejanje tabel
vstop$variable <- NULL
vstop$Unit <- NULL
View(vstop)

zaposlenost2<- zaposlenost[c("Country.Name","X2015")]
bdp2<- uvozi.BDP[c("Country.Name","X2015")]

names(vstop)[1] <- "Država"
names(vstop)[2] <- "Datum"
names(vstop)[3] <- "Število tujcev, ki so vstopili v državo"
Encoding(names(vstop))<-"WINDOWS-1250"

names(zaposlenost2)[1] <- "Država"
names(zaposlenost2)[2] <- "Število zaposlenih v turizmu(%)"
Encoding(names(zaposlenost2))<-"WINDOWS-1250"

names(bdp2)[1] <- "Država"
names(bdp2)[2] <- "Delež BDP-ja ki ga predstavlja turizem (%)"
Encoding(names(zaposlenost2))<-"WINDOWS-1250"
