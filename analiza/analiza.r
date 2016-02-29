# 4. faza: Analiza podatkov


#napoved rasti BDP-ja skozi čas(za SLO)
slo10$Leto<- slo10$Leto %>% as.numeric()
graf <- ggplot(slo10,aes(x=Leto,y=`Totalni doprinos BDP-ju`)) + geom_point(aes(size=`Totalni doprinos zaposlenosti`)) 
grafek <-graf+ geom_smooth(method = "lm",aes(group = 1))
napoved1 <- lm(data=slo10,`Totalni doprinos BDP-ju`~Leto)
s<-predict(napoved1,data.frame(Leto=seq(2006,2017,1)))
napoved1 <- matrix(s)
Leto<- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
napoved <-data.frame(Leto,napoved1)
names(napoved)[2]<- "Napoved rasti BDP-ja"
slo10$`Totalni doprinos BDP-ju`<- slo10$`Totalni doprinos BDP-ju` %>% as.numeric()

napoved3 <- lm(data = slo10, `Totalni doprinos zaposlenosti` ~ Leto)
z<-predict(napoved3, data.frame(Leto=seq(2006,2017,1)))
napoved3 <- matrix(z)
Leto<- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)
napoved3 <- data.frame(Leto,napoved3)
names(napoved3)[2]<-  "Napoved rasti zaposlenosti"

#tabelav kateri je prikazana napoved bdp-ja in zaposlenost
napoved<-inner_join(napoved,napoved3)
#preveranje zaposlenosti v odvisnosti od BDP-ja
graf2 <- ggplot(slo10,aes(`Totalni doprinos BDP-ju`,`Totalni doprinos zaposlenosti`))+geom_point()
graf2+geom_smooth(method="lm",aes(group = 1))
napoved2 <- lm(data=slo10,`Totalni doprinos zaposlenosti`~ `Totalni doprinos BDP-ju`)
j <-predict(napoved2)
napoved4 <-matrix(j)
Leto1<- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)
napoved4 <- data.frame(Leto1,napoved4)
names(napoved4)[2] <- "Napoved zaposlenosti v odvisnost od BDP-ja"
names(napoved4)[1]<- "Leto"
slo10<-inner_join(slo10,napoved4)

#razvrščanje
tabela2014 <- f%>%filter(Leto==2014& enota=="% share")
tabela2014<- tabela2014[c('Drzava','Totalni doprinos BDP-ju')]
zaposlenost2014 <- g%>%filter(Leto==2014 & enota=="% share")
zaposlenost2014 <- zaposlenost2014[c('Drzava','Totalni doprinos zaposlenosti')]
tabela2014$Drzava[tabela2014$Drzava== "UK"]<- "United Kingdom"
tabela2014$Drzava[tabela2014$Drzava== "Bosnia Herzegovina"]<- "Bosnia and Herzegovina"
tabela2014$Drzava[tabela2014$Drzava== "Serbia"]<- "Republic of Serbia"

tabela2014$Drzava[tabela2014$Drzava== "Bahamas"]<- "The Bahamas"
tabela2014$Drzava[tabela2014$Drzava== "Congo"]<- "Republic of Congo"
tabela2014$Drzava[tabela2014$Drzava== "Democratic Republic of Congo"]<- "Democratic Republic of the Congo"
tabela2014$Drzava[tabela2014$Drzava== "Madagasca"]<- "Madagascar"
tabela2014$Drzava[tabela2014$Drzava== "Trinidad & Tobago "]<- "Trinidad and Tobago"
tabela2014$Drzava[tabela2014$Drzava== "United States"]<- "United States of America"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "UK"]<- "United Kingdom"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Bosnia Herzegovina"]<- "Bosnia and Herzegovina"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Serbia"]<- "Republic of Serbia"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Bahamas"]<- "The Bahamas"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Congo"]<- "Republic of Congo"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Democratic Republic of Congo"]<- "Democratic Republic of the Congo"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Madagasca"]<- "Madagascar"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "Trinidad & Tobago "]<- "Trinidad and Tobago"
zaposlenost2014$Drzava[zaposlenost2014$Drzava== "United States"]<- "United States of America"


bdp_zap2014 <- inner_join(tabela2014,zaposlenost2014)
bdp_zap2014 <-bdp_zap2014%>%filter(Drzava=="Slovenia" | Drzava=="Caribbean" | Drzava=="Anguilla"  | Drzava=="Bahamas" | Drzava=="Aruba" | Drzava=="Antigua and Barbuda" | Drzava=="Seychelles" | Drzava=="Maldives" | Drzava=="Fiji" | Drzava=="Croatia" | Drzava=="Cambodia" | Drzava=="Montenegro" | Drzava=="Iceland" | Drzava=="Greece" | Drzava=="Portugal" | Drzava=="Spain" | Drzava=="Egypt" | Drzava=="Austria" | Drzava=="Sweden" | Drzava=="Hungary" | Drzava=="Italy" | Drzava=="France" | Drzava=="China" | Drzava=="	
Democratic Republic of Congo" | Drzava=="Uzbekistan" | Drzava=="Nigeria" | Drzava=="Niger" | Drzava=="Germany" | Drzava=="Canada" | Drzava=="Russia" | Drzava=="UK" | Drzava=="Brazil" | Drzava=="Poland" | Drzava=="Malta"  )

row.names(bdp_zap2014)<- bdp_zap2014$Drzava
bdp_zap2014 <- bdp_zap2014[c('Totalni doprinos BDP-ju','Totalni doprinos zaposlenosti')]
bdp_zap.norm <- scale(bdp_zap2014)

k<- kmeans(bdp_zap.norm,5,nstart = 10000)
table(k$cluster)
skupina <- data.frame(Drzava=names(k$cluster),skupina=factor(k$cluster))


# dendogram z ward.D
razporeditev <- dist(as.matrix(bdp_zap.norm))
hc <- hclust(razporeditev, method = "ward.D") 
n <- 5 # število skupin
dend <- as.dendrogram(hc, main = "Razporeditev držav", sub = "", hang = -1)
sk <- cutree(hc, k = n)
#plot(dend)

#razporeditev držav v skupine
pod_analiza <- bdp_zap2014
analiza.norm <- scale(pod_analiza)
k <- kmeans(analiza.norm,5)
table(k$cluster)
k <- kmeans(analiza.norm,5, nstart = 10000)
analiza.skupine <- data.frame(Drzava = names(k$cluster), skupina = factor(k$cluster))
skupine <- analiza.skupine

# drzave <- skupina[c("Slovenia","The Bahamas","Albania"), "skupina"]
# m3 <- match(svet$geounit, skupina$Drzava)
# svet$skupina <- factor(skupina$skupina[m3], levels = drzave, ordered = TRUE)
# evropa <- pretvori.zemljevid(svet, svet$continent == "Europe")
# 
# svet1<- pretvori.zemljevid(svet)
# zem3 <- ggplot() + geom_polygon(data = svet1, aes(x=long, y=lat, 
#                                                    group = group, fill = skupina),
#                                 color = "grey")  + xlab("") + ylab("") 
# plot(zem3)