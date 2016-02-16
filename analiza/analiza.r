# 4. faza: Analiza podatkov

#napoved za Slovenijo
slo10$Leto<- slo10$Leto %>% as.numeric()
graf <- ggplot(slo10,aes(x=Leto,y=`Totalni doprinos BDP-ju`)) + geom_point(aes(size=`Totalni doprinos zaposlenosti`)) 
graf+ geom_smooth(method = "lm",aes(group = 1))
napoved <- lm(data=slo10,`Totalni doprinos BDP-ju`~Leto)
predict(napoved,data.frame(Leto=seq(2006,2017,1)))

slo10$`Totalni doprinos BDP-ju`<- slo10$`Totalni doprinos BDP-ju` %>% as.numeric()

graf2 <- ggplot(slo10,aes(Leto,`Totalni doprinos zaposlenosti`))+geom_point()
graf2+geom_smooth(method="lm",aes(group = 1))
napoved2 <- lm(data=slo10,`Totalni doprinos zaposlenosti`~Leto)
predict(napoved2,data.frame(Leto = seq(2006,2017,1)))

tabela2014 <- f%>%filter(Leto==2014& enota=="% share")
tabela2014<- tabela2014[c('Drzava','Totalni doprinos BDP-ju')]
zaposlenost2014 <- g%>%filter(Leto==2014 & enota=="% share")
zaposlenost2014 <- zaposlenost2014[c('Drzava','Totalni doprinos zaposlenosti')]

bdp_zap2014 <- inner_join(tabela2014,zaposlenost2014)
row.names(bdp_zap2014)<- bdp_zap2014$Drzava
bdp_zap2014 <- bdp_zap2014[c('Totalni doprinos BDP-ju','Totalni doprinos zaposlenosti')]
bdp_zap.norm <- scale(bdp_zap2014)

k<- kmeans(bdp_zap.norm,3,nstart = 10000)
table(k$cluster)
skupina <- data.frame(Drzava=names(k$cluster),skupina=factor(k$cluster))

drzave <- skupina[c("Slovenia","The Bahamas","Albania"), "skupina"]
m3 <- match(svet$geounit, skupina$Drzava)
svet$skupina <- factor(skupina$skupina[m3], levels = drzave, ordered = TRUE)
evropa <- pretvori.zemljevid(svet, svet$continent == "Europe")

svet1<- pretvori.zemljevid(svet)
zem3 <- ggplot() + geom_polygon(data = svet1, aes(x=long, y=lat, 
                                                   group = group, fill = skupina),
                                color = "grey")  + xlab("") + ylab("") 
plot(zem3)






razporeditev <- dist(as.matrix(bdp_zap.norm))
hc <- hclust(razporeditev, method = "ward.D") 


n <- 4 # število skupin
dend <- as.dendrogram(hc, main = "Razporeditev držav", sub = "", hang = -1)
sk <- cutree(hc, k = n)
labels_colors(dend) <- rainbow(n)[sk][order.dendrogram(dend)]

plot(dend)




#sedaj narišem dendrogram z metodo centroid

razporeditev2 <- dist(as.matrix(bdp_zap.norm))
hc2 <- hclust(razporeditev2, method = "centroid") 
par(cex=0.6, mar=c(5, 8, 4, 1))
plot(hc2, xlab="", ylab="", main="", sub="", axes=FALSE)
par(cex=1)
title(xlab="xlab", ylab="ylab", main="main")
axis(2)

n <- 4 # število skupin
dend2 <- as.dendrogram(hc, main = "Razporeditev držav", sub = "", hang = -1)
sk2 <- cutree(hc2, k = n)
labels_colors(dend2) <- rainbow(n)[sk2][order.dendrogram(dend2)]

plot(dend2, cex = 0.3)





     