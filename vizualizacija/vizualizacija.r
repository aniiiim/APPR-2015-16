# 3. faza: Izdelava zemljevida

# Uvozimo zemljevid.
pretvori.zemljevid <- function(zemljevid,pogoj=TRUE) {
  fo <- fortify(zemljevid[pogoj,])
  data <- zemljevid@data
  data$id <- as.character(0:(nrow(data)-1))
  return(inner_join(fo, data, by="id"))
}

svet<-uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip",
                      "ne_110m_admin_0_countries",force=FALSE)

bdp <- select(procenti, Drzava, Leto, `Totalni doprinos BDP-ju`)
m <- match(svet$name_long,bdp$Drzava)
svet$`Totalni doprinos BDP-ju` <- procenti$`Totalni doprinos BDP-ju`[m]
x<-filter(procenti,procenti$Leto==2014)
svet$Leto <- x$Leto[m]
Evropa <- pretvori.zemljevid(svet,svet$continent=="Europe")
map1 <- ggplot() + geom_polygon(data = Evropa, aes(x=long,y=lat, group=group,
                                                   fill= `Totalni doprinos BDP-ju`), color="grey35")+ xlim(-25,50)+ylim(34,75)+ 
  scale_fill_continuous(low = "#69b8f6", high = "#142d45")+ xlab("") + ylab("") +
  guides(fill = guide_colorbar(title = "Totalni doprinos BDP-ju"))
#print(map1)


obiski <- select(vstop, Drzava, `2013`)
m2 <- match(svet$name_long,obiski$Drzava)
svet$`Stevilo turistov` <- vstop$`2013`[m2]
svet1 <- pretvori.zemljevid(svet)
map2 <- ggplot() + geom_polygon(data = svet1, aes(x=long,y=lat, group=group,
                                                  fill= `Stevilo turistov`), color="grey35")+ 
  scale_fill_continuous(low = "#69b8f6", high = "#142d45")+ xlab("") + ylab("") +
  guides(fill = guide_colorbar(title = "Stevilo turistov"))
#print(map2)
