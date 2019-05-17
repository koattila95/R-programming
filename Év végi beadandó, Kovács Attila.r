####################################
# R év végi beadandó		      	   #
# Kovács Attila Bence		      	   #
# Szociológia MA - Big Data elemzés#
####################################

setwd("C:/dokumentumok/MA egyetem/R minden/beadabndó r év végi/adatok")
csv <- read.table("C:/dokumentumok/MA egyetem/R minden/beadabndó r év végi/adatok/master.csv", 
                  header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE, encoding="UTF-8", #az adatfile beolvasásával kezdtem
                  check.names = FALSE, stringsAsFactors = FALSE)

colnames(csv)
#csv[1]
#csv$`�country`

newhead <- enc2native(colnames(csv))
newhead <- gsub("<[^>]+>", "", newhead) #beolvasésnál a colnemsben oda nem illő karaktereket el kellett távolítani
colnames(csv) <- newhead



#length(unique(csv$year))
#orszagok <- unique(as.factor(c(csv$X.U.FEFF.country)))
#str(orszagok)
#evek <- unique(c(csv$year))
#evek
#ossz <- merge(orszagok, evek)





colnames(csv)[7]  <- "suicides.100k.pop"
colnames(csv[7])
colnames(csv)[8]  <- "country.year"        #néhány változónévnek új nevet adtam, a német billentyűzet miatt így könnyebb volt nekem
colnames(csv[8])

################## kezdeti lépések és útkeresés ######################x
#dt2 <- data.frame(csv$country, csv$year, csv$suicides.100k.pop, csv$country.year)
#head(dt2)
#colnames(dt2)
library(plyr)
?ddply
#dt3 <- ddply(dt2,~csv.country.year, summarise,ongy.arany=sum(csv.suicides.100k.pop), év=unique(csv.year), ország=unique(csv.country))
#dt3
#tail(dt3)
#dt3 <- ddply(dt2, ~csv.country, ~csv.year, .fun = NULL, summarise,ongy.arany=sum(csv.suicides.100k.pop))
#dt3
#dt4 <- ddply(dt2, .(csv.country), function(df) {
#  data.frame(év = unique(df$csv.year), ongy.arany=sum(df$csv.suicides.100k.pop))
#})
#dt4



#############aranyositva###########x

df1 <- ddply(csv,.(country.year), summarise,ongyilkosok.szama=sum(suicides_no), lakossag.szama=sum(population), év=unique(year), ország=unique(country)) #a plyr csomag ddply parancsával összegeztem és rendeztem a megadott argumentumok alapján a vektorjaimat az összes táblához
#df1
df1$rate <- df1$ongyilkosok.szama/df1$lakossag.szama*100000 #kiszámoltam az eredeti öngyolkossági arány az össz lakosságras nézve
#df1$rate[df1$rate == 0] <- NA itt eredetileg a 0-át NA-ra akartam helyettesíteni, de mivel csak a top 25 országot vettem minden táblához, nincs benne amügy se 0

df1sort <- head(sort(tapply(df1$rate, df1$ország, mean), decreasing = T), 25) #rátának a mean-je alapján néztem az országokat, csökkenő sorba rendeztem, és vettem az első 25 elemét, így kaptam meg a 25 legnagyobb üngyilkosságarányú országot minden táblánál

df1s <- as.vector(dimnames(unlist(df1sort))) #mivel a tapply-al listába került, unlistelni kellett, meg csak az országnevek kellettek egy vektorba
df1s <- unlist(df1s)
df1s
df1 <- df1[df1$ország %in% df1s,] #az országnevek vektorból csak azok az adatok és országok szerepeljenek az adott df-emben amilyen országneveket előzőleg már kiszelektáltam
df1

############################# csak férfiak ############################
library(data.table)          #átalakítottam az adattáblát, és így vontam ki, logikai függvény segítségével csak a férfiakat (nőket), hogy csak az azokhoz tartozó értékek legyenek a táblában.
dt4 <- data.table(csv)
dt4 <- dt4[sex == "male"]
colnames(dt4)
dt4 <- ddply(dt4,.(country.year), summarise,ongyilkosok.szama=sum(suicides_no), lakossag.szama=sum(population), év=unique(year), ország=unique(country))
dt4
dt4$rate <- dt4$ongyilkosok.szama/dt4$lakossag.szama*100000
#dt4$rate[dt4$rate == 0] <- NA
dt4sort <- head(sort(tapply(dt4$rate, dt4$ország, mean), decreasing = T), 25)

dt4s <- as.vector(dimnames(unlist(dt4sort)))
dt4s <- unlist(dt4s)
dt4s
dt4 <- dt4[dt4$ország %in% dt4s,]
dt4
################## csak nőkre ####################

dt5 <- data.table(csv)
dt5 <- dt5[sex == "female"]
colnames(dt5)
#dt5 <- ddply(dt5,~country.year, summarise,ongy.arany=sum(suicides.100k.pop), év=unique(year), ország=unique(country))
dt5 <- ddply(dt5,.(country.year), summarise,ongyilkosok.szama=sum(suicides_no), lakossag.szama=sum(population), év=unique(year), ország=unique(country))
dt5$rate <- dt5$ongyilkosok.szama/dt5$lakossag.szama*100000
#dt5$rate[dt5$rate == 0] <- NA
dt5
dt5sort <- head(sort(tapply(dt5$rate, dt5$ország, mean), decreasing = T), 25)

dt5s <- as.vector(dimnames(unlist(dt5sort)))
dt5s <- unlist(dt5s)
dt5s
dt5 <- dt5[dt5$ország %in% dt5s,]
dt5


#################### generációk ###########################

str(csv)
dt6 <- ddply(csv, .(year, generation), summarise,ongyilkosok.szama=sum(suicides_no), lakossag.szama=sum(population))
dt6$rate <- dt6$ongyilkosok.szama/dt6$lakossag.szama*100000
#dt6$rate[dt6$rate == 0] <- NA
dt6
colnames(dt6)

#dt6sort <- head(sort(tapply(dt6$rate, dt6$generation, mean), decreasing = T), 25)

#dt6s <- as.vector(dimnames(unlist(dt6sort)))
#dt6s <- unlist(dt6s)
#dt6s
#dt6 <- dt6[dt6$ország %in% dt6s,]
#dt6
############################# top 6 arányaiban országok #######################
dt7 <- df1
dt7sort <- head(sort(tapply(dt7$rate, dt7$ország, mean), decreasing = T), 6)
dt7s <- as.vector(dimnames(unlist(dt7sort)))
dt7s <- unlist(dt7s)
dt7s
dt7 <- dt7[dt7$ország %in% dt7s,]
dt7
dt7$év <- as.factor(dt7$év) #ez majd a plothoz kellett, hogy normálisan jelenjen meg
str(dt7)
levels(dt7$év)

colnames(dt7)
orsz <- unique(dt7$ország)                     #függvénymeghívással gyorsabban ki tudtam szedni az országokat egyenként és azok értékeit, hogy csak a legfrissebb (utolsó) 5 elemet emelje ki, ez is a plothoz, hogy szép legyen
orszval <- function(x) {
  tail(dt7[dt7$ország %in% orsz[x],])
}

orsz1 <- orszval(1)
orsz2 <- orszval(2)
orsz3 <- orszval(3)
orsz4 <- orszval(4)
orsz5 <- orszval(5)
orsz6 <- orszval(6)
dt7elemzesre <- rbind(orsz1, orsz2, orsz3, orsz4, orsz5, orsz6)
dt7elemzesre$év <- as.factor(dt7elemzesre$év)
levels(dt7elemzesre$év)                              # összefűztem új adattáblává, és a már fentebb részletezett módon faktorizáltam az éveket.
str(dt7elemzesre)



######################### top 6 generációk ####################

dt8 <- ddply(csv, .(generation, country), summarise,ongyilkosok.szama=sum(suicides_no), lakossag.szama=sum(population))

dt8$rate <- dt8$ongyilkosok.szama/dt8$lakossag.szama*100000

str(dt8)
dt8sort <- head(sort(tapply(dt8$rate, dt8$country, mean), decreasing = T), 6)
dt8s <- as.vector(dimnames(unlist(dt8sort)))
dt8s <- unlist(dt8s)
dt8s
dt8 <- dt8[dt8$country %in% dt8s,]

dt8$generation <- as.factor(dt8$generation)

########################### ggplot országok ##########################

évek <- function(x) {
  seq(min(x), max(x), 2)                          #az x tengely értékeit és az y tengely értékeit számító függvényem (nem volt mindenhol használható)
}
arany <- function(x) {
  round(seq(min(x), max(x), 2), digits = 0)
}
évekg1 <- évek(df1$év)               #itt lett definiálva a függvény az adott plothoz (nem benne, így nekem átláthatóbb volt)
arányg1 <- arany(df1$rate)
arányg1

library(ggplot2)                      #csomagmeghívások
library(RColorBrewer)

#évekg1 <- seq(min(dt4$év), max(dt4$év), 2)
#évekg1
#arányg1 <- round(seq(min(dt4$rate), max(dt4$rate), 2), digits = 0)
#arányg1

colnames(df1)
g1 <- ggplot(df1, aes(x=év, y=rate, group = ország))
g1 <- g1+geom_line(aes(col = ország), size = 1) +                      #ggplot meghatározása
  labs(title = '25 legnagyobb öngyilkossági aránnyal rendelkező ország'
       ,y = 'öngyilkosságok 100ezer főre vetítve'
       ,x = 'évek')+
  scale_x_continuous(labels = évekg1, breaks = évekg1) +               #címek után az x és y skála a fentebb definiált függvény segítségével felveszi az adatokat
  scale_y_continuous(labels = arányg1, breaks = arányg1) + 
  theme(
    legend.direction = "horizontal",                                   #themen belül meghatároztam a különböző paramétereket a plothoz, pl, ábramagyarázat, címek, tengelycímek finomhangolása, stb.
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 8),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  )
 # scale_size_manual(values=seq(0.5, 2.9, 0.1)) #növekedjenek a vonalak vastagsága egytől egyig de nem látom értelmét
g1 <- g1 +  scale_color_manual(values=c("darkred", "darkblue", "green", "pink", "salmon4", "skyblue1", "snow4", "yellow", "violetred4", "tan4", "seagreen1", "orange", "purple2", "plum4", "royalblue1", "papayawhip", "saddlebrown", "tomato4", "violetred", "snow", "tan", "springgreen1", "black", "tomato1", "yellowgreen"))
#egyéni színeket is megadtam
g1

########################## országok csak férfiak ####################
évekg2 <- évek(dt4$év)
arányg2 <- round(seq(min(dt4$rate), max(dt4$rate), 5), digits = 0)
arányg2
colnames(dt4)

g2 <- ggplot(dt4, aes(x=év, y=rate, group = ország))
g2 <- g2+geom_line(aes(col = ország), size = 1) + 
  labs(title = "25 legnagyobb öngyilkossági aránnyal rendelkező ország"
       ,subtitle = "csak a férfiak arányában"
       ,y = "öngyilkosságok 100ezer főre vetítve"
       ,x = "évek") +
  scale_x_continuous(labels = évekg2, breaks = évekg2) + 
  scale_y_continuous(labels = arányg2, breaks = arányg2) + 
  theme(
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 10),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  )+
  scale_size_manual(values=seq(0.5, 2.9, 0.1))
  g2 <- g2 +  scale_color_manual(values=c("darkred", "darkblue", "green", "pink", "salmon4", "skyblue1", "snow4", "yellow", "violetred4", "tan4", "seagreen1", "orange", "purple2", "plum4", "royalblue1", "papayawhip", "saddlebrown", "tomato4", "violetred", "snow", "tan", "springgreen1", "black", "tomato1", "yellowgreen"))
g2

######################## országok csak a nők arányában ##################
évekg3 <- évek(dt5$év)
arányg3 <- arany(dt5$rate)


colnames(dt5)

g3 <- ggplot(dt5, aes(x=év, y=rate, group = ország))
g3 <- g3+geom_line(aes(col = ország), size = 1) + 
  labs(title = "25 legnagyobb öngyilkossági aránnyal rendelkező ország"
       ,subtitle = "csak a nők arányában"
       ,y = "öngyilkosságok 100ezer főre vetítve"
       ,x = "évek") +
  scale_x_continuous(labels = évekg3, breaks = évekg3) + 
  scale_y_continuous(labels = arányg3, breaks = arányg3) + 
  theme(
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.title = element_blank(), 
    legend.text = element_text(color = "black", size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 10),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  )
g3 <- g3 +  scale_color_manual(values=c("darkred", "darkblue", "green", "pink", "salmon4", "skyblue1", "snow4", "yellow", "violetred4", "tan4", "seagreen1", "orange", "purple2", "plum4", "royalblue1", "papayawhip", "saddlebrown", "tomato4", "violetred", "snow", "tan", "springgreen1", "black", "tomato1", "yellowgreen"))
g3 <- g3 + scale_size_manual(values=seq(0.5, 2.9, 0.1))
g3

######################## öngyilkosságok a generációk arányában ##########################
évekg4 <- évek(dt6$year)
arányg4 <- arany(dt6$rate)
arányg4
colnames(dt6)


g4 <- ggplot(dt6, aes(x=year, y=rate, group = generation))
g4 <- g4+geom_line(aes(col = generation), size = 1.5) + 
  labs(title = "Öngyilkosságok aránya a generációk tükrében"
       ,y = "öngyilkosságok 100ezer főre vetítve"
       ,x = "évek") +
  scale_x_continuous(labels = évekg4, breaks = évekg4) + 
  scale_y_continuous(labels = arányg4, breaks = arányg4) + 
  theme(
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.title = element_blank(), 
    legend.text = element_text(color = "black", size = 7),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(0.3,"cm"),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 10),
    legend.background = element_rect(fill="white", 
                                     size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  ) + 
  geom_text(data = dt6, label= round(dt6$rate, 1), size = 3, check_overlap = TRUE, #itt még a vonalakra ki lett íratva az értékeik
            colour="black")
g4
dt6$generation
str(dt6)

########################## barplot 1 #################################
arányg5 <- round(seq(1, max(dt8$rate), 4), digits = 0)
#első plot minden évvel túlátláthatatlan
colnames(dt7)
g5 <- ggplot(dt7, aes(ország, rate)) +   
  geom_bar(aes(fill = factor(év, levels=c("1985", "1986", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000",
                                           "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                                           "2015", "2016"))), position = "dodge", stat="identity") 
g5 #második plotnál már csak a szelektált évek alapján van, így átláthatóbb 

g5röv <- ggplot(dt7elemzesre, aes(ország, rate)) +   
  geom_bar(aes(fill = factor(év, levels=c("1999", "2000",
                                          "2001", "2002", "2003", "2006", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
                                          "2015", "2016"))), position=position_dodge(0.8), stat="identity", width=0.6, color="black")+ #ezért lett az adattáblában vektorizálva az évek
  labs(title = "6 legmagasabb öngyilkossági aránnyal rendelkező ország"
       ,subtitle = "az elmúlt 5 évük statisztikái alapján"
       ,y = "öngyilkosságok 100ezer főre vetítve"
       ,x = "országok")+
  scale_y_continuous(labels = arányg5, breaks = arányg5)+
  theme(
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.title = element_blank(), 
    legend.text = element_text(margin = margin(l = 5, r = 10, unit = "pt")),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 10),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  )


g5röv <- g5röv  +  scale_fill_manual(values=c("violetred4", "violetred1", "violet", "turquoise4", "turquoise1", "steelblue3", "slateblue4", "slateblue1", "springgreen", "springgreen4", "sienna1", "sienna4", "orangered1", "red2", "yellow"))
g5röv

################### barplot 2 ########################################
arányg6 <- round(seq(min(dt8$rate), max(dt8$rate), 5), digits = 0) #sajnos nem mindenhol volt jó megoldás az automatizálás, túl sok az öngyilkos itt
colnames(dt8)
g6 <- ggplot(dt8, aes(country, rate)) +   
  geom_bar(aes(fill = factor(generation, levels=c("G.I. Generation","Silent","Boomers","Generation X","Millenials","Generation Z" ))),  
           position=position_dodge(0.8), stat="identity", width=0.6, color="black")+ 
  labs(title = "országok öngyilkossági arányai"
       ,subtitle = "a generációk tükrében"
       ,y = "öngyilkosságok 100ezer főre vetítve"
       ,x = "országok")+
  scale_y_continuous(labels = arányg6, breaks = arányg6)+
  theme(
    legend.direction = "horizontal",
    legend.position="bottom",
    legend.title = element_blank(), 
    legend.text = element_text(margin = margin(l = 5, r = 10, unit = "pt")),
    panel.background = element_rect(fill = "darkseagreen3"),
    plot.title = element_text( color = "darkred"),
    axis.title.x = element_text(size = 10, color = "darkred"),
    axis.title.y = element_text(size = 10, color = "darkred"),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 10),
    legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="darkred"),
    panel.grid.major =   element_line(colour = "tan",size=0.75)
  )
 
  
g6 + scale_fill_brewer(palette = "Set1") 
g6
nrow(dt8$country)
View(dt8)
str(dt8)
################################ egybe a plotok ######################

library(ggpubr)


text <- paste("Adott öngyilkossági adatok tükrében",
              "hazánk a világ országai között is az élmezőnybe",
              "tartozik, több másik keleti blokkba tartozó országgal együtt",
              sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black") #ez lenne a 3 soros elemzése a tábláknak

osszplot2 <- ggarrange(g4, text.p, ncol = 1, nrow = 2)
 
ggarrange(g1, g2, g3, g4, g5röv, g6, ncol=3, nrow=2)               #háromféleképp összefűzve a diagrammok, majd az utolsó féleképp összefűzöttet lementettem a feladatban megadott méretek alapján

library(cowplot)

osszplot1 <- ggdraw() +
  draw_plot(g1, x = .1, y = .5, width = .8, height = .5) +
  draw_plot(g2, x = 0, y = 0, width = .5, height = .5) +
  draw_plot(g3, x = .5, y = 0, width = .5, height = .5)

library(gridExtra)

grid.arrange(g1, g2, g3, g4, g5röv, g6, ncol=3, nrow=2)
