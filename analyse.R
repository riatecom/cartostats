
library("rgeos")
library ("rgdal")
library("cartography")



# ***********************************
# IMPORT ET MISE EN FORME DES DONNEES
# ***********************************
# import du csv
results2017 <- read.csv( "data/results_comidf_2017.csv", header=TRUE, sep=",",
                         dec=".",encoding="utf-8")

# Retrouver le code INSEE des communes
# pour l'année 2017
results2017$id  <- gsub(".html+$", "", results2017$link)
results2017$id  <- gsub("056AR", "1", results2017$id )
results2017$id  <- substr(results2017$id , 6, 100)
results2017 <- results2017[,c("id","name","nb_jlm2017","tx_jlm2017","abstention", "exprimés")]

# pour l'année 2012
results2012 <- read.csv( "data/results_comidf_2012.csv", header=TRUE, sep=",",
                         dec=".", encoding="utf-8")
results2012$id <- gsub(".html+$", "", results2012$link)
results2012$id <- gsub("056AR", "1", results2012$id)
results2012$id <- substr(results2012$id, 6, 100)
results2012$id <- results2012$id
results2012 <- results2012[c("id","name","nb_jlm2012","tx_jlm2012","abstention", "exprimés")]
head(results2012)



# import du fond de carte des communes d'IDF
communes <- readOGR(dsn = "data/idf.geojson", layer = "OGRGeoJSON", 
                    stringsAsFactors = FALSE)
# import du fond de carte des départements
departements <- readOGR(dsn = "data/departements.geojson", 
                        layer = "OGRGeoJSON", 
                        stringsAsFactors = FALSE)


# modifier la projection des fond de carte
# WGS84 => Lambert93
prj <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
communes <- spTransform(x = communes, CRSobj = prj)
departements <- spTransform(x = departements, CRSobj = prj)







# **************************
# PLANCHE CARTOGRAPHIQUE 1 *
# **************************


# discretisation & colors
serie <- c(results2012$tx_jlm2012,results2017$tx_jlm2017)
hist(serie, probability = TRUE, nclass = 30)
rug(serie)
moy <- mean(serie)
med <- median(serie)
abline(v = moy, col = "red", lwd = 3)
abline(v = med, col = "blue", lwd = 3)
nb <- 8
breaks <- getBreaks(v = serie, nclass = 8, method = "quantile")
breaks2012 <- c(min(results2012$tx_jlm2012),breaks[2:8],max(results2012$tx_jlm2012))
breaks2017 <- c(min(results2017$tx_jlm2017),breaks[2:8],max(results2017$tx_jlm2017))
cols <- carto.pal(pal1 = "wine.pal" ,n1 = nb)

dev.off()
opar <- par(mar = c(0, 0, 1.2, 0), mfrow = c(2, 2))

# MAP 1


plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes, spdfid="INSEE_COM",
           df = results2012, dfid="id",
           var = "tx_jlm2012",
           breaks = breaks2012,
           nclass=nb,
           col = cols,
           border = "white",
           lwd=0.2,
           add = T,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Score en %\ndes suffrages\nexprimés",
           legend.values.rnd = 2)
plot(departements, col = NA, border = "white", lwd =1.5, add = TRUE)
layoutLayer(title = "Score de Jean-Luc Mélenchon en 2012", author = "", 
            sources = "", frame = TRUE, theme="wine.pal",
            north = FALSE, scale = NULL, col = "black")

# MAP 2

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#DDDDDD", border = "white", lwd = 1,add=T)
plot(communes, col = "#dbd6ce", border = "#EEEEEE", lwd = 0.5, add = TRUE)
plot(departements, col = NA, border = "white", lwd =1.5, add = TRUE)
propSymbolsChoroLayer(spdf = communes, spdfid="INSEE_COM",
                      df = results2012, dfid="id",
                      var = "exprimés",
                      var2 = "tx_jlm2012",
                      breaks = breaks2012,
                      nclass=nb,
                      col = cols,
                      border = "#DDDDDD",
                      lwd=0.4,
                      add = T,
                      fixmax = 120000,
                      inches=0.1,
                      legend.var2.pos = "n",
                      legend.var2.title.txt = "Score en %\ndes suffrages\nexprimés",
                      legend.var2.values.rnd = 2,
                      legend.var.pos = "topleft",
                      legend.var.style = "e",
                      legend.var.title.txt = "Suffrages exprimés",
                      legend.var.values.rnd = 0)
layoutLayer(title = "", author = "", 
            sources = "", frame = TRUE, theme="wine.pal", 
            north = TRUE, scale = NULL, col = "black")

# MAP 3

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
plot(communes, col = "white", border = "#487096", lwd = 0.5, add = TRUE)
choroLayer(spdf = communes, spdfid="INSEE_COM",
           df = results2017,
           var = "tx_jlm2017",
           breaks = breaks2017,
           nclass=nb,
           col = cols,
           border = "white",
           lwd=0.2,
           add = T,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Score en %\ndes suffrages\nexprimés",
           legend.values.rnd = 2)
plot(departements, col = NA, border = "white", lwd =1.5, add = TRUE)
layoutLayer(title = "Score de Jean-Luc Mélenchon en 2017", author = "", 
            sources = "", frame = TRUE, theme="wine.pal",
            north = FALSE, scale = NULL, col = "black")

# MAP 4

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#DDDDDD", border = "white", lwd = 1,add=T)
plot(communes, col = "#dbd6ce", border = "#EEEEEE", lwd = 0.5, add = TRUE)
plot(departements, col = NA, border = "white", lwd =1.5, add = TRUE)
propSymbolsChoroLayer(spdf = communes, spdfid="INSEE_COM",
                      df = results2017,
                      var = "exprimés",
                      var2 = "tx_jlm2017",
                      breaks = breaks2017,
                      nclass=nb,
                      col = cols,
                      border = "#DDDDDD",
                      lwd=0.4,
                      add = T,
                      fixmax = 120000,
                      inches=0.1,
                      legend.var2.pos = "n",
                      legend.var2.title.txt = "Score en %\ndes suffrages\nexprimés",
                      legend.var2.values.rnd = 2,
                      legend.var.pos = "topleft",
                      legend.var.style = "e",
                      legend.var.title.txt = "Suffrages exprimés",
                      legend.var.values.rnd = 0)
layoutLayer(title = "", author = "", 
            sources = "", frame = TRUE, theme="wine.pal",
            north = FALSE, scale = 20, col = "black")





# **************************
# PLANCHE EVOLUTION
# **************************

votes.df <- data.frame(results2012, results2017[match(results2012[,"id"], 
                                                      results2017[,"id"]),])
votes.df$diff <- votes.df$nb_jlm2017 - votes.df$nb_jlm2012
votes.df$evol <-  (votes.df$tx_jlm2017 / votes.df$tx_jlm2012)*100
votes.df <- votes.df[!is.na(votes.df$tx_jlm2012) & !is.na(votes.df$tx_jlm2017),]

# ABSOLU
# RELATIF
par(opar)
bks <- c(39.88,50,100,150, 175,200, 250, 1303)
cols <- carto.pal(pal1 = "blue.pal",n1=2, pal2 = "red.pal",n2=5)
plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
choroLayer(spdf = communes, spdfid = "INSEE_COM",
           df = votes.df,
           var = "evol",
           breaks =bks,
           nclass=6,
           col = cols,
           border = "white",
           lwd=0.2,
           colNA = "white",
           legend.pos = "bottomleft",
           legend.title.txt = "Evolution\ndes scores\n2012-2017 (%)",
           legend.values.rnd = 2,add=T)
plot(departements, col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Evolution du vote Mélenchon 2012-2017 (%)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")
# Fin de la planche
dev.off()


#---- 
# REGRESSION
#--------

plot(votes.df$tx_jlm2012, votes.df$tx_jlm2017, xlim = c(0,40), ylim = c(0, 40), asp = 1, pch = 21, col = "red", cex = 0.5)
x <- lm(tx_jlm2017~tx_jlm2012, data = votes.df)

abline(x, col = "green")
summary(x)

votes.df$res <- residuals(x)

par(mfrow = c(1,2), mar = c(0,0,1.2,0))

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
choroLayer(spdf = communes,spdfid = "INSEE_COM", df = votes.df, var = "res", 
           method = "q6", nclass=6, border = "white", lwd = 0.2, add=T, 
           col = carto.pal("turquoise.pal", 3, "wine.pal", 3))

plot(departements, col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Evolution du vote Mélenchon 2012-2017 (%)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")

head(votes.df)
plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
propSymbolsChoroLayer(spdf = communes,spdfid = "INSEE_COM", df = votes.df, var = "nb_jlm2017",var2 = "res", 
           method = "q6", nclass=6, border = "grey20", lwd = 0.5, add=T, inches = 0.15,
           col = carto.pal("turquoise.pal", 3, "wine.pal", 3))

plot(departements, col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Evolution du vote Mélenchon 2012-2017 (%)", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")





# **************************
# PLANCHE CARTOGRAPHIQUE 2 *
# **************************

results2012$nb <- results2012$nb_jlm2012*100
breaks <-  getBreaks(results2012$tx_jlm2012,6)
breaks[1] <- 5
breaks[7] <- 17.1

cols <-  carto.pal("wine.pal",n1=6)

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
smoothLayer(spdf = communes, df = results2012, spdfid = "INSEE_COM",
            var = 'nb', var2 = 'exprimés',
            span = 4000, beta = 2,breaks = breaks, 
            col = cols, 
            legend.title.txt = "Vote JLM\n(potentiel 4km)",
            mask=communes,
            legend.pos = "bottomleft", legend.values.rnd = 0,add=T)
plot(departements, col = NA, border = "white", lwd = 1,add=T)
layoutLayer(title = "Géographie du vote Mélenchon en 2012", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")
# CARTE 2017

results2017$nb <- results2017$nb_jlm2017*100
breaks <-  getBreaks(results2017$tx_jlm2017,6)
breaks[1] <- 10 
breaks[7] <- 35.5
cols <-  carto.pal("wine.pal",n1=6)

plot(communes, border = NA, col = NA, bg = "#dbd6ce")
plot(departements, col = "#dbd6ce", border = "white", lwd = 1,add=T)
smoothLayer(spdf = communes, df = results2017, spdfid = "INSEE_COM",
            var = 'nb', var2 = 'exprimés',
            span = 4000, beta = 2, 
            breaks = breaks,
            col = cols,
            legend.title.txt = "vote JLM\n(potentiel 4km)",
            mask=communes,
            legend.pos = "bottomleft", legend.values.rnd = 0,add=T) 



plot(departements, col = NA, border = "white", lwd = 1,add=T)
head(departements@data)
layoutLayer(title = "Géographie du vote Mélenchon en 2017", author = "", 
            sources = "", frame = TRUE, 
            north = FALSE, scale = 20, col = "black")


