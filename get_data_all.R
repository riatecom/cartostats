## Chargement des packages

library(xml2)
library(rvest)
library(curl)

## Obtenir les données de 2017

# Liens des pages à lire
site <- "http://elections.interieur.gouv.fr/presidentielle-2017/011"
departements <- c("075","077","078","091","092","093","094","095")

# PARIS
dep <- departements[1]
page <- paste(dep,"html",sep=".")
url <- paste (site,dep,page,sep="/")
webpage <- read_html(x = url)
l <- webpage %>% html_nodes(".offset2") %>% html_nodes("a") %>%  html_attr("href")
l <- gsub("../../011/075/", "", l)
l <- paste(dep,l,sep="/")
links <- l

# AUTRES DEPARTEMENTS
for(i in 2:length(departements)){
  #i <- 5
  dep <- departements[i]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  #l <- l[3:(length(l)-1)]
  tmp <- paste("../../01/",dep,sep="")
  l <- gsub(paste("../../011/",dep,"/",sep=""), "", l)
  l <- paste(dep,l,sep="/")
  if (i == 2){l2 <- l} else {l2 <- c(l2,l)}
}

for (i in 1:length(l2)) {
  url <- paste(site,l2[i],sep="/")
  w <- read_html(curl(url, handle = new_handle("useragent" = "Mozilla/5.0")))
  #w <- read_html(x = url)
  l <- w %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  #l <- l[4:(length(l)-1)]
  l <- gsub("../../011/", "", l)
  start <- length(grep("[A-Z]", l))+1
  stop <-   length(l)
  l <- l[start:stop]
  #l <- strsplit(l2[i],"/")[[1]][1]
  links <- c(links,l)
} 
links



################################################"

# Création d'un premier dataframe avec les resultats
links

df <- data.frame("id"=NA, "commune"=NA, "Hamon" = NA, "Macron" = NA, 
                 "Asselineau" = NA, "Fillon" = NA, "Cheminade" = NA, 
                 "Lassalle" = NA, "Mélenchon" = NA, "Le Pen" = NA, 
                 "Arthaud" = NA, "Dupont-Aignan" = NA, "Poutou" = NA, 
                 "Inscrits" = NA, "Abstentions" = NA, "Votants" = NA, 
                 "Blancs" = NA, "Nuls" = NA, "Exprimés" = NA)

for(i in 1:length(links)){
  df[i,"id"] <- links[i]
  url <- paste (site,links[i],sep="/")
  webpage <- read_html(x = url)
  name <- webpage %>% html_nodes(".row-fluid .pub-fil-ariane")%>% html_nodes("a")%>% html_text() 
  df[i,"commune"] <- name[4]
  results <- webpage %>% html_nodes("table") %>%html_table(header=T)
  tab1 <- results[[3]][,1:2]
  colnames(tab1) <- c("nom","voix")
  tab1 <- tab1[order(tab1$nom, decreasing=FALSE),]
  scores <- t(tab1)[2,]
  scores <- as.numeric(gsub("\\D", "", scores))
  tab2 <- results[[4]][,1:2]
  scores
  votes <- as.numeric(gsub("\\D", "", t(tab2)[2,]))
  scores <- c(scores,votes)
  df[i,3:19] <- scores
}


# Retrouver le code INSEE des communes
results2017 <- df
results2017$id  <- gsub(".html+$", "", results2017$id)
results2017$id  <- gsub("056AR", "1", results2017$id )
results2017$id  <- substr(results2017$id , 6, 100)
head(results2017)

# Export du fichier
write.csv(results2017,file = "data/results2017_all.csv", row.names = F)

