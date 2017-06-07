## Chargement des packages

library(xml2)
library(rvest)

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
links

# AUTRES DEPARTEMENTS
for(i in 2:length(departements)){
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
  w <- read_html(x = url)
  l <- w %>% html_nodes(".offset2") %>% html_nodes("a")%>%  html_attr("href")
  #l <- l[4:(length(l)-1)]
  l <- gsub("../../011/", "", l)
  start <- length(grep("[A-Z]", l))+1
  stop <-   length(l)
  l <- l[start:stop]
  #l <- strsplit(l2[i],"/")[[1]][1]
  links <- c(links,l)
} 



################################################"

# Création d'un premier dataframe avec les resultats
df <- data.frame()
for(i in 1:length(links)){
  df[i,"link"] <- links[i]
  url <- paste (site,links[i],sep="/")
  webpage <- read_html(x = url)
  name <- webpage %>% html_nodes(".row-fluid .pub-fil-ariane")%>% html_nodes("a")%>% html_text() 
  df[i,"name"] <- name[4]
  results <- webpage %>% html_nodes("table") %>%html_table(header=T)
  
  for (j in 1:11){
    if (results[[3]][j,1] == "M. Jean-Luc MÉLENCHON"){index <- j}
  }
  df[i,"nb_jlm2017"] <- as.numeric(gsub("\\D", "", results[[3]][index,2]))
  df[i,"tx_jlm2017"] <- as.numeric(gsub("\\D", ".", results[[3]][index,4]))
  df[i,"abstention"] <- as.numeric(gsub("\\D", "", results[[2]][2,2]))
  df[i,"exprimés"] <- as.numeric(gsub("\\D", "", results[[2]][6,2]))
}


# Retrouver le code INSEE des communes


# pour l'année 2017
results2017$id  <- gsub(".html+$", "", results2017$link)
head(results2017)
results2017$id  <- gsub("056AR", "1", results2017$id )
head(results2017)
results2017$id  <- substr(results2017$id , 6, 100)
head(results2017)
results2017 <- results2017[,c("id","name","nb_jlm2017","tx_jlm2017","abstention", "exprimés")]
head(results2017)


# Export du fichier
write.csv(results2017,file = "data/results2017.csv", row.names = F)




# obtenir les données de 2012

site <- "http://www.interieur.gouv.fr/Elections/Les-resultats/Presidentielles/elecresult__PR2012/(path)/PR2012/011"
departements <- c("075","077","078","091","092","093","094","095")

# PARIS
dep <- departements[1]
page <- paste(dep,"html",sep=".")
url <- paste (site,dep,page,sep="/")
webpage <- read_html(x = url)
l <- webpage %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
l <- l[4:(length(l)-1)]
l <- paste(dep,l,sep="/")
links <- l


# AUTRES DEPARTEMENTS
for(i in 2:length(departements)){
  dep <- departements[i]
  page <- paste(dep,"html",sep=".")
  url <- paste (site,dep,page,sep="/")
  webpage <- read_html(x = url)
  l <- webpage %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
  l <- l[3:(length(l)-1)]
  l <- paste(dep,l,sep="/")
  if (i ==2){l2 <- l} else {l2 <- c(l2,l)}
}

for (i in 1:length(l2)) {
  url <- paste(site,l2[i],sep="/")
  w <- read_html(x = url)
  l <- w %>% html_nodes("#content-wrap") %>% html_nodes("a")%>%  html_attr("href")
  l <- l[4:(length(l)-1)]
  start <- length(grep("[A-Z]", l))+1
  stop <-   length(l)
  l <- l[start:stop]
  toto <- strsplit(l2[i],"/")[[1]][1]
  l <- paste(toto,l,sep="/")
  links <- c(links,l)
} 


################################################"

# Création d'un premier dataframe avec les resultats
df <- data.frame()
for(i in 1:length(links)){
  df[i,"link"] <- links[i]
  url <- paste (site,links[i],sep="/")
  webpage <- read_html(x = url)
  name <- webpage %>% html_nodes("#content-wrap") %>% html_node("strong")%>%html_text()
  name <- strsplit(as.character(name),split = ">")[[1]][4]
  df[i,"name"] <- gsub("^.","",name)
  results <- webpage %>% html_nodes("table") %>%html_table(header=T)
  df[i,"nb_jlm2012"] <- as.numeric(gsub("\\D", "", results[[4]][4,2]))
  df[i,"tx_jlm2012"] <- as.numeric(gsub("\\D", ".", results[[4]][4,3]))
  df[i,"abstention"] <- as.numeric(gsub("\\D", "", results[[3]][2,2]))
  df[i,"exprimés"] <- as.numeric(gsub("\\D", "", results[[3]][5,2]))
}


# results2012 <- read.csv("data/results_comidf_2012.csv")


# Retrouver le code INSEE des communes

# pour l'année 2012
results2012$id <- gsub(".html+$", "", results2012$link)
results2012$id <- gsub("056AR", "1", results2012$id)
results2012$id <- substr(results2012$id, 6, 100)
results2012$id <- results2012$id
results2012 <- results2012[c("id","name","nb_jlm2012","tx_jlm2012","abstention", "exprimés")]
head(results2012)



# Export du fichier
write.csv(results2012,file = "data/results2012.csv", row.names = FALSE)



