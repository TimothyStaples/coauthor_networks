rm(list=ls())

library(readxl)
library(d3r)
library(jsonlite)

setwd("/home/timothy/Dropbox/Tim/CV/collabNetwork")
myName = "Timothy L. Staples"

myPapers <- read.csv("wos2.csv", stringsAsFactors = FALSE)
myCoAuth <- sort(unique(unlist(strsplit(myPapers$Author.Full.Names, "; ",))))
paste0("AU = (", paste0(myCoAuth, collapse = ") OR ("), ")")
# cycle through wos subfolder to import 500 paper blocks
wosFiles <- list.files(path="./wos", include.dirs=TRUE, pattern=".xls")

authorList <- strsplit(myPapers$Author.Full.Names, "; ")
coAuthPapers <- do.call("rbind", lapply(1:length(authorList), function(n1){
  data.frame(auth = authorList[[n1]],
             pID = n1,
             pubYear = myPapers$Publication.Year[n1])
}))

coAuthPapers$surname <- substr(coAuthPapers$auth,
                               1, regexpr(", ", coAuthPapers$auth)-1)
coAuthPapers$first <- substr(coAuthPapers$auth,
                               regexpr(", ", coAuthPapers$auth)+2, 
                             nchar(as.character(coAuthPapers$auth)))
coAuthPapers$full <- paste0(coAuthPapers$first, " ", coAuthPapers$surname)

# now make me a year
myPubYears <- sort(unique(coAuthPapers$pubYear[coAuthPapers$full  %in% c(myName, "Timothy Staples")]))
coAuthPapers$full[coAuthPapers$full %in% c(myName, "Timothy Staples")] = coAuthPapers$pubYear[coAuthPapers$full  %in% c(myName, "Timothy Staples")]

coAuthMat <- do.call("rbind", 
                     tapply(coAuthPapers$full, 
                            coAuthPapers$pID, 
                            function(x){expand.grid(x, x)}, simplify=FALSE))
coAuthMat <- table(coAuthMat$Var1, coAuthMat$Var2)

aTab <- data.frame(source = rep(rownames(coAuthMat), ncol(coAuthMat)),
                   target = rep(colnames(coAuthMat), each=nrow(coAuthMat)),
                   count=as.vector(coAuthMat), stringsAsFactors = FALSE)

#remove any edge not with me
aTab <- aTab[aTab$source %in% unique(coAuthPapers$pubYear) |
             aTab$target %in% unique(coAuthPapers$pubYear),]

for(n in 1:(length(myPubYears)-1)){

  aTab$count[aTab$source %in% myPubYears[n] & 
             aTab$target %in% myPubYears[n+1]] = 1
  
}

aTab <- aTab[aTab$source != aTab$target,]
aTab <- aTab[aTab$count > 0,]

aTab$primary <- ifelse((aTab$source %in% myPubYears | aTab$target %in% myPubYears), 
                       "#324158", "#8C96A6")

aTab$width <- ifelse(aTab$source %in% myPubYears | aTab$target %in% myPubYears, 2, 0.5)
aTab$width[aTab$source %in% myPubYears & aTab$target %in% myPubYears] = 7.5

countWithMe <- sapply(split(aTab, f=aTab$target), function(x){
  if(x$target %in% myPubYears){return(4)}
  sum(x$count[x$source %in% c(myPubYears)])
})

# convert table into graph
library(igraph)
network=graph_from_data_frame(d=aTab, directed=F)

V(network)$count = countWithMe[match(V(network)$name, names(countWithMe))]
V(network)$count <- ifelse(V(network)$count == 0, 5, 3.5*V(network)$count)
# V(network)$count[V(network)$name == myName] = 15

V(network)$primary = "#324158"
V(network)$primary[V(network)$name %in% myPubYears] = "white"

V(network)$label = V(network)$name

V(network)$shadow = "rgb(255, 255, 255) 2px 0px 0px, rgb(255, 255, 255) 1.75517px 0.958851px 0px, rgb(255, 255, 255) 1.0806px 1.68294px 0px, rgb(255, 255, 255) 0.141474px 1.99499px 0px, rgb(255, 255, 255) -0.832294px 1.81859px 0px, rgb(255, 255, 255) -1.60229px 1.19694px 0px, rgb(255, 255, 255) -1.97999px 0.28224px 0px, rgb(255, 255, 255) -1.87291px -0.701566px 0px, rgb(255, 255, 255) -1.30729px -1.51361px 0px, rgb(255, 255, 255) -0.421592px -1.95506px 0px, rgb(255, 255, 255) 0.567324px -1.91785px 0px, rgb(255, 255, 255) 1.41734px -1.41108px 0px, rgb(255, 255, 255) 1.92034px -0.558831px 0px"
V(network)$shadow[V(network)$name %in% myPubYears] = "rgb(50, 65, 88) 2px 0px 0px, rgb(50, 65, 88) 1.75517px 0.958851px 0px, rgb(50, 65, 88) 1.0806px 1.68294px 0px, rgb(50, 65, 88) 0.141474px 1.99499px 0px, rgb(50, 65, 88) -0.832294px 1.81859px 0px, rgb(50, 65, 88) -1.60229px 1.19694px 0px, rgb(50, 65, 88) -1.97999px 0.28224px 0px, rgb(50, 65, 88) -1.87291px -0.701566px 0px, rgb(50, 65, 88) -1.30729px -1.51361px 0px, rgb(50, 65, 88) -0.421592px -1.95506px 0px, rgb(50, 65, 88) 0.567324px -1.91785px 0px, rgb(50, 65, 88) 1.41734px -1.41108px 0px, rgb(50, 65, 88) 1.92034px -0.558831px 0px"

# add coordinates for each year
yearVs <- !is.na(as.numeric(V(network)$name))
yearYs <- seq(-length(yearVs), length(yearVs), len=sum(yearVs))
V(network)$cat = 0
V(network)$cat[yearVs] = yearYs
V(network)$catStrength = as.numeric(yearVs)

url <- read.csv("url.csv")

V(network)$url = NA
V(network)$url[match(url$name,
                     V(network)$name)] = as.character(url$url)

V(network)$hasurl <- !is.na(V(network)$url)

V(network)$offlabel = V(network)$name
V(network)$offlabel[V(network)$name %in% myCoAuth]=""
V(network)$offlabel[match(url$name,
                       V(network)$name)]= as.character(url$label)

V(network)$width = 0
V(network)$width[V(network)$name %in% myPubYears] = 5
V(network)$strokeCol = "#324158"
V(network)$strokeCol[V(network)$name %in% myPubYears] = "#324158"

# Transform it in a JSON format for d3.js

data_json <- d3_igraph(network)

# Save this file
write(data_json, "/home/timothy/Dropbox/Tim/CV/collabNetwork/data.json")

# trying to plumb Google Scholar (API keeps thinking I'm a bot :()
# 
# 
# install.packages("devtools")
# install.packages("d3r")
# 
# library(devtools)
# install_github("jkeirstead/scholar")
# library(scholar)
# 
# # start by finding your ID
# myName <- "Timothy L Staples"
# 
# myID <- get_scholar_id(last_name = "Staples", 
#                        first_name = "Timothy", 
#                        affiliation = NA)
# 
# # now get your publications
# myPubs <- get_publications(id=myID, sortby="year", pagesize=100)
# 
# # sort through your papers to get full author lists
# myCoA <- get_complete_authors(id = myID, pubid = myPubs$pubid, initials = FALSE)
# myCoAinit <- get_complete_authors(id = myID, pubid = myPubs$pubid, initials = TRUE)
# 
# myCoAsave <- myCoA
# myCoAinitsave <- myCoAinit
# # split these into separate strings
# myCoAv <- unlist(strsplit(myCoA, ", "))
# myCoAinit <- unlist(strsplit(myCoAinit, ", "))
# myCoAvunique <- myCoAv[!duplicated(myCoAinit)]
# 
# myCoAsur <- substr(myCoAvunique,
#                    sapply(gregexpr(" ", myCoAvunique), function(x){rev(x)[1]})+1,
#                    nchar(myCoAvunique))
# myCoAfirst <- substr(myCoAvunique, 1, 
#                      sapply(gregexpr(" ", myCoAvunique), function(x){rev(x)[1]})-1)
# 
# # now cycle through each co-author, looking first for an initialled profile,
# # and then for non-initialled
# n<-5
# aTab <- do.call("rbind", lapply(1:length(myCoAvunique), function(n){
#   
#   Asur <- myCoAsur[n]
#   Afirst <- myCoAfirst[n]
#   print(paste0("Looking for... ", paste(Afirst, Asur)))
#   
#   # this function seems to need both first and last names to find stuff
#   
#   Aid <- try(get_scholar_id(first_name = Afirst,
#                             last_name = Asur),
#              silent=TRUE)
#   
#   # try removing initials
#   if(class(Aid) == "try-error" & !grepl(" ", Afirst)){
#     print("...No Scholar ID")
#     return(NULL)
#   }
#   
#   if(class(Aid) == "try-error" & grepl(" ", Afirst)){
#     Aid = try(get_scholar_id(first_name = substr(Afirst, 1, regexpr(" ", Afirst)-1),
#                              last_name = Asur),
#               silent=TRUE)
#   }
#   
#   if(class(Aid) == "try-error"){
#     print("...No Scholar ID")
#     return(NULL)
#   }
#   
#   # get all paper Ids from authors (the 100 most recent). This sometimes
#   # fails if there's too many for an author (might be a workaround to loop
#   # through slowly)
#   APubs <- try(get_publications(id=Aid, sortby="year", pagesize=10, flush=TRUE),
#                silent=TRUE)
#   
#   if(class(APubs) == "try-error"){
#     print("...author has too many papers!")
#     return(NULL)
#   }
#   
#   # now get unique list of authors, only keeping them if they're in your
#   # coauthor list (we need to keep the network reasonably sized somehow!)
#   AcoA <- get_complete_authors(id=Aid, pubid=APubs$pubid, initials = FALSE)
#   AcoAinit <- get_complete_authors(id=Aid, pubid=APubs$pubid, initials = TRUE)
#   
#   # match co-authors to my co-authors
#   ACoAv <- unlist(strsplit(AcoA, ", "))
#   ACoAinit <- unlist(strsplit(AcoAinit, ", "))
#   ACoAv <- ACoAv[ACoAv %in% myCoAv]
#   
#   # make a table, removing self-references and references to me
#   ACoTab <- as.data.frame(table(ACoAv))
#   ACoTab <- ACoTab[!ACoTab[,1] %in% c(paste(Afirst, Asur), myName),]
#   ACoTab$A <- paste(Afirst, Asur)
#   
#   return(ACoTab)
#   
# }))
