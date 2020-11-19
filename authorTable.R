# input my papers
rm(list=ls())

library(readxl)

setwd("/home/timothy/Dropbox/Tim/CV/collabNetwork")

# cycle through wos subfolder to import 500 paper blocks
wosFiles <- list.files(path="./wos", include.dirs=TRUE)
coAuthPapers <- do.call("rbind", lapply(1:length(wosFiles), function(n){
  temp <- as.data.frame(read_excel(paste0("./wos/", wosFiles[n])))
  
  authorList <- strsplit(temp$`Author Full Names`, "; ")
  do.call("rbind", lapply(1:length(authorList), function(n1){
    data.frame(auth = authorList[[n1]],
               pID = n1)
  }))
  
  }))

coAuthPapers$surname <- substr(coAuthPapers$auth,
                               1, regexpr(", ", coAuthPapers$auth)-1)
coAuthPapers$first <- substr(coAuthPapers$auth,
                               regexpr(", ", coAuthPapers$auth)+2, nchar(coAuthPapers$auth))
coAuthPapers$full <- paste0(coAuthPapers$first, " ", coAuthPapers$surname)

# now make a co-author table
authors <- sort(unique(coAuthPapers$full))
coAuthMat <- matrix(0, nrow = length(authors) , ncol = length(authors),
                    dimnames=list(authors, authors))
coAuthTable <- sapply(authors, function(name){
  print(name)
  # get authored papers
  subPapers <- coAuthPapers[coAuthPapers$full == name,]
  subPapers <- coAuthPapers[coAuthPapers$pID %in% subPapers$pID,]
  
  coTable <- table(subPapers$full)
  
  coAuthMat[cbind(which(rownames(coAuthMat) == name),
                  match(subPapers$full, names(coTable)))] = coTable
  
  })


myPapers <- read.csv("file:///home/timothy/Dropbox/Tim/CV/collabNetwork/savedrecs.csv")

coAuth <- unique(unlist(strsplit(myPapers$Author.Full.Names, "; ")))
paste0(coAuth, collapse='" OR "')

# regardless, now we can make a co-author table and use it to populate a
# network plot in d3.js
aTab$Me = "Timothy L Staples"
aTab <- aTab[!agrepl("Timothy L Staples", aTab$myCoAv),]
aTab <- aTab[,c("Me", "myCoAv", "Freq")]
colnames(aTab) = c("source", "target", "count")
# convert table into graph
library(igraph)
network=graph_from_data_frame(d=aTab, directed=F)

E(network)$weight = aTab$count


# Transform it in a JSON format for d3.js
library(d3r)
data_json <- d3_igraph(network)

# Save this file
write(data_json, "/home/timothy/Dropbox/Tim/CV/collabNetwork/data.json")


# trying to plumb Google Scholar (API keeps thinking I'm a bot :()


install.packages("devtools")
install.packages("d3r")

library(devtools)
install_github("jkeirstead/scholar")
library(scholar)

# start by finding your ID
myName <- "Timothy L Staples"

myID <- get_scholar_id(last_name = "Staples", 
                       first_name = "Timothy", 
                       affiliation = NA)

# now get your publications
myPubs <- get_publications(id=myID, sortby="year", pagesize=100)

# sort through your papers to get full author lists
myCoA <- get_complete_authors(id = myID, pubid = myPubs$pubid, initials = FALSE)
myCoAinit <- get_complete_authors(id = myID, pubid = myPubs$pubid, initials = TRUE)

myCoAsave <- myCoA
myCoAinitsave <- myCoAinit
# split these into separate strings
myCoAv <- unlist(strsplit(myCoA, ", "))
myCoAinit <- unlist(strsplit(myCoAinit, ", "))
myCoAvunique <- myCoAv[!duplicated(myCoAinit)]

myCoAsur <- substr(myCoAvunique,
                   sapply(gregexpr(" ", myCoAvunique), function(x){rev(x)[1]})+1,
                   nchar(myCoAvunique))
myCoAfirst <- substr(myCoAvunique, 1, 
                     sapply(gregexpr(" ", myCoAvunique), function(x){rev(x)[1]})-1)

# now cycle through each co-author, looking first for an initialled profile,
# and then for non-initialled
n<-5
aTab <- do.call("rbind", lapply(1:length(myCoAvunique), function(n){
  
  Asur <- myCoAsur[n]
  Afirst <- myCoAfirst[n]
  print(paste0("Looking for... ", paste(Afirst, Asur)))
  
  # this function seems to need both first and last names to find stuff
  
  Aid <- try(get_scholar_id(first_name = Afirst,
                            last_name = Asur),
             silent=TRUE)
  
  # try removing initials
  if(class(Aid) == "try-error" & !grepl(" ", Afirst)){
    print("...No Scholar ID")
    return(NULL)
  }
  
  if(class(Aid) == "try-error" & grepl(" ", Afirst)){
    Aid = try(get_scholar_id(first_name = substr(Afirst, 1, regexpr(" ", Afirst)-1),
                             last_name = Asur),
              silent=TRUE)
  }
  
  if(class(Aid) == "try-error"){
    print("...No Scholar ID")
    return(NULL)
  }
  
  # get all paper Ids from authors (the 100 most recent). This sometimes
  # fails if there's too many for an author (might be a workaround to loop
  # through slowly)
  APubs <- try(get_publications(id=Aid, sortby="year", pagesize=10, flush=TRUE),
               silent=TRUE)
  
  if(class(APubs) == "try-error"){
    print("...author has too many papers!")
    return(NULL)
  }
  
  # now get unique list of authors, only keeping them if they're in your
  # coauthor list (we need to keep the network reasonably sized somehow!)
  AcoA <- get_complete_authors(id=Aid, pubid=APubs$pubid, initials = FALSE)
  AcoAinit <- get_complete_authors(id=Aid, pubid=APubs$pubid, initials = TRUE)
  
  # match co-authors to my co-authors
  ACoAv <- unlist(strsplit(AcoA, ", "))
  ACoAinit <- unlist(strsplit(AcoAinit, ", "))
  ACoAv <- ACoAv[ACoAv %in% myCoAv]
  
  # make a table, removing self-references and references to me
  ACoTab <- as.data.frame(table(ACoAv))
  ACoTab <- ACoTab[!ACoTab[,1] %in% c(paste(Afirst, Asur), myName),]
  ACoTab$A <- paste(Afirst, Asur)
  
  return(ACoTab)
  
}))
