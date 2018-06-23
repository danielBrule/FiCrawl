# if (!require(XML)){
#     install.packages("XML")
#     library(XML)
# } 
# if (!require(RODBC)){
#     install.packages("RODBC")
#     library(RODBC)
# } 
#install.packages("SnowballC")
#install.packages("tm")
library(xml2)
library(RODBC)
library(SnowballC)
library(tm)

setwd("C:/FiCrawl")

isTest <- FALSE


####################################################################################
####################################################################################
############################  Download sitemap actu ################################
####################################################################################
####################################################################################

fileURL <- "http://www.lefigaro.fr/sitemap_actu.xml"
destFile <- "sitemap_actu.xml"
destDir <- "data"
destFullPath <- paste(destDir, destFile, sep = "/")

if(!dir.exists(destDir)){
    dir.create(destDir)
}

if(isTest){
    if(!file.exists(destFullPath)){
        res <- download.file(fileURL,
                             destfile=destFullPath, 
                             method="auto")
        if(res != 0) {
            print("error while downloading file")
            quit(status = 1)
        }
    }
} else{
    res <- download.file(fileURL,
                         destfile=paste(destDir, destFile, sep = "/"), 
                         method="auto")
    if(res != 0) {
        print("error while downloading file")
        quit(status = 1)
    }
}

remove(list = c("destDir", "destFile", "fileURL","res"))


####################################################################################
####################################################################################
############################  read sitemap actu ####################################
####################################################################################
####################################################################################

f_parseSitemapActu <- function(rootNode){
    urlNode <- xml_children(rootNode)
    nbNode <- length(urlNode)
    output <- data.frame(ID = rep("", nbNode),
                         url = rep("", nbNode),
                         title = rep("", nbNode),
                         newspaper = rep("", nbNode),
                         keywords = rep("", nbNode),
                         publicationTime = rep("", nbNode),
                         modificationTime = rep("", nbNode),
                         stringsAsFactors = FALSE)
    
    for(i in 1:nbNode){
        curNode <- urlNode[i]
        
        url         <- xml_text(xml_find_all(curNode, ".//d1:loc"))
        title       <- xml_text(xml_find_all(curNode, ".//news:title"))
        newspaper   <- xml_text(xml_find_all(curNode, ".//news:name"))
        keywords    <- xml_text(xml_find_all(curNode, ".//news:keywords"))
        pubTime     <- xml_text(xml_find_all(curNode, ".//news:publication_date"))
        modTime     <- xml_text(xml_find_all(curNode, ".//d1:lastmod"))
        
        ID <- strsplit(url, "/")
        ID <- ID[[1]][length(ID[[1]])]
        ID <- strsplit(ID, "-")
        ID <- ID[[1]][2]

        
        pubTime <- as.POSIXct(strptime(pubTime,format='%Y-%m-%dT%H:%M:%S'))
        modTime <- as.POSIXct(strptime(modTime,format='%Y-%m-%dT%H:%M:%S'))
        
        output[i,] <- c(ID, url, title, newspaper, keywords, pubTime, modTime)
        
    }
    return(output)
    
}

rootNode <- read_xml(destFullPath)
ns <- xml_ns_rename(xml_ns(rootNode), d1 = "a")
parsedSitemapActu <- f_parseSitemapActu(rootNode)

remove(list=c("rootNode","ns", "f_parseSitemapActu","destFullPath"))

####################################################################################
####################################################################################
############################  read sitemap actu ####################################
####################################################################################
####################################################################################


parsedSitemapActu$isFlash <- grepl(pattern = "Flash", x = parsedSitemapActu$keywords)
parsedSitemapActu <- parsedSitemapActu[parsedSitemapActu$isFlash == FALSE,]
parsedSitemapActu <- subset(parsedSitemapActu, select = -c(isFlash))


fileName <- paste("data/ParsedSitemapActu-", Sys.Date(), ".csv", sep = "")
write.csv(parsedSitemapActu, fileName)




####################################################################################
####################################################################################
############################  Update DB ############################################
####################################################################################
####################################################################################


f_insertError <- function(query, exitValue, id, i){
    cat(paste(query, "\n"), file="SQLError_parseActu.txt", append = TRUE)
    cat(paste(exitValue, "\n\n"), file="SQLError_parseActu.txt", append = TRUE)
    print(paste("ERROR ", id, " - ", i, sep = ""))
}

f_cleanKeyword <- function(keyword){
    keywords <- tolower(keyword)
    keywords <- unlist(strsplit(keywords, ','))
    keywords <- data.frame(keyword = keywords, 
                           keywordStem =trimws(keywords),
                           stringsAsFactors = FALSE)
    if (nrow(keywords) == 0){
        return(NULL)
    }
    for (i in 1:nrow(keywords)){
        tmp <- iconv(keywords$keywordStem[i],from="UTF-8",to="ASCII//TRANSLIT")
        if(is.na(tmp)){
            tmp <- iconv(keywords$keywordStem[i],from="",to="ASCII//TRANSLIT")
        }
        tmp <- strsplit(tmp, ' ')
        tmp <- sapply(tmp, wordStem, language = "french")
        keywords$keywordStem[i] <- paste(tmp, collapse = " ")
    }
    keywords <- keywords [!duplicated(keywords[,c('keywordStem')]),]
    keywords <- keywords[order(keywords$keywordStem), ]
    return(keywords)
}

f_updateKeyword <- function(parsedSitemapActu, connect){
    keywords <- f_cleanKeyword(paste(parsedSitemapActu$keywords, collapse =","))

    existingKeywords <- sqlQuery(connect, "select * from keywords")
    existingKeywords <- existingKeywords[order(existingKeywords$KeywordID), ]
    '%nin%' <- Negate('%in%')
    keywords <- keywords[keywords$keywordStem %nin% existingKeywords$KeywordID, ]
    if (nrow(keywords) > 1 ){
        for (i in 1:nrow(keywords)){
            if (keywords$keyword[i] != "NA"){
                query <- paste('insert into keywords (KeywordID, FullKeyword) values (', 
                               '"', keywords$keywordStem[i],'",',
                               '"', keywords$keyword[i],'"', 
                               ')', 
                               sep = '')
                exitValue <- sqlQuery(connect, query)
                if (!identical(exitValue, character(0))){
                    f_insertError(query, exitValue, 1, i)              
                }
            }
        }
    }
}


f_updateNewArticle <- function(newArticle, connect){
    if (nrow(newArticle) == 0){
        return()
    }
    for (i in 1:nrow(newArticle)){
        query <- paste(
            'insert into  articles (ArticleID, Title, PublicationDate, 
            LastModificationDate, NewsPaper, URL, HasBeenParsed) values (' , 
            '"', newArticle$ID[i],'",', 
            '"', newArticle$title[i],'",', 
            '"', as.POSIXlt(as.numeric(newArticle$publicationTime[i]), origin = "1970-01-01"), '",',
            '"', as.POSIXlt(as.numeric(newArticle$modificationTime[i]), origin = "1970-01-01"), '",',
            '"', "leFigaro",'",', 
            '"', newArticle$url[i],'",', 
            '"', 0, '"', 
            ')',
            sep = '')
        exitValue <- sqlQuery(connect, query, errors = TRUE )
        if (!identical(exitValue, character(0))){
            f_insertError(query, exitValue, 2, i)
        }
        keywords <- f_cleanKeyword(newArticle$keywords[i])
        if (!is.null(keywords)){
            for (j in 1:nrow(keywords)){
                if (keywords$keywordStem[j] != "NA"){
                    query <- paste(
                        'insert into articlekeywords (ArticleID, keywordid) values (' , 
                        '"', newArticle$ID[i],'",', 
                        '"', keywords$keywordStem[j], '"', 
                        ')',
                        sep = '')
                    exitValue <- sqlQuery(connect, query)
                    if (!identical(exitValue, character(0))){
                        f_insertError(query, exitValue, 3, paste(i, j))
                    }
                }
            }
            
        }
        
    }

}

f_updateExistingArticle <- function(exitingArticle, connect){
    if (nrow(exitingArticle) == 0){
        return()
    }
    for (i in 1:nrow(exitingArticle)){
        query <- paste(
            'UPDATE  articles set HasBeenParsed = 0, ',
            'LastModificationDate = ' , 
            '"', as.POSIXlt(as.numeric(exitingArticle$modificationTime[i]), origin = "1970-01-01"), '" ',
            "where articleID = ",
            '"', exitingArticle$ID[i],'"',         
            sep = '')
        exitValue <- sqlQuery(connect, query, errors = TRUE )
        if (!identical(exitValue, character(0)) & 
            !identical(exitValue, "No Data")){
            f_insertError(query, exitValue, 4, i)
        } 
    }
}

f_updateArticle <- function(parsedSitemapActu, connect){
    existingArticlesID <- sqlQuery(connect, "select articleID from articles")$articleID
    '%nin%' <- Negate('%in%')
    newArticle <- parsedSitemapActu[parsedSitemapActu$ID %nin% existingArticlesID, ]
    exitingArticle <- parsedSitemapActu[parsedSitemapActu$ID %in% existingArticlesID, ]
    f_updateNewArticle(newArticle, connect)
    f_updateExistingArticle(exitingArticle, connect)
}

#parsedSitemapActu <- read.csv("data/ParsedSitemapActu-2018-06-18.csv")

f_updateDB <- function (parsedSitemapActu, connect){
    connect <- odbcConnect("FiCrawl")
    f_updateKeyword(parsedSitemapActu, connect) 
    f_updateArticle(parsedSitemapActu, connect)
    close(connect)
}

f_updateDB(parsedSitemapActu)


f_parseDic(){
    files <- list.files("./data")
    files <- files[grep("ParsedSitemapActu.*", files)]
    
    for (i in 1:length(files)){
        print(i)
        fileName <- paste("./data/", files[i], sep="")
        parsedSitemapActu <- read.csv (fileName)
        f_updateDB(read.csv (fileName))
    }

}




