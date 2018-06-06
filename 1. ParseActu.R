# if (!require(XML)){
#     install.packages("XML")
#     library(XML)
# } 

library(xml2)




setwd("C:/FiCrawl")

isTest <- TRUE


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
#rootNode <- getNodeSet(rootNode, "//urlset")
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



