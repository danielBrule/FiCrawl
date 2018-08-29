if (!require(rjson)){
    install.packages("rjson")
    library(rjson)
}

# if (!require(XML)){
#     install.packages("XML")
#     library(XML)
# }

library(xml2)
#library(RODBC)
library(odbc)
library(syuzhet)

setwd("C:/FiCrawl")




####################################################################################
####################################################################################
############################  Helper    ############################################
####################################################################################
####################################################################################


f_insertError <- function(query, exitValue, id, i){
#    cat(paste(query, "\n"), file="SQLError_parseComment.txt", append = TRUE)
    cat(paste(exitValue, "\n\n"), file="SQLError_parseComment.txt", append = TRUE)
    print(paste("ERROR ", id, " - ", i, sep = ""))
}

f_executeSelectQuery <- function(connect, query){
    result <- dbSendQuery(connect, query)
    output <- dbFetch(result)
    dbClearResult(result)
    return(output)
    
}


f_executeDataManipulationQuery <- function(connect, query){
    result <-tryCatch({
        res <- dbSendStatement(connect, query)
        dbClearResult(res)
        }, error = function(e) {
            f_insertError(query, e, 0, 0)
        })
                      

}

####################################################################################
####################################################################################
############################  Initialisation    ####################################
####################################################################################
####################################################################################

f_buildCommentURL<- function(ID, pageID){
    output <- paste("http://www.lefigaro.fr/async/comments/sdv/", 
                    ID, 
                    "?page=", 
                    pageID,
                    "&actions=0",
                    sep = "")
    return(output)
}


f_buildEmptyOutput <- function(){
    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
        isJournaliste = as.character(),
        hasChild = as.character(),
        sentiment = as.integer(),
        stringsAsFactors = FALSE
    )
    return(output)
}

f_parseCommentContentNode <- function(node, parentID){
    #comment ID 
    commentID <- xml_attr(node, "class")
    commentID <-strsplit(commentID, split= " ")[[1]][2]
    
    isJournalisteAttribute <- xml_attr(xml_children(node), "class")[1]
    
    if(isJournalisteAttribute == "fig-comment__journalist-wrapper"){
        nodeCommentContent <- xml_children(xml_children(node)[[1]])[[2]]
        isJournaliste <- TRUE
    } else{
        nodeCommentContent <- xml_children(node)[[2]]
        isJournaliste <- FALSE
    }
    #user Info
    nodeUser <- xml_child(nodeCommentContent, 1)
    userID <- xml_attr(nodeUser, "href")
    
    #comment 
    nodeComment <- xml_child(nodeCommentContent, 2)
    comment <- xml_text(nodeComment)

    #sentiment 
    sentiment <- get_sentiment(as.character(comment), method="nrc", lang = "french")
    
    #date comment 
    nodeDate <- xml_child(nodeCommentContent, 3)
    dateComment <- xml_text(xml_child(nodeDate, 1))
    dateComment <- as.character(as.POSIXct(strptime(dateComment,format='Le %d/%m/%Y à %H:%M')))

    hasChild <- ifelse(xml_length(node) == 3, TRUE, FALSE)

    #
    output <-f_buildEmptyOutput()
    
    output <- rbind(output, 
                    setNames(data.frame(commentID, userID, comment, parentID, dateComment, isJournaliste, hasChild, sentiment),
                             names(output)))
    
    if (xml_length(node) == 3){
        nodeAnswer <- xml_child(node,3)
        for (i in 1:xml_length(nodeAnswer)){
            output <- rbind(output, 
                            f_parseCommentContentNode(xml_child(nodeAnswer,i), commentID))
        }
        
    }   
    
    
    return(output)
}

f_parseJsonComments <- function (jsonNode){
    html <- read_html(jsonNode)
    nodes <- xml_child(html)
    nodes <- xml_children(nodes)

    output <- f_buildEmptyOutput()
    
    for(i in 1:length(nodes)){
        curNode <- nodes[[i]]
        output <- rbind(output,
                        f_parseCommentContentNode(curNode, 0))
    }
    return(output)
}

f_getComments<- function(ID){
    url <- f_buildCommentURL(ID, 0)
    
    #todo read json online 
    data <- fromJSON(file = url)
 
    if(data[[1]] == "empty_comment")
        return(NULL)
     
    output <- f_buildEmptyOutput()
    
    cpt <- 1
    while(data[["is_last_page"]] == FALSE){
        output <- rbind(output, 
                        f_parseJsonComments(data[[2]]))
        
        url <- f_buildCommentURL(ID, cpt)
        cpt <- cpt + 1 
        data <- fromJSON(file = url)
        Sys.sleep(10)
    }
    output <- rbind(output, 
                    f_parseJsonComments(data[[2]]))
    output$article <- ID
    return(output)

}




####################################################################################
####################################################################################
############################  Initialisation    ####################################
####################################################################################
####################################################################################


f_downloadAndParseComment <- function(){
    output <- f_buildEmptyOutput()
    
    connect <- dbConnect(odbc::odbc(), "FiCrawl")
    query <-   "SELECT articleID FROM articles 
                WHERE 
                    publicationdate < DATE_ADD(NOW(), INTERVAL -7 DAY) AND
                    HasBeenParsed = 0" 
    ArticlesIDList <- f_executeSelectQuery(connect, query)$articleID
    print(paste("nb of articles to crawl: ", length(ArticlesIDList)))
    ArticlesIDList <- head(ArticlesIDList, n=500)
    print(paste("nb of articles crawled: ", length(ArticlesIDList)))
    start_time <- Sys.time()
    
    for (i in 1:length(ArticlesIDList)){
        articleID <-  ArticlesIDList[i]
    #    articleID <- "20180503ARTFIG00311"
        print(paste ("gather", articleID, ":",i, "/",length(ArticlesIDList)))
        output <- rbind(output, 
                        f_getComments(articleID))
        Sys.sleep(10)
    }
    
    
    for (i in 1:length(ArticlesIDList)){
        articleID <-  ArticlesIDList[i]
        print(paste ("update", articleID, ":",i, "/",length(ArticlesIDList)))
        query <- paste(
            'UPDATE  articles set HasBeenParsed = 1 where articleID = ',
            '"', articleID,'"',         
            sep = '')
        f_executeDataManipulationQuery(connect, query )
    }
    
    end_time <- Sys.time()
    end_time - start_time
    
    dbDisconnect(connect)
    return(output)

}



output <- f_downloadAndParseComment()
fileName <- paste("data/comments-", Sys.Date()-1, ".csv", sep = "")
write.csv(output, fileName)

#output <- read.csv("data/comments-2018-07-15.csv")




####################################################################################
####################################################################################
############################  Update DB ############################################
####################################################################################
####################################################################################



f_updateUsers <- function(output){
    users <- sort(unique(output$userID))
    
    existingUsers <- sort(f_executeSelectQuery(connect, "select * from users")$UserID)
    '%nin%' <- Negate('%in%')
    users <- users[users %nin% existingUsers]
    if (length(users) > 0 ){
        for (i in 1:length(users)){
            if (users[i] != "NA"){
                query <- paste('insert into users (UserID) values ("' , users[i],'")', sep = '')
                f_executeDataManipulationQuery(connect, query)
            }
        }
    }
}

f_updateComments <- function(output){
    comments <- sort(unique(output$commentID))
    
    existingComments <- sort(f_executeSelectQuery(connect, "select CommentID from comments")$CommentID)
    '%nin%' <- Negate('%in%')
    comments <- comments[comments %nin% existingComments]
    
    
    print(paste("total comments to update: ", length(comments), "/", nrow(output)))
    if (length(comments) > 0 ){
        for (i in 1:length(comments)){
            data <- output[output$commentID == comments[i],]
            print(paste("comment", comments[i], "#", i,"/",length(comments)))
            query <- paste(
                'insert into  comments (CommentId, Comment, CommentDate, 
                    IsJournaliste, HasChild, UserId, ArticleID, ParentCommentId, sentiment) values (' , 
                '"', data$commentID[1],'",', 
                '"', gsub('"', '', data$comment[1]),'",', 
                '"', as.POSIXlt(data$dateComment[1], origin = "1970-01-01"), '",',
                '"', ifelse(data$isJournaliste[1], 1, 0), '",',
                '"', ifelse(data$hasChild[1], 1, 0), '",',
                '"', data$userID[1],'",', 
                '"', data$article[1],'",', 
                ifelse(data$parentComment[1] == 0, "NULL, ",paste('"',data$parentComment[1],'", ', sep = "")), 
                data$sentiment[1],
                ')',
                sep = '')
            f_executeDataManipulationQuery(connect, query)
        }
    }
}


output <- unique(output)

connect <-dbConnect(odbc::odbc(), "FiCrawl")
f_updateUsers(output)
f_updateComments(output)


dbDisconnect(connect)