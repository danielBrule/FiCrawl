if (!require(rjson)){
    install.packages("rjson")
    library(rjson)
}

# if (!require(XML)){
#     install.packages("XML")
#     library(XML)
# }

library(xml2)
library(RODBC)

setwd("C:/FiCrawl")


####################################################################################
####################################################################################
############################  Initialisation    ####################################
####################################################################################
####################################################################################

fileName <- paste("data/ParsedSitemapActu-", Sys.Date()-1, ".csv", sep = "")
parsedSitemapActu <- read.csv(fileName)
#todo : get info from DB


####################################################################################
####################################################################################
############################  Helper    ############################################
####################################################################################
####################################################################################


f_insertError <- function(query, exitValue, id, i){
    cat(paste(query, "\n"), file="SQLError_parseComment.txt", append = TRUE)
    cat(paste(exitValue, "\n\n"), file="SQLError_parseComment.txt", append = TRUE)
    print(paste("ERROR ", id, " - ", i, sep = ""))
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

f_parseCommentContentNode <- function(node, parentID){
    #node <- curNode
    # node <- xml_child(nodeAnswer,3)
    #parentID <- 0
    
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
    
    #date comment 
    nodeDate <- xml_child(nodeCommentContent, 3)
    dateComment <- xml_text(xml_child(nodeDate, 1))
    dateComment <- as.character(as.POSIXct(strptime(dateComment,format='Le %d/%m/%Y à %H:%M')))

    hasChild <- ifelse(xml_length(node) == 3, TRUE, FALSE)

    #
    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
        isJournaliste = as.character(),
        hasChild = as.character(),
        stringsAsFactors = FALSE
    )
    
    output <- rbind(output, 
                    setNames(data.frame(commentID, userID, comment, parentID, dateComment, isJournaliste, hasChild),
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
    #jsonNode <- fromJSON(file = "data/Comment.json")[[2]]
    #jsonNode <- data[[2]]
    
    html <- read_html(jsonNode)
    nodes <- xml_child(html)
    nodes <- xml_children(nodes)

    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
        isJournaliste = as.character(),
        hasChild = as.character(),
        stringsAsFactors = FALSE
    )
    
    for(i in 1:length(nodes)){
        curNode <- nodes[[i]]
        output <- rbind(output,
                        f_parseCommentContentNode(curNode, 0))
    }
    return(output)
}

f_getComments<- function(ID){
    # ID <- "20180607ARTFIG00353"
    #ID <- articleID
    url <- f_buildCommentURL(ID, 0)
    
    #todo read json online 
    data <- fromJSON(file = url)
    #data <- fromJSON(file = "data/Comment.json")   
 
    if(data[[1]] == "empty_comment")
        return(NULL)
     
    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
        isJournaliste = as.character(),
        hasChild = as.character(),
        stringsAsFactors = FALSE
    )
    
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


output <- data.frame(
    commentID = as.character(),
    userID = as.character(), 
    comment = as.character(), 
    parentComment = as.character(), 
    dateComment = as.character(),
    article = as.character(),
    hasChild = as.character(),
    stringsAsFactors = FALSE
)


connect <- odbcConnect("FiCrawl")
query <-   "SELECT articleID FROM articles 
            WHERE 
                publicationdate < DATE_ADD(NOW(), INTERVAL -7 DAY) AND
                HasBeenParsed = 0" 
ArticlesIDList <- sqlQuery(connect, query, errors = TRUE )$articleID



start_time <- Sys.time()
for (i in 1:length(ArticlesIDList)){
    articleID <-  ArticlesIDList[i]
#    articleID <- "20180607ARTFIG00353"
    output <- rbind(output, 
                    f_getComments(articleID))
    print(i)
    
    query <- paste(
        'UPDATE  articles set HasBeenParsed = 1 where articleID = ',
        '"', articleID,'"',         
        sep = '')
    exitValue <- sqlQuery(connect, query, errors = TRUE )
    if (!identical(exitValue, character(0))){
        f_insertError(query, exitValue, 0, i)
    } 
    
    
    Sys.sleep(10)
}
end_time <- Sys.time()
end_time - start_time

close(connect)



fileName <- paste("data/comments-", Sys.Date()-1, ".csv", sep = "")

write.csv(output, fileName)








####################################################################################
####################################################################################
############################  Update DB ############################################
####################################################################################
####################################################################################

#output <- read.csv("data/comments-2018-06-18.csv")
#output$hasChild <- FALSE



f_updateUsers <- function(output){
    users <- sort(unique(output$userID))
    
    existingUsers <- sort(sqlQuery(connect, "select * from users")$UserID)
    '%nin%' <- Negate('%in%')
    users <- users[users %nin% existingUsers]
    if (length(users) > 1 ){
        for (i in 1:length(users)){
            if (users[i] != "NA"){
                query <- paste('insert into users (UserID) values ("' , users[i],'")', sep = '')
                exitValue <- sqlQuery(connect, query)
                if (!identical(exitValue, character(0))){
                    f_insertError(query, exitValue, 1, i)              
                }
            }
        }
    }
}

f_updateComments <- function(output){
    i<-401
    for (i in 1:nrow(output)){
        query <- paste(
            'insert into  comments (CommentId, Comment, CommentDate, 
                IsJournaliste, HasChild, UserId, ArticleID, ParentCommentId) values (' , 
            '"', output$commentID[i],'",', 
            '"', gsub('"', '', output$comment[i]),'",', 
            '"', as.POSIXlt(output$dateComment[i], origin = "1970-01-01"), '",',
            '"', ifelse(output$isJournaliste[i], 1, 0), '",',
            '"', ifelse(output$hasChild[i], 1, 0), '",',
            '"', output$userID[i],'",', 
            '"', output$article[i],'",', 
            ifelse(output$parentComment[i] == 0, "NULL",paste('"',output$parentComment[i],'"', sep = "")), 
            ')',
            sep = '')
        exitValue <- sqlQuery(connect, query, errors = TRUE )
        if (!identical(exitValue, character(0))){
            f_insertError(query, exitValue, 2, i)
        }
    }
}


output <- unique(output)

connect <- odbcConnect("FiCrawl")
f_updateUsers(output)
f_updateComments(output)


close(connect)