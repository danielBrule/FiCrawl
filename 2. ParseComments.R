if (!require(rjson)){
    install.packages("rjson")
    library(rjson)
}

# if (!require(XML)){
#     install.packages("XML")
#     library(XML)
# }

library(xml2)

setwd("C:/FiCrawl")


####################################################################################
####################################################################################
############################  Initialisation    ####################################
####################################################################################
####################################################################################

parsedSitemapActu <- read.csv("data/ParsedSitemapActu.csv")
#todo : get info from DB

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
    #parentID <- 0
    
    #comment ID 
    commentID <- xml_attr(node, "class")
    commentID <-strsplit(commentID, split= " ")[[1]][2]
    
    nodeCommentContent <- xml_children(node)[[2]]
    
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



    #
    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
        stringsAsFactors = FALSE
    )
    output <- rbind(output, 
                    setNames(data.frame(commentID, userID, comment, parentID, dateComment),
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
    
    html <- read_html(jsonNode)
    nodes <- xml_child(html)
    nodes <- xml_children(nodes)

    output <- data.frame(
        commentID = as.character(),
        userID = as.character(), 
        comment = as.character(), 
        parentComment = as.character(), 
        dateComment = as.character(),
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
    # ID <- "20180604ARTFIG00006"
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
    stringsAsFactors = FALSE
)

start_time <- Sys.time()
for (i in 1:nrow(parsedSitemapActu)){
    articleID <- parsedSitemapActu[i,]$ID
    output <- rbind(output, 
                    f_getComments(articleID))
    print(i)
    Sys.sleep(10)
}
end_time <- Sys.time()

end_time - start_time


