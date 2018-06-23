CREATE DATABASE `ficrawl` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */;


CREATE TABLE `keywords` (
  `KeywordID` char(200) NOT NULL,
  `FullKeyword` varchar(200) NOT NULL,
  PRIMARY KEY (`KeywordID`),
  UNIQUE KEY `KeywordID_UNIQUE` (`KeywordID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;



CREATE TABLE `users` (
  `UserID` varchar(150) NOT NULL,
  PRIMARY KEY (`UserID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;


CREATE TABLE `articles` (
  `ArticleID` varchar(19) NOT NULL,
  `Title` varchar(500) NOT NULL,
  `PublicationDate` datetime NOT NULL,
  `LastModificationDate` datetime NOT NULL,
  `NewsPaper` varchar(10) NOT NULL,
  `URL` varchar(500) NOT NULL,
  `HasBeenParsed` tinyint(4) NOT NULL,
  PRIMARY KEY (`ArticleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;




CREATE TABLE `articlekeywords` (
  `ArticleID` varchar(19) NOT NULL,
  `KeywordID` char(50) NOT NULL,
  KEY `ArticleID_idx` (`ArticleID`),
  KEY `KeywordID_idx` (`KeywordID`),
  CONSTRAINT `ArticleID` FOREIGN KEY (`ArticleID`) REFERENCES `articles` (`articleid`),
  CONSTRAINT `KeywordID` FOREIGN KEY (`KeywordID`) REFERENCES `keywords` (`keywordid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;



CREATE TABLE `comments` (
  `CommentId` int(11) NOT NULL,
  `Comment` varchar(2000) NOT NULL,
  `CommentDate` datetime NOT NULL,
  `IsJournaliste` tinyint(4) NOT NULL,
  `HasChild` tinyint(4) NOT NULL,
  `Sentiment` int(11) NOT NULL,
  `UserId` varchar(150) NOT NULL,
  `ArticleID` varchar(19) NOT NULL,
  `ParentCommentId` int(11) DEFAULT NULL,
  PRIMARY KEY (`CommentId`),
  KEY `UserId_idx` (`UserId`),
  KEY `ParentCommentID_idx` (`ParentCommentId`),
  KEY `ArticleID_idx` (`ArticleID`),
  CONSTRAINT `ArticID` FOREIGN KEY (`ArticleID`) REFERENCES `articles` (`articleid`),
  CONSTRAINT `ParentID` FOREIGN KEY (`ParentCommentId`) REFERENCES `comments` (`commentid`),
  CONSTRAINT `UserId` FOREIGN KEY (`UserId`) REFERENCES `users` (`userid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

