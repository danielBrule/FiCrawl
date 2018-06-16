CREATE DATABASE `ficrawl` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci */;

CREATE TABLE `keyword` (
  `KeywordID` varchar(50) NOT NULL,
  PRIMARY KEY (`KeywordID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `user` (
  `UserID` varchar(150) NOT NULL,
  PRIMARY KEY (`UserID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;



CREATE TABLE `article` (
  `ArticleID` varchar(19) NOT NULL,
  `Title` varchar(500) NOT NULL,
  `PublicationDate` datetime NOT NULL,
  `LastModificationDate` datetime NOT NULL,
  `NewsPaper` varchar(10) NOT NULL,
  `URL` varchar(500) NOT NULL,
  PRIMARY KEY (`ArticleID`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;



CREATE TABLE `articlekeywords` (
  `ArticleID` varchar(19) NOT NULL,
  `KeywordID` varchar(50) NOT NULL,
  KEY `ArticleID_idx` (`ArticleID`),
  KEY `KeywordID_idx` (`KeywordID`),
  CONSTRAINT `ArticleID` FOREIGN KEY (`ArticleID`) REFERENCES `article` (`articleid`),
  CONSTRAINT `KeywordID` FOREIGN KEY (`KeywordID`) REFERENCES `keyword` (`keywordid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;


CREATE TABLE `comments` (
  `CommentId` int(11) NOT NULL,
  `Comment` varchar(2000) NOT NULL,
  `CommentDate` datetime NOT NULL,
  `IsJournaliste` binary(1) NOT NULL,
  `UserId` varchar(150) NOT NULL,
  `ArticleID` varchar(19) NOT NULL,
  `ParentCommentId` int(11) NOT NULL,
  PRIMARY KEY (`CommentId`),
  KEY `UserId_idx` (`UserId`),
  KEY `ParentCommentID_idx` (`ParentCommentId`),
  KEY `ArticleID_idx` (`ArticleID`),
  CONSTRAINT `ArticID` FOREIGN KEY (`ArticleID`) REFERENCES `article` (`articleid`),
  CONSTRAINT `ParentID` FOREIGN KEY (`ParentCommentId`) REFERENCES `comments` (`commentid`),
  CONSTRAINT `UserId` FOREIGN KEY (`UserId`) REFERENCES `user` (`userid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;
