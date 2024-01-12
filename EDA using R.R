## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------
#### 
library(knitr)
library(ggplot2)
library(htmlTable)
library(tidyverse)
require(reshape2)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(DataCombine)
library(cowplot)
library(jtools)
library(MASS)
library(Metrics)
library(randomForest)
library(caret)
library(dplyr)
library(plotly)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
#Import data
#salaries data
sal <- read.csv("Data for EDA/2018-19_nba_player-salaries.csv")
#Payroll
payr <- read.csv("Data for EDA/2019-20_nba_team-payroll.csv")
#Player statistics
Pstats <- read.csv("Data for EDA/2018-19_nba_player-statistics.csv")
#Team statistics 1
Tstats1 <- read.csv("Data for EDA/2018-19_nba_team-statistics_1.csv")
#Team statistics 2
Tstats2 <- read.csv("Data for EDA/2018-19_nba_team-statistics_2.csv")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df <- merge(Pstats, sal)
#rename variables
names(df) <- c("player_name", "Pos", "Age", "Tm", "G", "GS", "MP", "FG", "FGA", "FGP", "X3P",        
"X3PA", "X3PP", "X2P", "X2PA", "X2PP", "eFG", "FT", "FTA", "FTP", "ORB", "DRB",        
"TRB", "AST", "STL", "BLK", "TOV", "PF", "PTS", "player_id", "salary")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
miss <- data.frame(colSums(is.na(df)))
miss <- cbind(Variable = rownames(miss), miss)
rownames(miss) <- 1:nrow(miss)
names(miss) <- c("Variable","Number of missing observations")
htmlTable(miss)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
dfx <- df

dfx$player_name <- NULL
dfx$Pos <- NULL
dfx$Tm <- NULL

#Drop salary since it has a different scale
#dfx$salary <- NULL
# using colMeans()
mean_val <- colMeans(dfx,na.rm = TRUE)
  
# replacing NA with mean value of each column
for(i in colnames(dfx))
  dfx[,i][is.na(dfx[,i])] <- mean_val[i]

dfl <- dfx
df1 <- dfx
df1$player_name <- df$player_name
df1$Pos <- df$Pos
df1$salary <- df$salary
miss <- data.frame(colSums(is.na(df1)))
miss <- cbind(Variable = rownames(miss), miss)
rownames(miss) <- 1:nrow(miss)
names(miss) <- c("Variable","Number of missing observations")
htmlTable(miss)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df <- df %>% distinct()


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------
dfx <- data.frame(scale(dfx))
ggplot(stack(dfx), aes(x = ind, y = values))+
  geom_boxplot(fill='steelblue', color="black") +
  coord_flip()+ggtitle("Box plot of variables with outliers")


## ----fig.height=8, fig.width=10, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------
meltdf <- melt(dfx)
ggplot(data = meltdf, aes(x = value)) + 
stat_density(fill='steelblue') + 
facet_wrap(~variable, scales = "free")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
df %>% ggplot(aes(x =Pos , y = salary, fill = Pos)) + 
                geom_boxplot()+
        labs(x="Player Position", y="Salary")+
        ggtitle("Distribution of player income by position")


## ---------------------------------------------------------------------------------------------------------------------------------------------------------
htmlTable(cor(dfx[-27], dfx$salary))

