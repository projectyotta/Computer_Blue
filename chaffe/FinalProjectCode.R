install.packages("plyr")
library("plyr")
install.packages("stringr")
library("stringr")

#load song
Dataset1 = read.csv("~/Google Drive/GMU/Current Classes/STAT 515/Final Project/Data Set/1-1.csv",header = FALSE,sep = ",")
#View(Dataset)

Dataset=c(Dataset1)
View(Dataset)
#create positive and negative file
pos.words <- scan("~/Google Drive/GMU/Current Classes/STAT 515/Final Project/positive-words.txt", what='character', comment.char=';')
neg.words <- scan("~/Google Drive/GMU/Current Classes/STAT 515/Final Project/negative-words.txt", what='character', comment.char=';')

#compare data file to lists
pos.matches <- pmatch(Dataset, pos.words)
neg.matches <- match(Dataset, neg.words)
View(pos.matches)
pos.matches1 <- !is.na(pos.matches)
neg.matches1 <- !is.na(neg.matches)
View(pos.matches1)

#score song
score <- sum(pos.matches1) - sum(neg.matches1)
score

