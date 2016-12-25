install.packages("ISLR")
library("ISLR")
install.packages("tree")
library("tree")
install.packages("SDMTools")
library("SDMTools")

df <- read.csv("~/Google Drive/GMU/Current Classes/STAT 515/Final Project/Data Set/prince_csv.csv")

# predicting sentiment
df.lm = df[-c(1)]
#View(df.lm)
model <- lm(Sentiment ~.,data = df.lm)
summary(model)

# predicting billboard position
df.billboard = df[-c(1,4)]
model <- lm(Position ~.,data = df.billboard)
summary(model)

# predicting if it is a hit or not
df.logistic = df[-c(1,5)]
df.log = glm(In.Hot.100.Billboard~., data=df.logistic, family=binomial)
summary(df.log)
coef(df.log)
summary(df.log)$coef

# predicting position on album

model <- lm(Position.on.Album ~.,data = df.lm)
summary(model)

# predicting song length

model <- lm(Song.length.in.seconds ~.,data = df.lm)
summary(model)

# predicting word count

model <- lm(Word.Count ~.,data = df.lm)
summary(model)

# predicting BPM
model <- lm(BPM.value ~.,data = df.lm)
summary(model)

# categorization
df.cat=df[-c(1,2,3,5,9,11)]
row<-nrow(df.cat)
set.seed(12345)
trainindex <- sample(row, 0.6*row, replace=FALSE)
training <- df.cat[trainindex,]
show(training)
validation <- df.cat[-trainindex,]
mylogit<-glm(In.Hot.100.Billboard ~ .,data=training, family=binomial)
summary(mylogit)
coef(mylogit)
exp(coef(mylogit)) 
step(mylogit)
valC<-predict(mylogit,validation,type="response")
valC[1:5]
matrixC = confusion.matrix(validation$In.Hot.100.Billboard,valC,threshold=0.5)    
matrixC

##### decision trees #####
df.tree=df[-c(1,5,9,10)]
#View(df.tree)
set.seed(12345)
tree.df=tree(Category~.,df.tree)
par(bg = "Grey")

plot(tree.df)
text(tree.df,pretty=4)
summary(tree.df)

#cv.df=cv.tree(tree.df)
#plot(cv.df$size,cv.df$dev,type='b')
prune.df=prune.tree(tree.df,best=5)
plot(prune.df)
text(prune.df,pretty=5)

