# TitatnicDataPractice
Titanic Data prediction Kaggle

#Hypothesis_01 - Higher Class People count is > Middile > Low - verified
#Hypothesis_02 - More people Suvived in First Class > Middle Class > Lowe Class vice versa
#Hypothesis_03 - Title have significance Mrs married either travelling alone or with Spouce
#Hypothesis_04 - Young boys/girls eithe have spouce nor travelling with Sibblings or with Parents
#Hypothesis_05 - More Men perished in Lower Class
#Hypothesis_06 - Averge aged women >30 are married
#Hypothesis_07 - Duplicate Names - Same name but two different person

#Set the working directory
setwd("C:/Users/asket/Documents/Exploratory Data Analysis/DataSet/DataSamples/titanic")
#Get working directory
getwd()
#Read Test and Train Data
train_titanic<-read.csv("train.csv", header = TRUE)
test_titanic<-read.csv("test.csv", header = TRUE)
#view the data
View(train_titanic) #is having one variable more than test #12
View(test_titanic) #test doesnt have the survived column #11
#need to have the survived column in the test and bind them with train to make the 
#super set of the data
#create a [1*1] data frame variable is Survived , all the rows of test
#all the columns of test data set
test.survived <-data.frame(Survived= rep("NONE",nrow(test_titanic)),test_titanic[,])
View(test.survived)
#combain the data using rbind
titanic.bind<-rbind(train_titanic,test.survived)
View(titanic.bind)
#glance of the data
str(titanic.bind)
#convert the Surviced and PC Class as FACTOR
titanic.bind$Survived<-as.factor(titanic.bind$Survived)
titanic.bind$Pclass<-as.factor(titanic.bind$Pclass)
#check the grouping of survived & PC Class
#if the no of people perished is 10 and live is 1 then not good
#pc class  high clas > middle class > low class
#to check above please run table model
table(titanic.bind$Survived)
table(titanic.bind$Pclass)
#install library ggplot
library(ggplot2)
#Hypothesis , the people who stayed in Upper Deck (more) saved 
#lets check this fact in the Train data set
train_titanic$Pclass<-as.factor(train_titanic$Pclass)
#train_titanic$Survived<-as.factor(train_titanic$Survived)
str(train_titanic$Pclass)
attach(train_titanic)
str(train_titanic)
#Hypothesis is tested
ggplot(train_titanic,aes(x=Pclass,fill = factor(Survived))) + 
  stat_count(width = 0.5) +
  xlab ("Class") + ylab ("Count") + labs(fill = "Survived")
#Examine the Name variable, it is mentioned as FACTOR, coz these names are Unique hence
#R recognized it as Factor created nrow levels
head(as.character(train_titanic$Name))
#Check the name uniqness 
length(unique(as.character(titanic.bind$Name)))   
#1307 Unique Names, need to find out the duplicate
dupe.names<-as.character(titanic.bind[which
                        (duplicated(as.character(titanic.bind$Name))),"Name"])
dupe.names
#Techically these are not duplicate rows different people of same Name
titanic.bind[which(titanic.bind$Name %in% dupe.names),]

#lets examing sib(sibling)sp(spouse) & par(parent)ch(child)
library(stringr)
#Who are Miss
passenger_Miss<-titanic.bind[which(str_detect(titanic.bind$Name,"Miss.")),]
head(passenger_Miss,n=5)
#sibsp - siblings / spouses aboard the Titanic
#parch - parents / children aboard the Titanic
#Confirmed that Miss. is Non Married women as we have one 4 years old girl who is travelling 
#with Sibbling and her parents who has Mss. in her Name

#Hypothesis
#the Title and Age has the relationship with each other

passenger_Miss<-titanic.bind[which(str_detect(titanic.bind$Name,"Mrs.")),]
head(passenger_Miss,n=5)

#Looks like the Older Women who has Mrs. looks like married and they are travelling with
#their spouse or with their sibblings

#lets Examine SEX

passenger.male<-titanic.bind[which(titanic.bind$Sex == 'male'),]
head(passenger.male,n=5)
#Master title looks like Young boy
#Most of the men in top5 are perished
#Most of them in Class-3

#Now lets try to plot Survived & PC_Class & Title of the passenger to see the relationship between

extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) 
  {
    return("Miss.")
  }
  else if (length(grep("Master.", name)) > 0)
  {
    return("Master.")
  }
  else if (length(grep("Mrs.", name)) > 0)
  {
    return("Mrs")
  }
  else if (length(grep("Mr.", name)) > 0)
  {
    return("Mr.")
  }
  else {
    return("Other")
  }
  
}

#now extract the title from each row.name and bind it back to the data frame

getTitleName("Futrelle, Master. Jacques Heath (Lily May Peel)")

#View(titanic.bind)

titles<- NULL
for (i in 1:nrow(titanic.bind))
{
  titles <- c(titles,extractTitle(titanic.bind[i,"Name"]))
}
#bind the extracted values to the data frame
titanic.bind$title<-as.factor(titles)
#view the binded value is added
View(titanic.bind)


#ggplot Persihed / Survived / Titile

ggplot(titanic.bind[1:891,],aes(x=title,fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass)+
  xlab ("Class") + ylab ("Count") + labs(fill = "Survived")

#Female vs Males distribution
table(titanic.bind$Sex)

#male Vs Femail - Dead / Live / Count / Class

ggplot(titanic.bind[1:891,],aes(x=Sex,fill = Survived)) + 
  stat_count(width = 0.5) +
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab ("Sex") + ylab ("Total Count") + labs(fill = "Survived")

#age factor

summary(titanic.bind$Age)
#There are 263 NA's either we can replace with NUll
#or predict it using linear model
#or replace mean of respective title using the meadian value

#Check the age of the Train data set

summary(titanic.bind[1:891,"Age"])
#there are 177 are in train data set

#Age Plot

ggplot(titanic.bind[1:891,],aes(x=Age,fill = Survived)) + 
#  stat_count(width = 0.5) +
  facet_wrap(~Sex+Pclass)+
  geom_histogram(binwidth = 10)+
  ggtitle("Pclass")+
  xlab ("Age") + ylab ("Total Count") + labs(fill = "Survived")

#Lest examing the Master titled data against the age

boysData<-titanic.bind[which(titanic.bind$title=="Master."),]
summary(boysData$Age)

#confirmed that Master is baby boy

#Lest examing the MISS titled data against the age

MissData<-titanic.bind[which(titanic.bind$title=="Miss."),]
summary(MissData$Age)

ggplot(MissData[MissData$Survived!="NONE",],aes(x=Age,fill = Survived)) + 
  #  stat_count(width = 0.5) +
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 5)+
  ggtitle("Age for Miss by Class")+
  xlab ("Age") + ylab ("Total Count") + labs(fill = "Survived")

#Young ladies Miss Survival more in Class -1
#Class 2 also
#class 3 more perished

#lets see the Misses who are travelling alond

missData.Alone<-MissData[which(MissData$SibSp== 0 & MissData$Parch == 0),]
summary(missData.Alone$Age)
length(which(missData.Alone$Age<=14.5))

#Lets examine the sibsp (Sibbling Spouse Variable)

summary(titanic.bind$SibSp)

length(unique(titanic.bind$SibSp))

titanic.bind$SibSp<-as.factor(titanic.bind$SibSp)

ggplot(titanic.bind[1:891, ], aes(x = SibSp, fill = Survived)) +
  stat_count(width = 1) +
  #geom_histogram(binwidth = 1) +
  facet_wrap( ~Pclass + title) +
  ggtitle("Pclass Title sibbling") +
  xlab ("SibSp") +
  ylab ("Total Count") + 
  ylim(0,300)+
  labs(fill = "Survived")

#Lets examine the parch (Parent / Child Variable)

summary(titanic.bind$Parch)
length(unique(titanic.bind$Parch))

titanic.bind$Parch<-as.factor(titanic.bind$Parch)


ggplot(titanic.bind[1:891, ], aes(x = Parch, fill = Survived)) +
  stat_count(width = 1) +
  #geom_histogram(binwidth = 1) +
  facet_wrap( ~Pclass + title) +
  ggtitle("Pclass Title Parent Child") +
  xlab ("Parch") +
  ylab ("Total Count") + 
  ylim(0,300)+
  labs(fill = "Survived")
