# File is joint-work created by Michael Tran and Henry Vu,
# with the help of Dave Langer and Will Stanton via content from videos and links

setwd("C:/Users/Michael/TitanicDataset/")
#setwd("D:/Users/Henry/TitanicDataset/")

# CONTENTS FROM DAVE LANGER YOUTUBE VIDEO - PART 1

# Load raw data
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)
# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])
# Combine data sets
data.combined <- rbind(train,test.survived)
# A bit about R data types (e.g. factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train,aes(x = Pclass, fill = factor(Survived))) +
  stat_count(width = 0.5) +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Examine the first few names in the training data set
head(as.character(data.combined$Name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the 'Miss' and "Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g. SibSp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex =="male"), ]
males[1:5,]

# Expand upon the relationship between 'Survied' and 'Pclass' by adding the now 'Title' variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0){
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}

Titles <- NULL
for (i in 1:nrow(data.combined)) {
  Titles <- c(Titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(Titles)

# Since we only have survived lables for the train set, only use the 
# first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  stat_count() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# What's the distribution of females to males across train & test?
table(data.combined$sex)

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# OK, age and sex seem pretty important as derived from analysis of title, let's take
# a look at the distributions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) + 
  stat_count(width = 5) +
  xlab("Age") + 
  ylab("Total Count")

# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) + 
  stat_count(width = 10) +
  ggtitle("Age for 'Miss' by Pclass") +
  xlab("Age") + 
  ylab("Total Count")

# OK, appears female children may have different survival rate,
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the SibSp variable, summarize the  variable
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive. Visualize survival rates by SibSp, Pclass, and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  facet_wrap(~Pclass + Title) + 
  stat_count(width = 1) +
  xlab("SibSp") + 
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Treat the parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") + 
  xlab("Parch") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Let's try some feature engineering. What about creating a family size feature?
temp.SibSp <- c(train$SibSp, test$SibSp)
temp.Parch <- c(train$Parch, test$Parch)
data.combined$family.size <- as.factor(temp.SibSp + temp.Parch + 1)

#Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
  stat_count(width = 1) + 
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) + 
  labs(fill = "Survived")
  
# Take a look at the ticket variable
str(data.combined$Ticket)

# Based on the huge number of levels ticket really isn't a factor variable it is a string.
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
Ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(Ticket.first.char)

# OK, we can make a factor for analysis purposes and visualize
data.combined$Ticket.first.char <- as.factor(Ticket.first.char)

# First, a high-level plot of the data
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Ticket.first.char") + 
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of pclass & title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") + 
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0,200) +
  labs(fill = "Survived")

# Next up - the fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Can't make fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  stat_count(width = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") + 
  ylab("TotalCount") +
  ylim(0,200)

# Let's check to see if fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  stat_count(width = 5) +
  facet_wrap(~Pclass + Title) + 
  ggtitle("Pclass, Title") + 
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0,50) +
  labs(fill = "Survived")

# Analysis of the cabin variable
str(data.combined$Cabin)

# Cabin really isn't a factor, make a string and display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take a look at just the first char as a factor
Cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(Cabin.first.char)
levels(Cabin.first.char)

# Add to combined data set and plot
data.combined$Cabin.first.char <- Cabin.first.char

#High level plot
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survival by Cabin.first.char") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# Does this feature improve upon Pclass + Title
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0,500) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0,350) +
  labs(fill = "Survived")

# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)

# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Install randomForest (might already be installed)
library(randomForest) #this causes the margin object to be masked in ggplot2, might be an issue later on

# Train a Random Forest with the default parameters
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train a Random Forest using Pclass, Title, & SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a Random Forest using Pclass, Title, & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train a Random Forest using Pclass, Title, SibSp, Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a Random Forest using Pclass, Title, family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using Pclass, Title, SibSp, family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Train a Random Forest using Pclass, Title, Parch, family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)




# Additional verification on AgeRange I performed on my own - Michael
rm(data.combined)

data.combined$Age <- as.factor(data.combined$Age)
head(data.combined$Age)
tail(data.combined$Age)
table(data.combined$Age,data.combined$Survived)

data.combined$Age <- as.numeric(data.combined$Age)
data.combined$Age <- as.double(data.combined$Age)
data.combined$Age <- as.factor(data.combined$Age)

extractRange <- function(age)
{
  if (is.na(age)) # IS "NA"
  {
    return("NA")
  }
  else # NOT "NA"
  {
    if (age > 0 && age < 5) { return ("0.01-4") }
    if (age >=5 && age <=10) { return ("05-10") }
    if (age >10 && age <=15) { return ("11-15") }
    if (age >15 && age <=30) { return ("16-30") } 
    if (age >30 && age <=80) { return ("31-80") }
  }
}

AgeRange <- NULL
for (i in 1:nrow(data.combined)) {
  AgeRange <- c(AgeRange, extractRange(data.combined[i, "Age"]))
}
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$AgeRange <- as.factor(AgeRange)
library(ggplot2)
ggplot(data.combined[1:891,], aes(x = AgeRange, fill = Survived)) +
  stat_count() + 
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")

# GitHub confirmed working

# CONTENTS FROM WILL STANTON WEBSITE ON MACHINE LEARNING with R

library(caret)
library(randomForest)
library(fields)

#setwd("C:/Users/Michael/TitanicDataset/")
setwd("D:/Users/Henry/TitanicDataset/")

#testSet <- read.table("test.csv", sep = ",", header = TRUE)
#head(testSet)

trainSet <- read.table("train.csv", sep = ",", header = TRUE)
#head(trainSet)
#table(trainSet[,c("Survived","Pclass")])
#^ above comment line refers to crosstabs



#bplot.xy(trainSet$Survived, trainSet$Age)

#summary(trainSet$Age)
#summary(trainSet$Fare)


#Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
