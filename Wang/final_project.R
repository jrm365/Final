# MATH 110 Final Project #
titanic <- read.csv("titanic.csv", header = T, stringsAsFactors = F)

install.packages('corrplot')
install.packages('ggthemes')
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)

survival_factor <- function(df, variable)
{
  survive <- df %>%
    mutate(Class = ifelse(Survived == 0, 'Died', 'Survived')) %>%
     group_by_(variable, 'Class') %>%
    summarise(count = n())
  
  p <- ggplot(survive, aes(y=survive$count, x=survive[[variable]], 
                           color=survive$Class, 
                           fill=survive$Class)) 
  p <- p + geom_bar(stat="identity", position="dodge") + labs(color="Class", fill="Class", y="Number of Passenger",x=variable)
  print(p)
}

survival_factor(titanic, 'Pclass')
survival_factor(titanic, 'Sex')
survival_factor(titanic, 'Parch')

class_fare <- function(df) {
  m <- rep(NA, 3)
  for (i in 1:length(m)) {
    class_df <- dplyr::filter(df, Pclass == i)
    m[i] <- mean(class_df$Fare)
  }
  fare_df <- data.frame(Class=1:3, Avg.Fare=m, stringsAsFactors = F)
  
  return (fare_df)
}

survival_two_factors <- function(df, f1, f2) {
  ggplot(df, aes(df[[f1]], fill = factor(df$Survived))) + 
    geom_bar(stat = "count")+
    theme_few() +
    xlab("Pclass") +
    facet_grid(.~df[[f2]])+
    ylab("Count") +
    scale_fill_discrete(name = "Survived") + 
    ggtitle("Pclass vs Sex vs Survived")
}

survival_two_factors(titanic, 'Pclass', 'Sex')


survival_three_factors <- function(df, Age, Sex, Pclass){
  p <- ggplot(df, aes(x = Age, y = Sex)) + 
    geom_jitter(aes(colour = factor(Survived))) + 
    theme_few()
  p <- p + theme(legend.title = element_blank())+
    facet_wrap(~Pclass) 
  p <- p + labs(x = "Age", y = "Sex", title = "Survivor factors: Class vs Sex vs Age")
  p <- p + scale_fill_discrete(name = "Survived") + 
    scale_x_continuous(name="Age",limits=c(0, 81))
  print(p)
}

survival_three_factors(titanic, 'Age', 'Sex', 'Pclass')


survival_title <- function(df) {
  df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)
  
  officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
  royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
  
  # Reassign titles
  df$Title[df$Title == 'Mlle']        <- 'Miss' 
  df$Title[df$Title == 'Ms']          <- 'Miss'
  df$Title[df$Title == 'Mme']         <- 'Mrs' 
  df$Title[df$Title %in% royalty]  <- 'Royalty'
  df$Title[df$Title %in% officer]  <- 'Officer'
  
  df$Surname <- sapply(df$Name,  
                            function(x) strsplit(x, split = '[,.]')[[1]][1])
  p <- ggplot(df, aes(Title,fill = factor(Survived))) +
    geom_bar(stat = "count")+ xlab('Title') + ylab("Count") 
  p <- p + scale_fill_discrete(name = " Survived") + 
    ggtitle("Title vs Survived")+
    theme_few()
  print(p)
}

survival_title(titanic)

# Survival: Family Size

survival_familysize <- function(df) {
  df$Fsize <- df$SibSp + df$Parch + 1
  
  p <- ggplot(df, aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat='count', position='dodge') 
  p <- p + scale_x_continuous(breaks=c(1:11)) + xlab('Family Size') +
    ylab("Count")
  p <- p + theme_few()+scale_fill_discrete(name = "Survived") 
  p <- p + ggtitle("Family Size vs Survived")
  print(p)
}
survival_familysize(titanic)


survival_correlation <- function(df) {
  df <- titanic
  df$Fsize <- df$SibSp + df$Parch + 1
  df$FsizeD[df$Fsize == 1] <- 'Alone'
  df$FsizeD[df$Fsize < 5 & df$Fsize > 1] <- 'Small'
  df$FsizeD[df$Fsize > 4] <- 'Big'
  df$Title <- gsub('(.*, )|(\\..*)', '', df$Name)
  
  officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
  royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
  
  # Reassign titles
  df$Title[df$Title == 'Mlle']        <- 'Miss' 
  df$Title[df$Title == 'Ms']          <- 'Miss'
  df$Title[df$Title == 'Mme']         <- 'Mrs' 
  df$Title[df$Title %in% royalty]  <- 'Royalty'
  df$Title[df$Title %in% officer]  <- 'Officer'
  
  corr_data <- df
  corr_data$Sex <- revalue(corr_data$Sex, 
                           c("male" = 1, "female" = 2))
  corr_data$Title <- revalue(corr_data$Title, 
                             c("Mr" = 1, "Master" = 2,"Officer" = 3, 
                               "Mrs" = 4,"Royalty" = 5,"Miss" = 6))
  corr_data$FsizeD <- revalue(corr_data$FsizeD, 
                              c("Small" = 1, "Alone" = 2, "Big" = 3))
  corr_data$FsizeD <- as.numeric(corr_data$FsizeD)
  corr_data$Sex <- as.numeric(corr_data$Sex)
  corr_data$Title <- as.numeric(corr_data$Title)
  corr_data$Pclass <- as.numeric(corr_data$Pclass)
  corr_data$Survived <- as.numeric(corr_data$Survived)
  
  corr_data <-corr_data[,c("Survived", "Pclass", "Sex", 
                           "FsizeD", "Fare", "Title")]
  
  str(corr_data)
  mcorr_data <- cor(corr_data)
  
  corrplot(mcorr_data,method="circle")
}

survival_correlation(titanic)

