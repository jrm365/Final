library(dplyr)
library(countrycode)
library(ggplot2)
library(WDI)

setwd("C:/Users/Jaquelin Martinez/Desktop/MATH 110/R Final")

#reading WHO and WDI datasets 
suicide_df <- function() {
 df <- read.csv("who_suicide_statistics.csv", header = T, stringsAsFactors = F, encoding = "UTF-8")
 return(df)
}

wdi_df <- function() {
  df <- WDI(indicator = c("NY.GDP.PCAP.PP.KD"), start = 1987, end = 2016, extra = TRUE)
  
  #keep only revelant indicators from the WDI dataset
  df <- df %>% select("iso3c", "country","NY.GDP.PCAP.PP.KD", "year","region", "income")
  return(df)
}

###CLEANING SPECIFICS IN THE DATASETS### 


clean_who <- function() {
  df <- suicide_df()
  df <- df[complete.cases(df),] 
  df <- mutate(df, rate = ((df$suicides_no/df$population)*100000))
  df$age <- gsub(" years","", df$age)
  
  return(df)
}

clean_wdi <- function() {
  
  df <- wdi_df()
  df <- df[complete.cases(df),]
  
  #some country names were different in the WHO dataset and the WDI data 
  df$country <- gsub("Venezuela, RB","Venezuela (Bolivarian Republic of)", df$country)
  df$country <- gsub("St. Vincent and the Grenadines","Saint Vincent and Grenadines", df$country)
  df$country <- gsub("United States","United States of America", df$country)
  df$country <- gsub("Macao SAR, China","Macau", df$country)
  df$country <- gsub("Macedonia, FYR" ,"TFYR Macedonia", df$country)
  df$country <- gsub("Moldova","Republic of Moldova", df$country)
  df$country <- gsub("Morocco" ,"Morocco", df$country)
  df$country <- gsub("St. Lucia","Saint Lucia", df$country)
  df$country <- gsub("Korea, Rep." ,"Republic of Korea", df$country) 
  df$country <- gsub("St. Kitts and Nevis","Saint Kitts and Nevis", df$country)
  df$country <- gsub("Kyrgyz Republic","Kyrgyzstan", df$country)
  df$country <- gsub("Iran, Islamic Rep.","Iran (Islamic Rep of)", df$country)
  df$country <- gsub("Hong Kong SAR, China","Hong Kong SAR", df$country) 
  df$country <- gsub("Egypt, Arab Rep.","Egypt", df$country)
  df$country <- gsub("Dominican Republic","Dominican Republic", df$country) 
  df$country <- gsub("Bahamas, The" ,"Bahamas", df$country)
  df$country <- gsub("Bolivia","Bolivia", df$country) 

  return(df)
}

merged_who_wdi <- function() {
  who <- clean_who()
  wdi <- clean_wdi()
  
  df <- merge(who, wdi, by=c("country","year"))
}
