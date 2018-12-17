#Final Project- Gun Violence in US from 2013-2018

#The CSV file contains data for all recorded gun violence incidents in the US
#between January 2013 and March 2018, inclusive.
#In the notes about the dataset, it states "The list of incidents from 2013 is not exhaustive; only 279 incidents from that year were catalogued." 
#Because it will throw off the analysis, along with 2018 which only goes up to 2018, I will remove them from the data.

#These are the following columns I will use in my analyeis
#date (Date of crime)
#state (State of crime)
#city_or_county (City/ County of crime)
#n_killed (Number of people killed)
#latitude (Location of the incident)
#longitude (Location of the incident)
#participant_age_group (Age group of participant(s) at the time crime)

# I will analyze what factors determine shootings in the US.

library(lubridate)
library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)

#first i want to change the dates into columns for day of the week, day, month, and year
df <- function(file){
  df <- read.csv(file, header=T, stringsAsFactors = F)
  df_filt <- df[year(df$date)=="2014"|year(df$date)=="2015"|year(df$date)=="2016"| year(df$date)=="2017",]
  df_filt$year <- year(df_filt$date)
  df_filt$month <- lubridate::month(df_filt$date, label=TRUE)
  df_filt$day <- day(df_filt$date)
  df_filt$weekday <- lubridate::wday(df_filt$date, label=TRUE)
  return(df_filt)
}

#number of shootings per year in US
shootings_per_year <- function(df){
  
  df_years <- table(df$year) %>% as.data.frame
  years <- df_years$Var1
  freq <- df_years$Freq
  p <- ggplot(data=df_years, aes(x=years, y=freq)) + geom_bar(stat="identity") + xlab("Year") + ylab("Number of Gun Violence Incidents") + ggtitle("Number of Gun Violence Incidents in U.S. from 2014-2017") +theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq)) + theme_bw()
  print(p)

  return(df_years)
}

killed_per_year <-function(df){
  year <- unique(df$year)
  deadly_incidents <- rep(NA, length(year))
  total_deaths <- rep(NA, length(year))
  for(i in 1:length(year)){
    df_filt <- df[df$year==year[i],]
    deadly_incidents[i] <- nrow(df_filt[df_filt$n_killed >0,])
    total_deaths[i] <- sum(df_filt$n_killed)
  }
  
  df_deaths <- data.frame(year=year, deadly_incidents=deadly_incidents, total_deaths=total_deaths, stringsAsFactors = F)
  df_final <- melt(df_deaths, id.vars='year')
  p <- ggplot(df_final, aes(year,value, fill=factor(variable))) + geom_bar(stat="identity",position="dodge")+ggtitle("Number of Deaths from Gun Violence Incidents in U.S. from 2014-2017") +theme(plot.title = element_text(hjust = 0.5)) + guides(fill=guide_legend(title="Legend")) + theme_bw()
  print(p)
  
  return(df_final)
}

#number of shootings from 2014-2017 in each state
shootings_by_state <- function(df){
  
  
  df_states <- table(df$state) %>% as.data.frame
  df_states <- df_states[order(df_states$Freq, decreasing=TRUE),]
  states <- factor(df_states$Var1, levels = df_states$Var1[order(df_states$Freq)])
  freq <- df_states$Freq
  p <- ggplot(data=df_states, aes(x=states, y=freq)) + geom_col() + coord_flip() + xlab("State") + ylab("Number of Gun Violence Incidents by State") + ggtitle("Number of Gun Violence Incidents in U.S. from 2014-2017 by State") +theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
  print(p)
  
  return(df_states)
}
#mention top states

#lets get the top ten cities 
shootings_by_city <- function(df){
  df_city <- table(df$city_or_county) %>% as.data.frame
  df_city <- df_city[order(df_city$Freq, decreasing=TRUE),]
  df_city <- df_city[1:10,]
  cities <- factor(df_city$Var1, levels = df_city$Var1[order(df_city$Freq)])
  freq <- df_city$Freq
  p <- ggplot(data=df_city, aes(x=cities, y=freq)) + geom_col() + coord_flip() + xlab("Cities") + ylab("Number of Gun Violence Incidents for Top 10 Cities") + ggtitle("Top Ten Cites with Most Gun Violence Incidents in U.S. from 2014-2017") +theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
  print(p)
  
  return(df_city)
}
#wow at Chicago

#states are different sizes and different amounts per years, what would interest me more is how much in percentage have states changed over the years (in amt)
percent_change <- function(df){
  state <- unique(df$state)
  amt_2014 <- rep(NA, length(state))
  amt_2017 <- rep(NA, length(state))
  percent_change <- rep(NA, length(state))
  for(i in 1:length(state)){
    df_filt <- df[df$state==state[i],]
    amt_2014[i] <- nrow(df_filt[df_filt$year=="2014",])
    amt_2017[i] <- nrow(df_filt[df_filt$year=="2017",])
    percent_change[i] <- (amt_2017[i]-amt_2014[i])/amt_2014[i] *100}
  df_new <- data.frame(state=state,amt_2014=amt_2014, amt_2017=amt_2017, percent_change=percent_change,stringsAsFactors = F)
  df_new <- df_new[order(df_new$percent_change, decreasing=TRUE),]
  
  states <- factor(df_new$state, levels = df_new$state[order(df_new$percent_change)])
  change <- df_new$percent_change
  p <- ggplot(data=df_new, aes(x=states, y=change)) + geom_col() + coord_flip() + xlab("State") + ylab("Percent Change (%)") + ggtitle("Percent Change of Number of Gun Violence Incidents by State from 2014-2017") +theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
  print(p)
  
  return(df_new)
}
#south dakota and alaska!


killed_per_state <-function(df){
  df <- df[df$n_killed >0,]
  state <- unique(df$state)
  deadly_incidents <- rep(NA, length(state))
  total_deaths <- rep(NA, length(state))
  for(i in 1:length(state)){
  df_filt <- df[df$state==state[i],]
  deadly_incidents[i] <- nrow(df_filt)
  total_deaths[i] <-sum(df_filt$n_killed)
  }
  df_new <- data.frame(state=state,deadly_incidents=deadly_incidents,totaL_deaths=total_deaths,stringsAsFactors = F)
  df_new <- df_new[order(total_deaths, decreasing=TRUE),]
  
  states <- factor(state, levels = state[order(total_deaths)])
  p <- ggplot(data=df_new, aes(x=states, y=total_deaths)) + geom_col() + coord_flip() + xlab("State") + ylab("Number of Deaths due to Gun Violence") + ggtitle("Number of Deaths due to Gun Violence in U.S. from 2014-2017 by State") +theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
  print(p)

  return(df_new)
}

#mention how while Illinois was top in overall number, California is top for incidents resulting in death. What can this tell us?

#important to consider percent of shootings that resulted in death in determining which state sets up their citizens for death
#get percent of shootings that are deaths in each state - deadly incidents
murder_percent <- function(df){
  state <- unique(df$state)
  murders <- rep(NA, length(state))
  shootings <- rep(NA, length(state))
  percent <- rep(NA, length(state))
  for(i in 1:length(state)){
    df_filt <- df[df$state==state[i],]
    murders[i] <- nrow(df_filt[df_filt$n_killed>0,])
    shootings[i] <- nrow(df_filt)
    percent[i] <- murders[i]/shootings[i] *100
  }
  df_new <- data.frame(state=state, percent_of_shootings = percent,fill=rep("no",51))
  
  #comparing with the nation in general
  all_murders <- nrow(df[df$n_killed >0,])
  all_shootings <- nrow(df)
  all_percent <- all_murders/all_shootings * 100
  df_usa <- data.frame(state="USA",percent_of_shootings=all_percent,fill="yes")
  
  df_final <- rbind(df_new,df_usa)
  df_final <- df_final[order(df_final$percent_of_shootings, decreasing = TRUE),]
  
  states <- factor(df_final$state, levels = df_final$state[order(df_final$percent_of_shootings)])
  percent <- df_final$percent_of_shootings
  p <- ggplot(data=df_final, aes(x=states, y=percent, fill=fill)) + geom_bar(stat="identity") + coord_flip() + xlab("State") + ylab("Percent of Gun Violence Incidents that Result in Death (%)") + ggtitle("Percent of Gun Violence Incidents that Result in Death in the U.S. from 2014-2017 by State") +theme(plot.title = element_text(hjust = 0.5)) + theme_bw()
  print(p)
  
  return(df_final)
}
#terriyfing that in Arizona 40% of shootings result in death

#also we can map where the most deaths occur from gun violence

k_means <- function(df){
df_filt<-na.omit(df[df$n_killed>0,])

m <- dplyr::select(df_filt, longitude, latitude) %>% as.matrix
km_out <- kmeans(m, centers=5)

df_clustered <- data.frame(df_filt, cluster=factor(km_out$cluster))

p <- ggplot()
p <- p + geom_point(mapping=aes(x=longitude, y=latitude, color=cluster), data=df_clustered)
#p <- p + facet_grid(~.)
print(p)

}
#Looked around and found this function that does it on an actual map
map_shooting_deaths <- function(df){
global <- map_data("state")
p <- ggplot(global, aes(x = long, y = lat)) + geom_polygon(aes(group = group), fill = "white", col = "black") + coord_fixed(1.3, xlim = c(-130,-60), ylim = c(20,50)) +
  geom_point(data = df, aes(x = longitude, y = latitude, col = n_killed), size = 0.001, alpha = .1) +  
  theme_void() + 
  theme(legend.position = "none")
print(p)
}
#i want to make a function that allows me to get important stats about shooting in a particular city and state

stats_by_place <- function(df, state, city){
  if(city=="all"){
  df_filt <- df[df$state==state,]
  city <- "all"
  }
  else{
  df_filt <- df[df$state==state & df$city==city,]
  }
  total_shootings <- nrow(df_filt)
  shootings_2014 <- nrow(df_filt[df_filt$year=="2014",])
  shootings_2015 <- nrow(df_filt[df_filt$year=="2015",])
  shootings_2016 <- nrow(df_filt[df_filt$year=="2016",])
  shootings_2017 <- nrow(df_filt[df_filt$year=="2017",])
  percent_change <- (shootings_2017 - shootings_2014)/shootings_2014 * 100
  total_deaths <- sum(df_filt$n_killed)
  deadly_incidents <- nrow(df_filt[df_filt$n_killed>0,])
  percent_deadly_incidents <- total_deaths/total_shootings * 100
  city <- city
  
  df_new <- data.frame(state=state,city=city, total_shootings=total_shootings,shootings_2014=shootings_2014,shootings_2015=shootings_2015,shootings_2016=shootings_2016,shootings_2017=shootings_2017,percent_change_num_shootings = percent_change,total_deaths=total_deaths, deadly_incidents=deadly_incidents, percent_deadly_incidents=percent_deadly_incidents)
  
  return(df_new)
}

#Next I will analyze the relation between the day of the week/month and gun_violence
shootings_by_day <- function(df){
  df_days <- table(df$weekday) %>% as.data.frame
  day <- df_days$Var1
  num_shootings <- df_days$Freq
  p <- ggplot(data=df_days, aes(x=day, y=num_shootings)) + geom_bar(stat="identity") + xlab("Day of the Week") + ylab("Number of Gun Violence Incidents") + ggtitle("Number of Gun Violence Incidents in U.S. from 2014-2017 by Day") +theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq)) + theme_bw()
  print(p)
  
  return(df_days)
}
#Sunday has most shootings followed by Saturday - weekends provide more free time for people who get into trouble

shootings_by_month <- function(df){
  df_month <- table(df$month) %>% as.data.frame
  df_month <- df_month[order(df_month$Freq, decreasing=TRUE),]
  month <- df_month$Var1
  num_shootings <- df_month$Freq
  p <- ggplot(data=df_month, aes(x=month, y=num_shootings)) + geom_bar(stat="identity") + xlab("Month") + ylab("Number of Gun Violence Incidents") + ggtitle("Number of Gun Violence Incidents in U.S. from 2014-2017 by Month") +theme(plot.title = element_text(hjust = 0.5)) + geom_text(aes(label=Freq)) + theme_bw()
  print(p)
  
  return(df_month)
}
#shootings seem heavily related to the warmer months- they are at an all time high in July/August and low in February- coldest month

#Finally I will look at the reasons for the shootings
#i did df <- df("gun_violence.csv) and head(df$incident_characteristics) and realized that I wanted to look into three categories that popped up alot "Gang involvement", "Drug involvement" and "Domestic Violence"

reason_for_shooting <- function(df){
  year <- unique(df$year)
  gang_involvement <- rep(NA, length(year))
  drug_involvement <- rep(NA, length(year))
  domestic_violence <- rep(NA, length(year))
  for(i in 1:length(year)){
    df_filt <- df[df$year==year[i],]
    gang_involvement[i] <- nrow(df_filt[grepl("Gang involvement",df_filt$incident_characteristics),])
    drug_involvement[i] <- nrow(df_filt[grepl("Drug involvement",df_filt$incident_characteristics),])
    domestic_violence[i] <- nrow(df_filt[grepl("Domestic Violence",df_filt$incident_characteristics),])
  }
  df_new <- data.frame(year=year,gang_involvement=gang_involvement, drug_involvement=drug_involvement, domestic_violence=domestic_violence)
  return(df_new)
}
#gang_involvement shootings seem to be staying steady, drug involvement shootings have gone way up, domestic violence shootings have also gone up --> mention how i dont know how reliably the data reflects the involvement

#Lastly, i want to see how many children/teens are involved in shootings (whether that be killed, injured,involved)
kid_shootings <- function(df){
  kids_in_shootings <- length(grep("Child 0-11",df$participant_age_group))
  teens_in_shootings <- length(grep("Teen 12-17",df$participant_age_group))
  df_new <- data.frame(kids_in_shootings=kids_in_shootings,teens_in_shootings=teens_in_shootings,stringsAsFactors = F)
  
return(df_new)
  }
