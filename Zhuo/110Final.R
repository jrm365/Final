library(ggplot2)
library(dplyr)

#1.get the dataset and fix it
get_data <- function()
{
  nba_file <- "../math110/NBA.csv"
  nba <- data.table::fread(nba_file, header=T,sep=",", quote="\"",
                           dec=".", fill=T, stringsAsFactors = F)
  return(nba)
}

nba <- get_data()
#head(nba)

#To add a column of Inside/Outside players
get_InOut <- function(nba)
{
  rownum <- nrow(nba)
  for (i in 1:rownum) {
    if(nba$pos[i]=="PF"|nba$pos[i]=="C"){
      nba$InOut[i] <- "Inside"
    }else{
      nba$InOut[i] <- "Outside"
    }
  }
  return(nba)
}

nba <- get_InOut(nba)
#head(nba)

#2.The percentage of number of players in every position
get_position_percent <- function(position)
{
  nba_pos <- nba[nba$pos== position, ]
  
  num_pos <- nrow(nba_pos)
  num_nba <- nrow(nba)
  fraction_pos <- num_pos/num_nba
  return(fraction_pos)
}

#get_position_percent("PG")
#get_position_percent("SG")
#get_position_percent("PF")
#get_position_percent("SF")
#get_position_percent("C")

#3. Get the points/ rebounds/ assists of every position
get_point <- function(position)
{
  nba_filt3 <- nba[nba$pos == position,]
  
  hist(nba_filt3$point,xlim = c(0,33),breaks=10, ylim = c(0,30), 
       main = "Histogram of the stats of a certain position")
}

get_point("SG")

get_rebound <- function(position)
{
  nba_filt3 <- nba[nba$pos == position,]
  
  hist(nba_filt3$rebound,xlim = c(0,20),breaks=10, ylim = c(0,30), 
       main = "Histogram of the stats of a certain position")
}

get_rebound("C")

get_assist <- function(position)
{
  nba_filt3 <- nba[nba$pos == position,]
  
  hist(nba_filt3$assist,xlim = c(0,13),breaks=10, ylim = c(0,20), 
       main = "Histogram of the stats of a certain position")
}

get_assist("PG")

#4.SALARY

#Study the pay of players from all different aspects, and try to get factors that affect salary

#Sometimes there are unusual situations happening. Like players may get injured, so their attendance and stats would 
#be drastically influenced. To get an unbiased judgement, we clear out players who play less than 20 games
# during this year. Besides, for convenience, we convert "PG-SG" and "SF-SG" into "SG". 
clear_players <- function(nba)
{
  NBA <- nba[nba$games >= 20,]
  number_row <- nrow(NBA)
  for (i in 1:number_row) {
    if(NBA$pos[i]=="PG-SG"|NBA$pos[i]=="SF-SG"){
      NBA$pos[i] <- "SG"
    }
    
  }
  return(NBA)
}

NBA <- clear_players(nba)
#nrow(nba)
#nrow(NBA)
#View(NBA)

#As we know, age affects the athletic levle of a player, thus affecting his salary level. 

#Create a data.frame with columns age, players, topSalary.  The age column should contain 
#all the unique age in NBA. The players column should, for a given age, contain the 
#number of players in that age.  The topSalary column should, for a given age, 
#contain the highest salary of that age. And write the data.frame into a csv file.
get_salary_age <- function(NBA)
{
  NBA_filt4 <- NBA[, c("age","Player","pay")]
  NBA_filt4_age <- sort(unique(NBA_filt4$age))
  age_number <- length(NBA_filt4_age)
  NBA_filt4_players <- rep(NA, age_number)
  NBA_filt4_topSalary <- rep(NA, age_number)
  
  for (i in 1:age_number) {
    NBA_c <- NBA[NBA_filt4$age == NBA_filt4_age[i],]
    NBA_filt4_players[i] <- length(NBA_c$Player)
    NBA_filt4_topSalary[i] <- NBA_c$pay[1]
    
  }
  
  salary_age <- data.frame(age=NBA_filt4_age,
                           players=NBA_filt4_players,
                           topSalary=NBA_filt4_topSalary,
                           stringsAsFactors = F)
  write.csv(salary_age, "salary_age.csv")
  return(salary_age)
}

salary_age <- get_salary_age(NBA)
#salary_age

#Use Graph to show the relationship between topSalary and age
plot_graph <- function(base_name)
{
  file_name <- paste(base_name, ".csv", sep="")
  salary_a <- read.csv(file_name, header=T,
                  stringsAsFactors = F)
  plot(salary_a$age, salary_a$topSalary, xlim = c(19, 41), ylim = c(2300000,35000000),
       xlab = "Age", ylab = "Top Salary Gained", main = "Age Salary Plot",
       col = "gold", pch = 16)
  lines(salary_a$age, salary_a$topSalary, col = "goldenrod1")
  legend("bottomright", legend = c("TopSalry"),
         col = c("gold"), pch = 16, bty = "n")
}
plot_graph("salary_age")

#Try to fit a regression line
regression_graph <- function(salary_age)
{
  x <- 1:length(salary_age$age)
  y <- salary_age$topSalary
  plot(x,y, col="red")
  s <- stats::smooth.spline(x, y, nknots=15)
  many_x <- seq(0,length(salary_age$age),1) 
  y_smooth <- predict(s, many_x) 
  lines(y_smooth$x, y_smooth$y, col="red")
}
regression_graph(salary_age)

#From these two graphs we can conclude that an age around 30 is the best for a player, and players 
#under this age is likely to make a big amount of money in a season.

#We then naturally think that ages around 30 should have the largest number of players as well. We 
#will test this thought below. We use a function to find the age with most players.
Age_mostPlayers <- function(salary_age)
{
  most_age <- which.max(salary_age$players)
  age1 <- salary_age$age[most_age]
  return(age1)
}
Age_mostPlayers(salary_age)
#The age with the most players is 23. It is different from the age of 30 as we have guessed. 
#This inconsistency may be caused by the fact that several ages share similar numbers of players.

#The mismatch between the age with the greatest top salary and the age with the most players makes me to 
#reconsider my index selection. Perhaps it would be better to use average salary instead of top salary.
get_meanSalary <- function(NBA)
{
  NBA_filt_age <- sort(unique(NBA$age))
  h_average <- rep(NA, length(NBA_filt_age))
  
  for (i in 1:length(NBA_filt_age)) {
    NBA_agefilted <- dplyr::filter(NBA, age == NBA_filt_age[i])
    sum_pay <- sum(NBA_agefilted$pay)
    h_average[i] <- sum_pay/length(NBA_agefilted$pay)
    
  }
  
  Mean_Salary <- data.frame(age=NBA_filt_age,
                            average_salary=h_average,
                            stringsAsFactors = F)
  return(Mean_Salary)
  
}
Mean_Salary <- get_meanSalary(NBA)
#Mean_Salary
#Then we use a function to find the age with greatest average salary
Age_mostMean <- function(Mean_Salary)
{
  most_salary <- which.max(Mean_Salary$average_salary)
  age2 <- Mean_Salary$age[most_salary]
  return(age2)
}
Age_mostMean(Mean_Salary)
#Age 29 has the largest average salary. This supports our precious conclusion that players of ages 
#around 30 can make the most money.

#5.Then we want to study the relationship between salary and stats (including Ponits/ Rebounds/ Assists)
#A graph of the relationship between salary and points. To compare, we separate inside players 
#and outside players.
point_salary_graph <- function(NBA)
{
  p <- ggplot()
  p <- p + geom_point(mapping=aes(x=point, y=pay, 
                                  color=pos), 
                      data=NBA)
  p + geom_smooth(mapping=aes(x=point, y=pay,), 
                  data=NBA, method="lm")+ facet_grid(InOut ~ .)
  
}
point_salary_graph(NBA)

#A graph of the relationship between salary and rebounds.
rebound_salary_graph <- function(NBA)
{
  p <- ggplot()
  p <- p + geom_point(mapping=aes(x=rebound, y=pay, 
                                  color=pos), 
                      data=NBA)
  p + geom_smooth(mapping=aes(x=rebound, y=pay,), 
                  data=NBA, method="lm")+ facet_grid(InOut ~ .)
  
}
rebound_salary_graph(NBA)

#A graph of the relationship between salary and assists.
assist_salary_graph <- function(NBA)
{
  p <- ggplot()
  p <- p + geom_point(mapping=aes(x=assist, y=pay, 
                                  color=pos), 
                      data=NBA)
  p + geom_smooth(mapping=aes(x=assist, y=pay,), 
                  data=NBA, method="lm")+ facet_grid(InOut ~.)
  
}
assist_salary_graph(NBA)
#From these graphs we can see that players with higher points, rebounds and assists will get a better
# salary, which is consistent with our common sense.

#6. As we know, NBA players come from different colleges, and some of the colleges are famous of 
#basketball, like Duke, Georgetown and so on. To find out the influence of college on NBA, I try to 
#give a function, with which, you input a NBA team name and a college name, and get the names of the
#players in both that team and that collage.
get_college_team <- function(NBA, College, Team)
{
  NBA_filt6 <- dplyr::filter(NBA, college == College & team == Team)
  NBA_filted6 <- NBA_filt6$Player
  return (NBA_filted6)
}
#get_college_team(NBA, "Georgetown University", "CHO")
#get_college_team(NBA, "Temple University", "OKC")
#get_college_team(NBA, "Yale University", "MIL")


#7. Best offensive team
#From above we can see that players with greater average point are better. So we can generate the idea
#of "Best offensive team" based on the thought that a team with a higher average point generally do 
#better in offensive side. 
#Below I offer a function that gives the total score of a team inputted. 
#Round the number into a integer.
get_total_point <- function(NBA, Team)
{
  NBA_filt7 <- dplyr::filter(NBA, team == Team)
  t <- NBA_filt7$point
  leng <- length(t)
  tp <- sapply(t, function(t)
  {
    if((t-floor(t)) >= 0.5){
      tp <- (floor(t)+1)
    }else{
      tp <- floor(t)
    }
    return(tp)
  }
  )
  total <- sum(tp)
  return(total)
}
#get_total_point(NBA, "CHI")
#get_total_point(NBA, "OKC")

#To get the "Best offensive team"
get_best_offensive <- function(NBA)
{
  teams <- sort(unique(NBA$team))
  team_num <- length(teams)
  total_score <- rep(NA, team_num)
  for (i in 1:team_num) {
    team_score <- get_total_point(NBA, teams[i])
    total_score[i] <- team_score
  }
  score_team <- data.frame(team=teams, score=total_score,
                           stringsAsFactors = F)
  most_score <- which.max(score_team$score)
  team_good <- score_team$team[most_score]
  return(team_good)
}
get_best_offensive(NBA)
#So we can see that team "BRK" did best in offensive side during the last season.




