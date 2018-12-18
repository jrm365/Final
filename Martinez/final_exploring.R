#this function graphs suicide rate across age groups, separated by gender 
graph_by_country <- function(df, country, year) 
{
  df <- df[df$year==year,]
  df <- df[df$country==country,]
  
  p <- ggplot(df, aes(x=reorder(age, rate), rate, fill=sex))+
    xlab("Age Groups") + ylab("Suicides per 100,000 People") +
    ggtitle(paste("Suicide Rate by Age and Gender for ", country, " in ",
                  year,sep=""))
  p <- p + geom_col(position=position_dodge(width=1))
  
  return(p)
}

#this graph shows changes in data across years for a year for different age groups
#divided again by gender
time_trend <- function(df, country) {
  
  df <- df[df$country==country,]
  p <- ggplot(df, aes(year, rate, color=age)) +
    xlab("Year") + ylab("Suicides per 100,000 People") + facet_grid(.~sex)+
    ggtitle(paste("Suicide Rate over Time", " in ", country, sep=""))
  
  p <- p + geom_line()
  return(p)
}

#since there are big disparities between age groups and gender, I made a function that plots 
#data according to a specific gender or age group 
graph_gender_age <- function(df, gender, age_group) {
  
  df <- df[df$sex==gender,]
  df <- df[df$age==age_group,]
  p <- ggplot(df, aes(year, rate, color=region)) + facet_grid(.~income)
  p <- p + geom_point()
  return(p)
}


