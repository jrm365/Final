#this function an aggregate dataframe, grouping by specific country and year
aggregate_df <- function(df) {
  
  group <- df %>% group_by(country, year) %>%
    summarise(sumsui= sum(as.numeric(suicides_no)),popsum = sum(as.numeric(population)))
  
  gdp <- df %>% group_by(country, year)
  group$gdp <- unique(gdp$NY.GDP.PCAP.PP.KD) 
  
  group_with_rate <- group %>% mutate(rate =(sumsui/popsum*100000)) %>% arrange(-rate)

  return(group_with_rate)
}

#plotting GDP per capita (PPP) and suicide rate from aggregate data
plot_aggregate <- function(df) {
  aggregate_data <- aggregate_df(df)
  
  aggregate_data$decade <- cut(aggregate_data$year, breaks = c(-Inf, 1999, 2009, Inf), 
                               labels = c("90s", "2000s", "2010s"))
  p <- ggplot(data=aggregate_data, aes(gdp, rate))
  p <- p + geom_point() + geom_smooth(method="lm") + facet_grid(.~decade)
  return(p)
}

#name of country with the highest aggregate suicide rate per year
top_rate_year <- function(df)
{
  aggregate_data <- aggregate_df(df)
  unique_years <- sort(unique(aggregate_data$year))
  top <- rep(NA, length(unique_years))
  
  for (i in 1:length(unique_years)) {
    cy <- unique_years[i]
    filt <- aggregate_data[aggregate_data$year==cy,]
    max_rate <- max(filt$rate)
    max_country <- filter(filt, rate==max_rate)
    top[i] <- as.character(max_country$country)
    
  }
  
  df <- data.frame(year=unique_years,
                   top_country=top,
                   stringsAsFactors = F)
  return(df)
}

top_raw_year <- function(df)
{
  aggregate_data <- aggregate_df(df)
  unique_years <- sort(unique(aggregate_data$year))
  top <- rep(NA, length(unique_years))
  
  for (i in 1:length(unique_years)) {
    cy <- unique_years[i]
    filt <- aggregate_data[aggregate_data$year==cy,]
    max_number <- max(filt$sumsui)
    max_country <- filter(filt, sumsui==max_number)
    top[i] <- as.character(max_country$country)
    
  }
  
  df <- data.frame(year=unique_years,
                   top_country=top,
                   stringsAsFactors = F)
    return(df)
}


