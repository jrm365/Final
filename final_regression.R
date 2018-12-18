
#to test to see if gdp is a good predictator for suicide rates, this function runs a one variable 
#regression of gdp on rate (aggregate level data)

simple_regression <- function(df) {
  aggregate <- aggregate_df(df)
  fit <- lm(rate ~ gdp, data=aggregate)
  return(summary(fit))
}

#create a multivariable regression that controls for age, gdp, gender, and country 
multivarate_regression <- function(df) {
  fit <- lm(rate ~  year + as.factor(age) + NY.GDP.PCAP.PP.KD + 
              as.factor(sex) + 
              as.factor(country), data=df)
  sum <- summary(fit) # show results
  #plot(fit, las = 1)
  return(sum)
}
