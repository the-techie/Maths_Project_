library(dplyr)

df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")
df1


states = unique(df1$STATE.UT)
years = unique(df1$YEAR)
years

pupose = unique(df1$Pupose)

getMalesCases <- function(){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
      data = subset(df1, df1$YEAR == x & df1$Pupose == "Total")
      data = sum(data$Total.Male)
      total_crimes = c(total_crimes, data)
  }
  return(total_crimes)
}

total_cases_males = getMalesCases()

getFemalesCases <- function(){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
    data = subset(df1, df1$YEAR == x & df1$Pupose == "Total")
    data = sum(data$Total.Female)
    total_crimes = c(total_crimes, data)
  }
  return(total_crimes)
}

total_cases_females = getFemalesCases()

years = c(2001:2012)

plot(years, total_cases_males, type = 'l', col = 'blue', las = 2, lwd = 5,
     xlab = "Year", ylab = "Cases", ylim = c(0, 50000), main = "Total cases year by year")
lines(years, total_cases_females, type = 'l', col = 'red', lwd = 5)

legend(x = "topleft", legend = c("Males", "Females"), fill = c("blue",  "red"))


