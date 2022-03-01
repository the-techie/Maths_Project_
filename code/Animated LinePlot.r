#Line Plot
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


plot(years, total_cases_males, type = 'l', col = '#7570B3', las = 2, lwd = 5,
     xlab = "Year", ylab = "Cases", ylim = c(0, 50000), main = "Total cases year by year")
lines(years, total_cases_females, type = 'l', col = '#E7298A', lwd = 5)

legend(x = "topleft", legend = c("Males", "Females"), fill = c('#7570B3', '#E7298A'))


#Animated Line Plot
library(ggplot2)
library(gganimate)

years = c(years, years)
cases = c(total_cases_females, total_cases_males)
gender = c(rep("Females", 12), rep("Males", 12))
df_new = data.frame(
  "year" = years,
  "cases" = cases,
  "Gender" = gender  
)
df_new
p = ggplot(df_new, aes(x = year, y = cases, color = Gender))+
  geom_line(size=3) + scale_x_continuous(limits = c(2001, 2012)) + 
  theme_minimal(base_size = 28) + labs(
    title = 'Yearwise total males and females cases', x = 'Year', y = 'Cases', 
  ) +
  transition_states(year, 12) + transition_reveal(year)

animate(p, fps = 10, duration = 10, width = 1800, height = 1200)