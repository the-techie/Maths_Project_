library(dplyr)

df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")
df1


states = unique(df1$STATE.UT)
years = unique(df1$YEAR)
years

pupose = unique(df1$Pupose)

total_crimes = c()

for(x in states){
  data = subset(df1, df1$STATE.UT == x & df1$YEAR == 2002 & df1$Pupose == "Total")
  print(x)
  print(data$Grand.Total)
  
  total_crimes = c(total_crimes, data$Grand.Total)
  
}

total_crimes

plot(states, total_crimes, type = 'b')
