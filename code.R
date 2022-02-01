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
palette <- RColorBrewer::brewer.pal(length(states),name = 'YlOrRd')

barplot(total_crimes, names.arg = states, las=2, cex.names = 0.57, xlab = "State",
        ylab = "Crimes", main = "Statewise total cases in 2002", col = palette)

# all using functions
getGraph <- function(year){
  total_crimes = c()
  
  for(x in states){
    data = subset(df1, df1$STATE.UT == x & df1$YEAR == year & df1$Pupose == "Total")
    total_crimes = c(total_crimes, data$Grand.Total)
    
  }
  palette <- RColorBrewer::brewer.pal(length(states),name = 'BuGn')
  
  title_name = paste("Statewise total cases", year)
  
  barplot(total_crimes, names.arg = states, las=2, cex.names = 0.57, xlab = "State",
          ylab = "Crimes", main = title_name, col = palette)
}

years_correct = c(2002:2012)

for(x in years_correct){
  getGraph(x)
}



total_cases = list()

getValues <- function(year){
  total_crimes = c()
  
  for(x in states){
    data = subset(df1, df1$STATE.UT == x & df1$YEAR == year & df1$Pupose == "Total")
    total_crimes = c(total_crimes, data$Grand.Total)
  }
  return(total_crimes)
}

k = 1

for(x in years_correct){
  
  res = getValues(x)
  
  print(res)
  
  total_cases[[k]] = res
  k = k + 1
}

stacked = as.matrix(total_cases)

barplot(b)

stacked = matrix(unlist(total_cases), nrow = 11, ncol = 35)

palette <- RColorBrewer::brewer.pal(length(states),name = 'BuGn')

barplot(stacked, names.arg = states, las=2, cex.names = 0.57, xlab = "State",
        ylab = "Crimes", main = "Yearwise cases distribution of each state", 
        col = palette, ylim = c(0, 20000))