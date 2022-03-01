df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")

getCases <- function(column_name){
  total_crimes = c()
  years = 2001 : 2012
  
  for(x in years){
    data = subset(df1, df1$YEAR == x & df1$Pupose == "Total")
    data = sum(data[column_name])
    total_crimes = c(total_crimes, data)
  }
  return(total_crimes)
}

col_names = names(df1)
col_names = col_names[-c(1, 2, 3, 4, 17, 18, 19)]

n = length(col_names)

n = seq(1, n, 2)


data = c()

for(i in n){
  total_cases_males = getCases(col_names[i])
  
  total_cases_females = getCases(col_names[i+1])
  data = c(data, list(total_cases_males))
  data = c(data, list(total_cases_females))
  name = paste(col_names[i+1], col_names[i])
  
}


palette <- rep(c("red", "#1B9E77"), 3)

col_names = c("M<10yrs", "F<10yrs", "10<M<=15", "10<F<=15", "15<M<=18", 
              "15<F<=18", "18<M<=30", "18<F<=30", "30<M<=50", "30<F<=50", "M>50yrs", "F>50yrs" )

boxplot(data, col = palette, names = col_names, las = 2, cex.names = 0.32, ylim = c(0, 20000),
        ylab = "Number of cases", main = "Number of Cases v/s Age Group")
