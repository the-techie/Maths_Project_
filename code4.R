df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")
df1


states = unique(df1$STATE.UT)
years = unique(df1$YEAR)
years

pupose = unique(df1$Pupose)

getTotalCases <- function(year, purpose){
  
  data = subset(df1, df1$Pupose == purpose & df1$YEAR == year)
  
  total = sum(data$Total.No..of.cases.reported)
  return(total)
}

pupose = pupose[-14]

for(year in years){
  
  total_cases = getTotalCases(year, "Total")
  
  each_pupose_cases = c()
  
  for(p in pupose){
    cases = getTotalCases(year, p)
    
    each_pupose_cases = c(each_pupose_cases, cases)
  }
  
  pie(each_pupose_cases, main = year, cex.names = 0.4)
  legend(x = "topleft", legend = pupose)
  
}

getTotalCases(2001, "Total")
