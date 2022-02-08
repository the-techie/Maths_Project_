df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")

getCount = function(pupose, year){
  
  data = subset(df1, df1$YEAR == year & df1$Pupose == pupose)
  
  return(sum(data$Grand.Total))
  
}

unique_pupose = unique(df1$Pupose)

unique_pupose = unique_pupose[-c(14)]
years = 2001:2012


for(p in unique_pupose){
  
  for(year in years){
    print(p)
    print(year)
    
    d = getCount(p, year)
    
    print(d)
    
  }
  print("==============================================")
  
  
}

