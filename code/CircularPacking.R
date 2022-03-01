library(packcircles)
library(ggplot2)
library(viridis)

df1 = read.csv("C:\\Users\\vidhi\\Desktop\\Vidhi\\PG College\\Maths\\dataset.csv")

getCount = function(pupose, year){
  data = subset(df1, df1$YEAR == year & df1$Pupose == pupose)
  return(sum(data$Grand.Total))
}

unique_pupose = unique(df1$Pupose)
unique_pupose = unique_pupose[-c(14)]
years = 2001:2012

countPupose = c()
name = c()
alt_pupose <- c("For Adoption"="A", "For Begging"="B", "For Camel racing"="C",
                "For Illicit intercourse"="D", "For marriage"="E", "For Prostitution"="F",
                "For Ransom"="G", "For Revenge"="H", "For Sale"="I", "For Selling body parts"="J",
                "For Slavery"="K", "For unlawaful activity"="L", "Others"="M")
for(p in unique_pupose){
  alternative_name = alt_pupose[p]
  for(year in years){
    d = getCount(p, year)
    countPupose<-c(countPupose, d)
    name <- c(name, paste(alternative_name, year, sep="-"))
  }
}

# Create data
data <- data.frame(group=name, value=countPupose) 


# Generate the layout. This function return a dataframe with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(data$value, sizetype='area')
packing$radius <- 0.94*packing$radius

# We can add these packing information to the initial data frame
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing)

# Make the plot
ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size=value, label = group), color="black") +
  theme_void() + 
  coord_equal()