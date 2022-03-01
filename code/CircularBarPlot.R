library("ggplot2")

df1 = read.csv("/home/the-techie/Desktop/Maths_Project/data12.csv")

states = unique(df1$STATE.UT)

getData <- function(year){
  total_crimes = c()
  
  for(x in states){
    
    data = subset(df1, df1$STATE.UT == x & df1$YEAR == year & df1$Pupose == "Total")
    
    k = data$Grand.Total
    
    if(length(k) == 0){
      k = 0
    }
    
#    cat("State:\t", x, "\n", "actual:\t", k)
    
    k = log1p(k)
    
#    cat("\nnew:\t", k, "\n\n")
    
    total_crimes = c(total_crimes, k)
    
  }

  return(total_crimes)
  
}

data_2001 = getData(2001)
data_2002 = getData(2002)
data_2003 = getData(2003)
data_2004 = getData(2004)
data_2005 = getData(2005)
data_2006 = getData(2006)
data_2007 = getData(2007)
data_2008 = getData(2008)
data_2009 = getData(2009)
data_2010 = getData(2010)
data_2011 = getData(2011)
data_2012 = getData(2012)


df = data.frame(
  "states" = states,
  "Y2001" = data_2001,
  "Y2002" = data_2002,
  "Y2003" = data_2003,
  "Y2004" = data_2004,
  "Y2005" = data_2005,
  "Y2006" = data_2006,
  "Y2007" = data_2007,
  "Y2008" = data_2008,
  "Y2009" = data_2009,
  "Y2010" = data_2010,
  "Y2011" = data_2011,
  "Y2012" = data_2012
)

df$id = 1:35

years = 2001:2012


label_data = df
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$id) /number_of_bar
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)


label_data$dist = nchar(df$states)/10


#==========================================================
x = 1:35

# for every year, change this also

df = df[order(df$Y2012), ]

p = ggplot(df, aes(x = x, y = Y2012)) + 
  geom_bar(stat="identity", colour = df$Y2012) +
  ylim(-3, 20)+
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1, 5), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  geom_text(data = label_data, aes(x=id, y=  10 - dist, label=df$states, hjust=hjust)
            , color="black", fontface="bold",alpha=0.6, show.legend = TRUE,
            size=8, angle= label_data$angle, inherit.aes = FALSE )+
  geom_text(aes(x = 0, y = -3, label = "2012"), color="black", fontface="bold",alpha=0.6, 
            size=15)

p