library("ggplot2")
library("gganimate")

df1 = read.csv("C:\\Users\\vidhi\\Desktop\\Vidhi\\PG College\\Maths\\dataset.csv")

df1 = df1[df1$Pupose == "Total", ]
data = df1[c("STATE.UT", "YEAR", "Grand.Total")]

data$Grand.Total = log1p(data$Grand.Total)

p = ggplot(data, aes(x = as.factor(YEAR), y = Grand.Total, color = STATE.UT)) + geom_point() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  theme_minimal(base_size = 20) + facet_wrap(~STATE.UT) + labs(
    title = 'Yearwise cases in each state', x = 'year', y = 'log1p(cases)', 
  ) + 
  transition_states(YEAR, 12) + transition_reveal(YEAR)


animate(p, fps = 10, duration = 10, width = 1800, height = 1200)