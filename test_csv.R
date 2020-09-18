library(curl)
library(ggplot2)

p <- read.csv(curl("https://raw.githubusercontent.com/maxlindmark/cod_condition/master/data/mdat_cond.csv"))

ggplot(p, aes(X, Y, fill = est2)) +
  geom_raster() +
  facet_wrap(~year, ncol = 5) +
  theme_bw() +
  coord_cartesian(expand = 0) + 
  ggtitle("Prediction (random + fixed")

