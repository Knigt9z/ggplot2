library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)

ggplot(data = iris,aes(x=Sepal.Length,y=Petal.Length)) +
  geom_pointdensity(adjust=2，size = 3, shape = 17)+  #adjust控制点的平滑程度
  scale_color_viridis()  #颜色映射      