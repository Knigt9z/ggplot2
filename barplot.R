library(ggplot2)
library(viridis)
library(ggstatsplot)
library(dplyr)
library(tidyverse)
heart <- na.omit(heart)
#position参数：
#"identity"：默认值，使用原始数据作为条形图的高度。每个组别的条形并排排列。
#"stack"：堆积条形图，将不同组别的条形叠加显示。
#"fill"：百分比堆叠条形图，将各组别的条形叠加显示，并将高度标准化为百分比，以展示各组别的相对比例。
#"dodge"：并排放置的条形图，不同组别的条形通过并排摆放来显示，方便进行比较。

#stat参数
#"count":计算每个分组中观测值的频数。
#"bin"：将连续变量离散化为分组，并计算每个分组中的观测值频数。
#"sum"：计算每个分组中观测值的总和。
#"mean"：计算每个分组中观测值的平均值。
#"median"：计算每个分组中观测值的中位数



# #geom_bar ---------------------------------------------------------------
ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "dodge",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "identity",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "fill",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "stack",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()


#更好看的百分比堆积柱形图
library(ggstatsplot)
ggbarstats(data=heart,
           AHD,
           Thal,
           palette = 'Set3', #设置颜色板
           results.subtitle = F,
           title = '不同Thal下AHD的分布')+ 
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.2))


# geom_col() --------------------------------------------------------------

#创建绘图所需数据源
df_Thal_Age <- heart %>% 
  group_by(Thal) %>% 
  summarise(Agemean=mean(Age)) %>% 
  arrange(desc(Agemean))

#渐变色填充geom_col()
ggplot(data=df_Thal_Age,aes(x=Thal,y=Agemean,fill=Agemean))+
  geom_col()+
  scale_fill_viridis(begin = 1,end = 0.5,option = "G")+   #渐变色填充
  geom_text(aes(label=Agemean),hjust=1.5,color="white")+  #柱子数值标签
  labs(x="Thal",y="Agemean")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "none")





