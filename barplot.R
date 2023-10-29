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

#dodge
ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "dodge",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

#identity
ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "identity",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

#fill
ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "fill",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()

#stack
ggplot(data=heart,aes(x=Thal,fill=AHD))+  #填充色按照AHD来分
  geom_bar(stat = "count",position = "stack",width = 0.4)+
  scale_fill_manual(values = c("#104E8B", "#87CEFA"))+
  theme_minimal()


#更好看的百分比堆积柱形图
#ggbarstats
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



# Economic bar -------------------------------------------------------------------
#创建数据
names <- c(
  "Hantavirus", "Tularemia", "Dengue", "Ebola", "E. coli", 
  "Tuberculosis", "Salmonella", "Vaccinia", "Brucella"
)

# Name is an ordered factor. We do this to ensure the bars are sorted.
data <- data.frame(
  count = c(6, 7, 7, 9, 11, 15, 17, 18, 54), 
  name = factor(names, levels = names),
  y = seq(length(names)) * 0.9
)



library(grid)
library(shadowtext)
#基本条形图
plt <- ggplot(data=data)+
  geom_col(aes(count, name),fill="#076fa2",width = 0.6)
plt

#自定义布局
plt <- plt + 
  scale_x_continuous(  # 设置水平轴的连续型变量的刻度范围、刻度标签和其他属性
    limits = c(0, 55.5),  #设置水平轴的刻度范围为0到55.5
    breaks = seq(0, 55, by = 5), #设置水平轴的刻度标签为从0到55，间隔为5
    expand = c(0, 0), # 设置水平轴的范围不向两侧扩展，保持原始范围。
    position = "top"  # 设置刻度标签位于水平轴的顶部。
  ) +
  # 设置垂直轴的离散型变量的刻度范围、刻度标签和其他属性。
  scale_y_discrete(expand = expansion(add = c(0, 0.5))) +
  theme(  #设置图形的主题，包括背景色、网格线、刻度线、轴线和标签等属性。
    panel.background = element_rect(fill = "white"), #设置图形背景色为白色。
    panel.grid.major.x = element_line(color = "#A8BAC4", linewidth = 0.3), #设置水平轴网格线的颜色为"#A8BAC4"，线宽为0.3。
    axis.ticks.length = unit(0, "mm"), #设置刻度线的长度为0，即不显示刻度线。
    axis.title = element_blank(), #移除水平和垂直轴的标题。
    axis.line.y.left = element_line(color = "black"), #设置垂直轴的左侧线条颜色为黑色。
    axis.text.y = element_blank()  #移除垂直轴的刻度标签。
  )

plt


#添加标签
plt <- plt + 
  geom_shadowtext(
    data = subset(data, count < 8),#选择数据集data中count值小于8的子集作为geom_shadowtext的数据。
    aes(count, y = name, label = name),
    hjust = 0, #水平对齐的设置。0表示左对齐，1表示右对齐，0.5表示中心对齐。
    nudge_x = 0.3, #表示在x方向上将文本微调0.3个单位。
    colour = "#076fa2", #设置文本颜色为蓝色。
    bg.colour = "white", #设置文本背景颜色为白色。
    bg.r = 0.2,  #设置文本背景阴影的半径为0.2。
    size = 7  #设置文本的字体大小为7。
  ) + 
  geom_text(
    data = subset(data, count >= 8),  #表示选择数据集data中count值大于等于8的子集作为geom_text的数据。
    aes(0, y = name, label = name),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    size = 7
  )
plt

#添加注释和最终调整
plt <- plt +
  labs(
    title = "Escape artists",  #主标题
    subtitle = "Number of laboratory-acquired infections, 1970-2021" #副标题
  ) + 
  theme(
    plot.title = element_text( #设置图表主标题的样式
      face = "bold", #设置字体样式为粗体。
      size = 22
    ),
    plot.subtitle = element_text(   #设置图表副标题的样式
      size = 20
    )
  )
plt



