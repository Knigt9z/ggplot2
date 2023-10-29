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


# circular barplot --------------------------------------------------------

hike_data <- readr::read_rds('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds')

#将hike_data数据框中的location列中的每个元素按照分隔符" -- "分割，并提取出第一个部分作为新的region列。as.factor()函数将region列转换为因子变量。
hike_data$region <- as.factor(word(hike_data$location, 1, sep = " -- "))

#将hike_data数据框中的length列中的每个元素按照空格分割，并提取出第一个部分作为新的length_num列。as.numeric()函数将length_num列转换为数值型。
hike_data$length_num <- as.numeric(sapply(strsplit(hike_data$length, " "), "[[", 1))


plot_df <- hike_data %>%
  group_by(region) %>%    #按region分组
  summarise(
    sum_length = sum(length_num),  #每个分组中的length_num列的总和
    mean_gain = mean(as.numeric(gain)), #计算每个分组中的gain列的平均值
    n = n()  #计算每个分组中的行数
  ) %>%
  mutate(mean_gain = round(mean_gain, digits = 0)) #将mean_gain列的数值取整为0位小数。



#基本雷达图
plt <- ggplot(plot_df) +
  geom_hline(  #geom_hline()函数绘制水平线，用于创建自定义的面板网格。
    aes(yintercept = y),  #设置水平线的y轴截距为y
    data.frame(y = c(0:3) * 1000), #创建一个数据框，其中包含了4个水平线的y轴截距（0、1000、2000和3000）。
    color = "lightgrey"
  ) + 
  geom_col(
    aes( #设置x轴为region列的值，使用sum_length列的值作为y轴，使用n列的值作为填充颜色。
      x = reorder(str_wrap(region, 5), sum_length),
      y = sum_length,
      fill = n
    ),
    position = "dodge2", #设置条形图的位置为"dodge2"，使得条形图在同一位置堆叠。
    show.legend = TRUE,
    alpha = .9
  ) +
  geom_point(
    aes(  #设置x轴为region列的值，使用mean_gain列的值作为y轴。
      x = reorder(str_wrap(region, 5),sum_length), 
      y = mean_gain
    ),
    size = 3,
    color = "gray12"  #设置点的颜色为灰色。
  ) +
  geom_segment(  #geom_segment()函数绘制线段图。
    aes(  #设置起点和终点的x轴为region列的值，起点的y轴为0，终点的y轴为3000。
      x = reorder(str_wrap(region, 5), sum_length),
      y = 0,
      xend = reorder(str_wrap(region, 5), sum_length),
      yend = 3000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  coord_polar()  #将坐标系设置为极坐标，使图表呈现圆形的形状。
plt

#添加批注和图例
plt <- plt +
  annotate(   #annotate()函数在图表中添加注释
    x = 11,   #注释的位置
    y = 1300,
    label = "Mean Elevation Gain\n[FASL]", #注释的文本
    geom = "text", #注释的几何形状
    angle = -67.5,  #注释的角度
    color = "gray12",  #注释的颜色
    size = 2.5,
  ) +
  annotate(
    x = 11, 
    y = 3150,
    label = "Cummulative Length [FT]",
    geom = "text",
    angle = 23,
    color = "gray12",
    size = 2.5,
  ) +
  annotate(
    x = 11.7, 
    y = 1100, 
    label = "1000", 
    geom = "text", 
    color = "gray12", 
  ) +
  annotate(
    x = 11.7, 
    y = 2100, 
    label = "2000", 
    geom = "text", 
    color = "gray12", 
  ) +
  annotate(
    x = 11.7, 
    y =3100, 
    label = "3000", 
    geom = "text", 
    color = "gray12", 
  ) +
  scale_y_continuous(  #设置y轴的连续刻度
    limits = c(-1500, 3500),  #设置y轴的取值范围
    expand = c(0, 0),  #设置y轴的扩展。
    breaks = c(0, 1000, 2000, 3000) #设置y轴的刻度值。
  ) + 
  scale_fill_gradientn(  #设置填充颜色的渐变。
    "Amount of Tracks",  #设置图例的标题为"Amount of Tracks"。
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195") #设置填充颜色的渐变。
  ) +
  guides(  #设置图例的样式。
    fill = guide_colorsteps(  #设置填充颜色的图例样式。
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    axis.title = element_blank(),#设置轴标题的样式。
    axis.ticks = element_blank(),#设置轴刻度线的样式。
    axis.text.y = element_blank(),#设置y轴刻度标签的样式
    axis.text.x = element_text(color = "gray12", size = 12),#设置x轴刻度标签的样式
    legend.position = "bottom",#设置图例的位置。
  )

#最终调整
plt <- plt + 
  labs(
    title = "\nHiking Locations in Washington",  #设置图表的主标题。
    subtitle = paste(    #设置图表的副标题
      "\nThis Visualisation shows the cummulative length of tracks,",
      "the amount of tracks and the mean gain in elevation per location.\n",
      "If you are an experienced hiker, you might want to go",
      "to the North Cascades since there are a lot of tracks,",
      "higher elevations and total length to overcome.",
      sep = "\n"
    ),
    caption = "\n\nData Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: TidyX Crew (Ellis Hughes, Patrick Ward)\nLink to Data: github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md") +
  #设置图表的注释。
  theme(
    text = element_text(color = "gray12"), #设置文本的颜色。
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),#设置图表主标题的样式。
    plot.subtitle = element_text(size = 14, hjust = 0.05),#设置图表副标题的样式
    plot.caption = element_text(size = 10, hjust = .5), #设置图表注释的样式。
    panel.background = element_rect(fill = "white", color = "white"),#设置图表背景的填充颜色。
    panel.grid = element_blank(),#设置图表网格线的样式。
    panel.grid.major.x = element_blank()  #设置x轴主要网格线的样式。
  )
plt

