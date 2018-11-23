# https://github.com/jespermaag/gganatogram

# devtools::install_github("jespermaag/gganatogram")

library(ggplot2)
library(ggpolypath)
library(gganatogram)
library(dplyr)

organPlot <- data.frame(organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"), 
                        type = c("circulation", "circulation",  "nervous system", "nervous system", "digestion", "digestion", "digestion"), 
                        colour = c("red", "red", "purple", "purple", "orange", "orange", "orange"), 
                        value = c(10, 5, 1, 8, 2, 5, 5), 
                        stringsAsFactors=F)
# 创建包含器官组织、颜色、数值的数据框
head(organPlot)

# 使用函数gganatogram，根据颜色填充器官
gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='male', fill="colour")

# 使用ggplot主题和函数调整图上细节
gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") + 
theme_void()

# 使用hgMale_key绘制所有可用组织，这是一个可用的对象
hgMale_key$organ

gganatogram(data=hgMale_key, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") +theme_void()

# 要跳过图表的轮廓，使用outline = F
organPlot %>%
  dplyr::filter(type %in% c('circulation', 'nervous system')) %>%
  gganatogram(outline=F, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") + 
  theme_void()

# 根据给予每个器官的值来填充组织
gganatogram(data=organPlot, fillOutline='#a6bddb', organism='human', sex='male', fill="value") + 
  theme_void() +
  scale_fill_gradient(low = "white", high = "red")

# 使用facet_wrap来比较组，首先创建两个数据框以及设置类型列中的不同数值和条件
compareGroups <- rbind(data.frame(organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"), 
                                  colour = c("red", "red", "purple", "purple", "orange", "orange", "orange"), 
                                  value = c(10, 5, 1, 8, 2, 5, 5), 
                                  type = rep('Normal', 7), 
                                  stringsAsFactors=F),
                       data.frame(organ = c("heart", "leukocyte", "nerve", "brain", "liver", "stomach", "colon"), 
                                  colour = c("red", "red", "purple", "purple", "orange", "orange", "orange"), 
                                  value = c(5, 5, 10, 8, 2, 5, 5), 
                                  type = rep('Cancer', 7), 
                                  stringsAsFactors=F))
gganatogram(data=compareGroups, fillOutline='#a6bddb', organism='human', sex='male', fill="value") + 
  theme_void() +
  facet_wrap(~type) +
  scale_fill_gradient(low = "white", high = "red") 

# 按系统展示
gganatogram(data=hgMale_key, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") +
  theme_void() +
  facet_wrap(~type)

gganatogram(data=hgMale_key, outline=F, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") +
  theme_void() +
  facet_wrap(~type, scale='free')

# 按器官展示
organtype <- organPlot
organtype %>%
  mutate(type=organ) %>%
  gganatogram( outline=F, fillOutline='#a6bddb', organism='human', sex='male', fill="colour") +
  theme_void() +
  facet_wrap(~type, scale='free')