library(data.table)
library(dplyr)
library(formattable)

dataset = read.csv('Data.csv')
formattable(dataset)


# A变量进行灰色加粗
# B变量进行高亮
# C变量变成百分数
# D变量标识成红绿色
# 用颜色的深浅来标识E-G变量
# H变量以箭头标注
formattable(dataset, 
            align = c("l",rep("r", NCOL(dataset) - 1)),
            list(`A` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `B` = color_bar("#FA614B"),
                 `C` = percent,
                 `D` = formatter("span", 
                                 # x ~ percent(x / 100),
                                  style = x ~ style(color = ifelse(x < 15, "red", "green"))),
                 area(col = 5:7) ~ color_tile("#DeF7E9", "#71CA97"),
                 `H` = formatter("span", 
                                 style = ~ style(color = ifelse(`H` >`B`, "green", "red")),                                    
                                 ~ icontext(sapply(`H`, function(x) if (x < 14) "arrow-down" else if (x > 16) "arrow-up" else "")))))


# 添加多种不同的元素，比如柱、线、条、饼图
library(sparkline)
df = data.frame("Type" = c("bar", "line", "bullet", "pie", "tristate", "discrete"),
                Sparkline = c(as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bar"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "line"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "bullet"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "pie"))), 
                              as.character(htmltools::as.tags(sparkline(c(-1,0,1,1,1,-1,0,2), type = "tristate"))), 
                              as.character(htmltools::as.tags(sparkline(c(1,2,7,6,5), type = "discrete")))))
out = as.htmlwidget(formattable(df))
out$dependencies = c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out


dataset$`I` = apply(dataset[, 2:8], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "bar"))))
names(dataset)[9] = "  "
out = as.htmlwidget(formattable(dataset,
                                align = c("l",rep("r", NCOL(dataset) - 1)), 
                                list(`A` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")))))                          
out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

