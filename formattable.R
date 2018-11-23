library(data.table)
library(dplyr)

#Download the Austin indicator data set
#Original data set from: https://data.austintexas.gov/City-Government/Imagine-Austin-Indicators/apwj-7zty/data
austinData= data.table::fread('https://raw.githubusercontent.com/lgellis/MiscTutorial/master/Austin/Imagine_Austin_Indicators.csv', data.table=FALSE, header = TRUE, stringsAsFactors = FALSE)

i1 <- austinData %>%
  filter(`Indicator Name` %in% 
           c('Prevalence of Obesity', 'Prevalence of Tobacco Use', 
             'Prevalence of Cardiovascular Disease', 'Prevalence of Diabetes')) %>%
  select(c(`Indicator Name`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`)) %>%
  mutate (Average = round(rowMeans(
    cbind(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`), na.rm=T),2), 
    `Improvement` = round((`2011`-`2016`)/`2011`*100,2))
prevalence = i1

library(formattable)
# formattable()函数：得到该数据可视化的整洁版本，表头自动加粗
formattable(prevalence)

# align参数：设置不同行的对齐方式，”l”表示左对齐，“r”表示右对齐
formattable(prevalence, align = c("l",rep("r", NCOL(prevalence) - 1)))

# 对Average变量进行高亮
prevalence[, "Improvement"] = prevalence[, "Improvement"] / 100
formattable(prevalence, 
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `Average` = color_bar("#FA614B"), 
                 `Improvement` = percent))

# 不同疾病的变化趋势即Improvement的取值，上升标识成绿色，下降标识成红色
formattable(prevalence, 
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `Average` = color_bar("#FA614B"), 
                 `Improvement` = formatter("span", 
                                           x ~ percent(x / 100),
                                           style = x ~ style(color = ifelse(x < 0, "red", "green")))))

# “Yes”和“No”来标识
formattable(prevalence, 
            align = c("l",rep("r", NCOL(prevalence) - 1)),
            list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")), 
                 `Average` = color_bar("#FA614B"), 
                 `Improvement` = formatter("span", 
                                           x ~ icontext(ifelse(x > 0, "ok", "remove"), ifelse(x > 0, "Yes", "No")), 
                                           style = x ~ style(color = ifelse(x < 0, "red", "green")))))

# 用颜色的深浅来标识prevalence的大小
formattable(prevalence, align = c("l",rep("r", NCOL(prevalence) - 1)), list(
  `Indicator Name` = formatter("span", style = ~ style(color = "grey",font.weight = "bold")), 
  area(col = 2:7) ~ color_tile("#DeF7E9", "#71CA97")))

# 在表格中表示两组之间的均值差异
prev.sig = prevalence[, c(1, 6:7)]
prev.sig$z = c(-1.97, .12, 2.2, 2.1)
prev.sig

formattable(prev.sig,
            list(z = FALSE,
                 `2016` = formatter("span", 
                                    style = ~ style(color = ifelse(`2016` >`2015`, "green", "red")),                                    
                                    ~ icontext(sapply(`z`, function(x) if (x < -1.96) "arrow-down" else if (x > 1.96) "arrow-up" else ""), `2016`))))

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


prevalence$` ` = c(4.1, -.3, .5, 1.4)
prevalence$`2012` = apply(prevalence[, 2:7], 1, FUN = function(x) as.character(htmltools::as.tags(sparkline(as.numeric(x), type = "line"))))
names(prevalence)[3] = "  "
new.prevalance = prevalence[, c(1, 2, 3, 7, 10)]                          
out = as.htmlwidget(formattable(new.prevalance,
                                align = c("l",rep("r", NCOL(prevalence) - 1)), 
                                list(`Indicator Name` = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                                     " " = formatter("span", 
                                                     style = ~ style(color = ifelse(`2016` >`2011`, "green", "red")),                                    
                                                     ~ icontext(sapply(` `, function(x) if (x < -1.96) "arrow-down" else if (x > 1.96) "arrow-up" else ""))))))                          
out$dependencies <- c(out$dependencies, htmlwidgets:::widget_dependencies("sparkline", "sparkline"))
out

# 自定义数据集                                                       
df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny", 
    "Hans", "Leo", "John", "Emily", "Lee"), 
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

# age从白到橙标识，grade为A的以绿标识，score粉色高亮，final排序，registered标勾叉                                                                       
formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A", 
    style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
    style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
    x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
    style = x ~ style(color = ifelse(x, "green", "red")),
    x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))                                                                      
