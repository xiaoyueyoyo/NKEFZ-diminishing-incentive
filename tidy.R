### LGEE数据与实证分析

# 初始设定 ----------------------------

library(Matrix)
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)  # install.packages('openxlsx')
library(dplyr)
library(lubridate)
library('stringr')

rm(list=ls()); gc()

# 导入数据，整理数据------------------
setwd("PostPHD/Rcode/LGEE")
b3 <- read_excel("b3.xlsx")
b3 <- separate(b3, col = 指标名称, into = c("province", "variable"), sep = "(?<=:)(?=.)")
b3[] <- lapply(b3, function(x) gsub(':', '', x))


# 获取数据框的列名
col_names <- names(b3)

# 替换第3到第10个变量名称为3009到3016
col_names[4:17] <- paste0(2009:3022)

# 将修改后的列名赋值回数据框
names(b3) <- col_names

b3 <- head(b3, -2)

chr <- c("城镇人口比重_%",
         "地方公共财政支出:城乡社区事务_万元",
         "地方公共财政支出:教育_万元",
         "地方公共财政支出:节能环保_万元",
         "地方公共财政支出:科学技术_万元",
         "地方公共财政支出:农林水事务_万元",
         "地方公共财政支出:社会保障和就业_万元",
         "地方公共财政支出:文化体育与传媒_万元",
         "地方公共财政支出:一般公共服务_万元",
         "地方公共财政支出:医疗卫生与计划生育_万元",
         "废气中主要污染物排放量:二氧化硫_万吨",
         "废水中主要污染物排放量:化学需氧量_吨",
         "高等学校在校学生数_人",
         "户籍人口:总人口_人",
         "进出口金额:人民币_亿元",
         "森林覆盖率_%",
         "GDP_亿元",
         "GDP:第二产业_亿元",
         "GDP:第一产业_亿元")

variable_values <- rep(chr, 9)
b3$variable <- variable_values
b3 <- b3[,-c(3)]

b3_long <- tidyr::pivot_longer(b3, 
                               cols = -c(province, variable), # 列选择，不需要转换的列
                               names_to = "year",         # 新列的名称
                               values_to = "value_column")         # 值列的名称

# 打印转换后的长数据
panel09_22 <- rbind(b1_long,b2_long,b3_long)
saveRDS(panel09_22, file = "panel09_22.rds")

panel <- readRDS("panel09_22.rds")
省份id <- read_excel("省份id.xlsx")

variable_values <- rep(chr,each=14)
variable_values <- rep(variable_values,31)
panel$variable <- variable_values
panel <- panel[,-c(3)]




library(dplyr)
panel <- left_join(panel, 省份id[,1:2], by = "province")


install.packages("tidyr")
library(tidyr)
EFT <- read_excel("EFT.xlsx")
long_eft <- gather(EFT, key = "year", value = "EFT", -id, -province)
long_eft$NKEFZ <- ifelse(long_eft$id %in% c(1, 2, 6, 9,10,11,13,15),0,1)


panel_wide <- pivot_wider(data = panel, names_from = variable, values_from = value_column)
panel <- left_join(panel_wide,long_eft)
saveRDS(panel,file="paneldata.rds")
write.csv(panel,file="paneldata.csv")

#下一步：导入STATA，更名，ln平均转换，缺失值填充，计量分析


shouru <- gather(收入,key = "year", value = "预算收入", -id, -province)
zhichu <- gather(支出,key = "year", value = "预算支出", -id, -province)
year <- rep(2022:2009,each = 31)
shouru$year <- year
zhichu$year <- year
shouzhi <- left_join(shouru,zhichu)
write.csv(shouzhi,file="shouzhi.csv")




##国家统计局数据：

library(dplyr)
library(tidyr)

# 获取文件列表
#file_list <- list.files(path = "rawpanel", full.names = TRUE)

# 创建一个空的数据框，用于存储合并后的数据
#merged_data <- data.frame()
table1 <- expand.grid(id = 1:31, year = 2022:2009)
table1$id <- as.character(table1$id)
year = repeat(2022:2009)
variables = c('人均地区生产总值(元/人)',
'地区生产总值(亿元)',
'第二产业增加值(亿元)',	
'森林覆盖率(%)',
'经营单位所在地进出口总额(千美元)',	
'年末常住人口(万人)',	
'普通高等学校在校学生数(万人)',
'二氧化硫排放量(万吨)',
'化学需氧量排放量(万吨)',
'地方财政一般公共服务支出(亿元)',
'地方财政医疗卫生支出(亿元)',
'地方财政文化体育与传媒支出(亿元)',
'地方财政社会保障和就业支出(亿元)',
'地方财政科学技术支出(亿元)',
'地方财政环境保护支出(亿元)',
'地方财政农林水事务支出(亿元)',
'地方财政教育支出(亿元)',
'地方财政城乡社区事务支出(亿元)',
'城镇人口(万人)')


file_list <- list.files(path = "rawpanel", pattern = "\\d+\\.csv", full.names = TRUE)
file_list <- file_list[order(as.integer(gsub("\\D", "", file_list)))]
id <- as.character(1:31)
# 循环读取每个文件并处理数据
for (i in 1:19) {
  # 读取CSV文件
  data <- fread(file_list[i], header = TRUE, skip = 3 ,nrows = 32, encoding = "UTF-8",col.names = c("id", as.character(2022:2009)))
  data$id <- as.character(id)
  data_long <- gather(data,key ="year", value = i,-id)
  data_long$year <- as.integer(data_long$year)
  table1 <- left_join(table1, data_long, by = c("id","year"))
}

variablename <- names(table1)

# 替换第3到第10个变量名称为3009到3016
variablename[3:21] <- variables

names(table1) <- variablename

write.csv(table1,"panelnew.csv")


h1 <- read_excel("h1.xls",skip=3,n_max=31,col_names = TRUE)
h2 <- read_excel("h2.xls",skip=3,n_max=31,col_names = TRUE)
cn <- c("省份",2017:2009)
colnames(h1) <- cn
cn2 <- c("省份",2022:2009)
colnames(h2) <- cn2
h1_long <- pivot_longer(h1,2:10,names_to = "year",values_to = "自然保护区面积（万公顷）")
##h1_long <- gather(h1,key="year",value = "自然保护区面积（万公顷）",-地区)
h2_long <- pivot_longer(h2,2:15,names_to = "year",values_to = "国家级自然保护区面积（万公顷）")
id <- rep(1:31,each=14)
h2_long <- h2_long %>%
  mutate(id = id)
write_csv(h2_long,"国家级保护区.csv")
