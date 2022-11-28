install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("arules")
install.packages("corrplot")

library(readxl)
library(dplyr)
library(ggplot2)
library(arules)
library(corrplot)

data <- readxl::read_excel(path="C:\\Users\\psc\\Desktop\\논문\\origin.xlsx", sheet="Sheet1", col_names=TRUE)
summary(data)

ro_data <- readxl::read_excel(path="C:\\Users\\psc\\Desktop\\논문\\ro_elongation.xlsx", sheet="Sheet1", col_names=TRUE)
summary(ro_data)

# R버전의 박스플롯을 그린다.
ggplot(data, aes(x=Roller1_speed, y=Elongation, group=Roller1_speed)) + geom_boxplot(outlier.color='red', outlier.shape=2, outlier.size=3) + stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")
ggplot(ro_data, aes(x=Roller1_speed, y=Elongation, group=Roller1_speed)) + geom_boxplot(outlier.color='red', outlier.shape=2, outlier.size=3) + stat_summary(fun="mean", geom="point", shape=22, size=3, fill="blue")
