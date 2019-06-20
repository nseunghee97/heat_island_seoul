library(tidyverse)
library(ggplot2)
library(GGally)

wind <- read.csv("C:/Users/shd04/Desktop/2019/1_Data_Science/project/qgis/wind_analysis/그리드_풍속_정리.csv", header=TRUE)

ggpairs(wind, columns = c(5,4,6,7,8,9),   
        axisLabels = "show", columnLabels = c("10m 풍속","80m 풍속","풍속 차", "average", "max", "min"))
