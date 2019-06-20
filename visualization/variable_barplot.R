#JEON

setwd("project")


########################### b_sum #########################

library(readr)
library(dplyr)


all <- read_csv("최종본.csv")
colnames(all)

building <- all[,-c(6,9,10,11,12,15,16,17,18,20,21,23,24,25,26,27,28,29,31,32,34:49)]
colnames(building)

b_sum <- building[,c(5:13)]

b_sum <- apply(b_sum,1,sum,na.rm=T)

b_sum <- cbind(all,b_sum)

colnames(b_sum)


##################### building sum barplot ###################

library(ggplot2)

building <- b_sum[,c("fid","b_sum")]

all <- left_join(all, building, by="fid")

# write.csv(all, "최종본_추가.csv", row.names = F)

building_s <- all %>% group_by(label) %>%
  summarize(build=mean(b_sum,na.rm=T))

building_s$label <- factor(building_s$label)

barplot <- ggplot(building_s, aes(x=label, y=build, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40)

barplot

######################### green #########################
green_area <- all[,c(1,9,11,15,21,24,26,29)]
colnames(green_area)

g_sum <- green_area[,c(2:8)]

g_sum <- apply(g_sum,1,sum,na.rm=T)

g_sum <- cbind(all,g_sum)

green_area <- g_sum[,c("fid","g_sum")]
all <- left_join(all, green_area, by="fid")

green_s <- all %>% group_by(label) %>%
  summarize(green_area=mean(g_sum,na.rm=T))

green_s$label <- factor(green_s$label)

barplot <- ggplot(green_s, aes(x=label, y=green_area, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40)

barplot


# write.csv(all, "최종본_추가.csv", row.names=F)

####################### write csv ##########################

# colnames(all)
# all <- all[,c(1,2,3,4,6,10,12,16,17,18,20,23,25,27,28,31,32,34:51)]
# write.csv(all, "최종본_변수정리.csv", row.names=F)


######################### road #########################

# all <- read_csv("최종본_변수정리.csv")
road <- all[,c(1,6)]
colnames(road) <- c("fid","road_ratio")

all <- left_join(all, road, by="fid")

road_s <- all %>% group_by(label) %>%
  summarize(road=mean(road_ratio,na.rm=T))

road_s$label <- as.factor(road_s$label)
road_s$road <- as.factor(road_s$road)

barplot <- ggplot(road_s, aes(x=label, y=road, fill=road)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40)

barplot










###################### on ppt ###########################

tmp <- heat_sf[,c(1,2,33)]
tmp <- tmp %>% group_by(label) %>%
  summarize(avg_tmp=mean(avg_tmp,na.rm=T))

tmp$label <- factor(tmp$label)

barplot1 <- ggplot(tmp, aes(x=label, y=avg_tmp, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("평균 온도")

wind <- heat_sf[,c(1,23,33)]
wind <- wind %>% group_by(label) %>%
  summarize(wind=mean(X10m_wind_speed,na.rm=T))

wind$label <- factor(wind$label)

barplot2 <- ggplot(wind, aes(x=label, y=wind, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("풍속")

grid.arrange(barplot1, barplot2, nrow=1, ncol=2)


b_sum <- heat_sf[,c(1,33,34)]
building <- b_sum %>% group_by(label) %>%
  summarize(building=mean(b_sum,na.rm=T))

building$label <- factor(building$label)

barplot1 <- ggplot(building, aes(x=label, y=building, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("평균 건물밀집도")

g_sum <- heat_sf[,c(1,33,35)]
green_s <- g_sum %>% group_by(label) %>%
  summarize(green_area=mean(g_sum,na.rm=T))

green_s$label <- factor(green_s$label)

barplot2 <- ggplot(green_s, aes(x=label, y=green_area, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("평균 녹지면적")

grid.arrange(barplot1, barplot2, nrow=1, ncol=2)

ele <- heat_sf[,c(1,33,30)]
avg_ele <- ele %>% group_by(label) %>%
  summarize(avg_ele=mean(avg_ele,na.rm=T))

avg_ele$label <- factor(avg_ele$label)

barplot1 <- ggplot(avg_ele, aes(x=label, y=avg_ele, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("평균 해발고도")

bad <- heat_sf[,c(1,33,21)]
bad <- bad %>% group_by(label) %>%
  summarize(avg_bad=mean(순수불량_비율,na.rm=T))

bad$label <- factor(bad$label)

barplot2 <- ggplot(bad, aes(x=label, y=avg_bad, fill=label)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = c("red", "green", "blue", "purple") ) +
  scale_fill_hue(c = 40) +
  ggtitle("평균 토양 배수등급 불량 비율")

grid.arrange(barplot1, barplot2, nrow=1, ncol=2)
