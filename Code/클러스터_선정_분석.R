library(car)
library(MASS)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(lmtest)
library(corrplot)

## 클러스터별 비교
setwd('C:/Users/요우용/Desktop/데사입_프로젝트/그리드화')
file1 <- read.csv('최종본.csv',header=T,stringsAsFactors=F,fileEncoding='utf-8')
head(file1)

# 1) 평균온도
mean_tmp <- file1 %>% group_by(label) %>% summarize(mean_tmp = mean(avg_tmp))
ggplot(mean_tmp) + geom_col(aes(x=as.factor(label),y=mean_tmp,group=as.factor(label)))

# 2) 평균고도
mean_ele <- file1 %>% group_by(label) %>% summarize(mean_ele = mean(avg_ele))
ggplot(mean_ele) + geom_col(aes(x=as.factor(label),y=mean_ele,fill=as.factor(label)))

# 3) 평균 바람 10m
mean_wind_10 <- file1 %>% group_by(label) %>% summarize(mean_wind = mean(X10m_wind_speed))
ggplot(mean_wind_10) + geom_col(aes(x=as.factor(label),y=mean_wind,fill=as.factor(label)))

# 4) 평균 바람 80m
mean_wind_80 <- file1 %>% group_by(label) %>% summarize(mean_wind = mean(X80m_wind_speed))
ggplot(mean_wind_80) + geom_col(aes(x=as.factor(label),y=mean_wind,fill=as.factor(label)))

# 5) 풍속의 차이
mean_diff <- file1 %>% group_by(label) %>% summarize(mean_wind = mean(difference_wind))
ggplot(mean_diff) + geom_col(aes(x=as.factor(label),y=mean_wind,fill=as.factor(label)))

# 6) 평균 미세먼지 차이
mean_dust <- file1 %>% group_by(label) %>% summarize(mean_dust = mean(avg_dust_10))
ggplot(mean_dust) + geom_col(aes(x=as.factor(label),y=mean_dust,fill=as.factor(label)))

# 7) 순수 불량비율
mean_bad <- file1 %>% group_by(label) %>% summarize(mean_bad = mean(순수불량_비율))
ggplot(mean_bad) + geom_col(aes(x=as.factor(label),y=mean_bad,fill=as.factor(label)))




################
### 그리드화 ###
################
setwd('C:/Users/요우용/Desktop/데사입_프로젝트/그리드화')
file1_rescale <- read.csv('최종본_rescale.csv',header=T,stringsAsFactors=F,fileEncoding='utf-8')
file2_rescale <- read.csv('최종본_변수정리_rescale.csv',header=T,stringsAsFactors=F,fileEncoding='utf-8')
file1_rescale2 <- read.csv('최종본_rescale2.csv',header=T,stringsAsFactors=F,fileEncoding='utf-8')
file2_rescale2 <- read.csv('최종본_변수정리_rescale2.csv',header=T,stringsAsFactors=F,fileEncoding='utf-8')


## 클러스터1들의 상관관계와 varImPlot 보기
c1_rescale <- file1_rescale %>% select(-fid) %>% filter(label == 0)
c2_rescale <- file2_rescale %>% select(-fid) %>% filter(label == 0)
c1_rescale2 <- file1_rescale2 %>% select(-fid) %>% filter(label == 0)
c2_rescale2 <- file2_rescale2 %>% select(-fid) %>% filter(label == 0)

# 1-1) c1_rescale
data0 <- c1_rescale %>% select(-label,-min_tmp,-max_tmp)
head(data0) # 암벽/석산(임지), 공업나지(도시), 가축/사육(도시), 공원묘지(임지), 과수원(농지) == 0

corr0 <- cor(data0)
corr0
corrplot(corr0)

set.seed(1)
m_random <- randomForest(avg_tmp~.,data=data0,importance=T)
m_random
importance(m_random)
varImpPlot(m_random,main='varlmPlot of 변수정리X_cluster1_rescale')


# 1-2) c1_rescale2
data1 <- c1_rescale2 %>% select(-label,-min_tmp,-max_tmp)
head(data1)

corr1 <- cor(data1)
corrplot(corr1)

set.seed(1)
m_random <- randomForest(avg_tmp~.,data=data1,importance=T)
m_random
importance(m_random)
varImpPlot(m_random,main='varlmPlot of 변수정리X_cluster1_rescale2')


# 2-1) c2_rescale
data2 <- c2_rescale %>% select(-label,-min_tmp,-max_tmp)
head(data2) # 공업나지(도시), 매립지(도시) == 0

corr2 <- cor(data2)
corrplot(corr2)

set.seed(1)
m_random <- randomForest(avg_tmp~.,data=data2,importance=T)
m_random
importance(m_random)
varImpPlot(m_random,main='varlmPlot of 변수정리O_cluster1_rescale')


# 2-2) c2_rescale2
data3 <- c2_rescale2 %>% select(-label,-min_tmp,-max_tmp)
head(data3) # 발전시설(도시), 경지정리답(농지), 가축/사육시설(도시), 매립지(도시) == 0

corr3 <- cor(data3)
corrplot(corr3)

set.seed(1)
m_random <- randomForest(avg_tmp~.,data=data3,importance=T)
m_random
importance(m_random)
varImpPlot(m_random,main='varlmPlot of 변수정리O_cluster1_rescale2')


## c1_rescale2로 확정
## c1_rescale2 비슷한 변수 제거 및 설명력을 낮추는 요소들(%InMSE -인 elements) 삭제

data4 <- data1 %>% select(-max_ele,-min_ele,-diff_ele,-max_dust_10,-min_dust_10,-X80m_wind_speed,-인공녹지.도시._비율,
                          -자연초지.임지._비율,-인공초지.임지._비율,-경지정리답.농지._비율) 

corr4 <- cor(data4)
corrplot(corr3)
corr4

set.seed(1) 
m_random <- randomForest(avg_tmp~.,data=data4,importance=T)
m_random # 32.13
varImpPlot(m_random,main='varlmPlot of 변수정리O_cluster1_rescale2 변수 일부 삭제')

vars <- importance(m_random)[,'%IncMSE']
vars_df <-as.data.frame(vars)
vars_df # %IncMSE data.frame 
write.csv(vars_df,'MSE 데이터프레임.csv',row.names=T)






