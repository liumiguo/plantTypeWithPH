#=========================================================
#02 MDS维度分布
#=========================================================

getwd()

library(tidyverse)
library(FactoMineR)
library(stats)
library(MASS)

#----导入数据
list.files("./data/T16_PH_data/")
data<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")


#----MDS分析
#轴没有权重信息，只是保留相对位置
mds_re2 <- isoMDS(data,k = 10)
mds_re2_df<-as.data.frame(mds_re2$points)
ggplot(mds_re2_df, aes(x=V1,y=V2))+
  geom_point()+
  labs(x="MDS1",y="MDS2")+
  theme_bw()

#MDS1
ggplot(data = mds_re2_df,aes(x=V1))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS1")+
  theme_bw()

#MDS2
ggplot(data = mds_re2_df,aes(x=V2))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS2")+
  theme_bw()

#MDS3
ggplot(data = mds_re2_df,aes(x=V3))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS3")+
  theme_bw()

#MDS4
ggplot(data = mds_re2_df,aes(x=V4))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS4")+
  theme_bw()

#MDS5
ggplot(data = mds_re2_df,aes(x=V5))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS5")+
  theme_bw()

#MDS6
ggplot(data = mds_re2_df,aes(x=V6))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS6")+
  theme_bw()

#MDS7
ggplot(data = mds_re2_df,aes(x=V7))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS7")+
  theme_bw()

#MDS8
ggplot(data = mds_re2_df,aes(x=V8))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS8")+
  theme_bw()

#MDS9
ggplot(data = mds_re2_df,aes(x=V9))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS9")+
  theme_bw()

#MDS10
ggplot(data = mds_re2_df,aes(x=V10))+
  geom_histogram(bins=50)+
  # geom_density()+
  labs(x="MDS10")+
  theme_bw()
