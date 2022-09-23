#=============================================================================
# 02 传统表型与株型的相关性
#=============================================================================

getwd()

library(tidyverse)
library(FactoMineR)
library(stats)
library(MASS)

#----导入数据
list.files("./data/T16_PH_data/")
data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

data_trad_para<-read_csv("./data/T16_tradition_data/gwas_tradtion_shape.csv")
data_trad_para<-data_trad_para%>%
  mutate(Aspect_Rate=height/width)
names(data_trad_para)
#----MDS分析
mds_re2 <- isoMDS(data_BD,k = 5)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:5)
mds_re2_df2<-mds_re2_df%>%
  rownames_to_column(var="ID")

#---合并数据
str(data_BD)
str(mds_re2_df2)

# names(join_data)
join_data<-left_join(data_trad_para,mds_re2_df2)%>%
  drop_na()%>%
  dplyr::select(tips:MDS5)
names(join_data)<-c("Tips",
                    "Convex hull area",
                    "Solidity",
                    "Perimeter",
                    "Width",
                    "Height",
                    "Aspect rate",
                    "MDS1",
                    "MDS2",
                    "MDS3",
                    "MDS4",
                    "MDS5")


#---相关性图
library(ggcorrplot)
corr <- cor(join_data)
p.mat <- cor_pmat(join_data)

pp<-ggcorrplot(corr, p.mat = p.mat)+
  theme(axis.text.x = element_text(size=12,angle = 45, hjust = 1,color="black"),
        axis.text.y = element_text(size=12,color="black"))+
  theme(text = element_text(size = 14, color="black"))
pp
