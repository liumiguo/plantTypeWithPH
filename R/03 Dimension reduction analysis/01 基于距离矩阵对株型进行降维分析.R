#=============================================================================
# 01 基于距离矩阵对株型进行降维分析
#=============================================================================

getwd()

library(tidyverse)
library(FactoMineR)
library(stats)
library(MASS)

#----导入数据
list.files("./data/T16_PH_data/")
data<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#----解析编号对应的材料信息
gwas_name<-list.files("D:/DataSet_From_Web/GWAS_plant_type/GWAS")
gwas_name_df<-data.frame(gwas_name)%>%
  dplyr::mutate(loc=str_locate(gwas_name,pattern = "\\(")[,1],
                ID=str_sub(gwas_name,1,loc-1),
                LinesName=toupper(str_sub(gwas_name,loc+1,-2)))

gwas_name_df_sum<-gwas_name_df%>%
  group_by(LinesName)%>%
  summarise(N=n())

#----确定材料数量
dim(data)
ID_num<-colnames(data)
data_df<-data.frame(ID_num)%>%
  dplyr::mutate(loc=str_locate(ID_num,pattern = "_")[,1],
                ID=str_sub(ID_num,1,loc-1))


MAT_data<-gwas_name_df%>%
  filter(ID%in%data_df$ID)%>%
  group_by(LinesName)%>%
  summarise(N=n())

names(MAT_data)<-c("Mat_Name","Image number")
library(xlsx)
# write.xlsx(MAT_data,"../../../Submit/01 The Crop Journal/Revised/Supporting information.xlsx")

#----MDS分析
#轴没有权重信息，只是保留相对位置
mds_re = cmdscale(data)
mds_re_df<-as.data.frame(mds_re)

ggplot(mds_re_df, aes(x=V1,y=V2))+
  geom_point()+
  labs(x="MDS1",y="MDS2")+
  theme_bw()

mds_re2 <- isoMDS(data,k = 6)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:6)

#散点图
ggplot(mds_re2_df, aes(x=MDS1,y=MDS2))+
  geom_point()+
  labs(x="MDS1",y="MDS2")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )



#MDS分布图
mds_re2_df_l<-mds_re2_df%>%
  rownames_to_column()%>%
  gather(.,key = MDS,value = value,MDS1:MDS3)
ggplot(data = mds_re2_df_l,aes(x=value))+
  geom_density(aes(fill=MDS),alpha=0.6)+
  labs(x="MDS axes value",fill="MDS:")+
  # scale_fill_manual(values = c("blue","yellow","red"))+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=14,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        text  = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
  )


#探索第一个维度的变化：株高
class(mds_re2_df$MDS1)
MDS1<-mds_re2_df$MDS1
names(MDS1)<-rownames(mds_re2_df)

MDS1_sort<-sort(MDS1)
length(MDS1_sort)
names(MDS1_sort)[c(1,252,503,755,1006)]

#探索第二个维度的变化：
MDS2<-mds_re2_df$MDS2
names(MDS2)<-rownames(mds_re2_df)

MDS2_sort<-sort(MDS2)
length(MDS2_sort)
names(MDS2_sort)[c(1,252,503,755,1006)]

#探索第三个维度的变化：
MDS3<-mds_re2_df$MDS3
names(MDS3)<-rownames(mds_re2_df)

MDS3_sort<-sort(MDS3)
length(MDS3_sort)
names(MDS3_sort)[c(1,252,503,755,1006)]

#探索第四个维度的变化：
MDS4<-mds_re2_df$MDS4
names(MDS4)<-rownames(mds_re2_df)

MDS4_sort<-sort(MDS4)
length(MDS4_sort)
names(MDS4_sort)[c(1,252,503,755,1006)]

#探索第五个维度的变化：
MDS5<-mds_re2_df$MDS5
names(MDS5)<-rownames(mds_re2_df)

MDS5_sort<-sort(MDS5)
length(MDS5_sort)
names(MDS5_sort)[c(1,252,503,755,1006)]

#探索第六个维度的变化：
MDS6<-mds_re2_df$MDS6
names(MDS6)<-rownames(mds_re2_df)

MDS6_sort<-sort(MDS6)
length(MDS6_sort)
names(MDS6_sort)[c(1,252,503,755,1006)]

#----MDS轴的解释率
#特征值为负数说明不是欧式空间，但是如果前几个正值能解释大部分的方差则可以考虑使用前几个轴表示
mds_re_eig<-cmdscale(data,k=3,eig = T)
eig_var<-(mds_re_eig$eig[1:5])/sum(mds_re_eig$eig)
eig_var_cumsum<-cumsum(eig_var)

MDS_df<-data.frame(MDS=str_c("MDS",1:5),
                   eig_var=eig_var,
                   Var_cumsum=eig_var_cumsum)
MDS_df$MDS<-factor(MDS_df$MDS,ordered = T,levels = str_c("MDS",1:5))
ggplot(data = MDS_df,aes(x=MDS,y=eig_var))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=eig_var+0.04,label=round(Var_cumsum,2)),size=5)+
  labs(y="Proportion",x="")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=14,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        text  = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
  )





#----pcoa分析
#https://www.bilibili.com/read/cv10516480/
pcoa<-cmdscale(data,k=3,eig = T)#计算距离矩阵的相关值，k是维度，推荐3，eig是返回特征值
library(vegan)
ordiplot(scores(pcoa)[,c(1:2)],type = "t")#简要查看结果
#查看主要排序轴的特征值
pcoa$eig

#坐标轴解释量（前两轴）
pcoa_eig<-(pcoa$eig[1:10])/sum(pcoa$eig)

#----pca分析







