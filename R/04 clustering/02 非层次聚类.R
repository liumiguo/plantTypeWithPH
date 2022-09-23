#===========================================================
#02 非层次聚类
#===========================================================

getwd()

library(TDAkit)
library(tidyverse)
library(vegan)
library(ggrepel)
library(ggpubr)
library(cluster)
library(factoextra)
library(paletteer)

data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#--案例
pamx <- pam(data_BD, 2,diss=T)

pamx$silinfo$avg.width

#--寻找最优分类
#平均宽度法
# k_range<-2:30
# widths <- sapply(k_range, function(k) pam(data_BD, k=k)$silinfo$avg.width)
# widths_df<-data.frame(K=k_range,widths=widths)
#
# ggplot(data = widths_df,aes(x=K,y=widths))+
#   geom_point()+
#   geom_line(aes(group=1))+
#   geom_vline(xintercept = 16,linetype=2)+
#   scale_x_continuous(breaks = seq(2,30,2))+
#   scale_y_continuous(breaks = seq(0,0.5,0.1))+
#   labs(x="Number of clusters",y="Average width value")+
#   theme_bw()+
#   theme(panel.grid=element_blank(),
#         legend.position = "top")+
#   theme(axis.text.x = element_text(size=14,color="black"),
#         axis.text.y = element_text(size=14,color="black"),
#         text  = element_text(size = 16, color = "black"),
#         strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
#         strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
#   )
#其他方法
# fviz_nbclust(data_BD, cluster::pam, method = "wss") +
#   geom_vline(xintercept = 6, linetype = 2)+
#   labs(subtitle = "Elbow method")
#
# # Silhouette method
# fviz_nbclust(data_BD, cluster::pam, method = "silhouette")+
#   labs(subtitle = "Silhouette method")
#
# fviz_nbclust(data_BD, cluster::pam, nstart = 25,  method = "gap_stat", nboot = 50,k.max = 20)+
#   labs(subtitle = "Gap statistic method")


#--绘制最优分类数下的类别
K<-12
pam_K <- pam(data_BD, K)

#确定主要类型的中间株型的名称
ID_16<-colnames(data_BD)[pam_K$id.med]

#确定所有的类别关系
clustering<-pam_K$clustering
cluster_df<-as.data.frame(clustering)%>%
  rownames_to_column(var="ID")%>%
  mutate(Group=str_c("G",clustering))


#----绘图
mds_re2 <- isoMDS(data_BD,k = 2)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:2)
mds_re2_df_2<-mds_re2_df%>%
  rownames_to_column(var = "ID")

join_cluster<-left_join(mds_re2_df_2,cluster_df)%>%
  mutate(Group=factor(Group,ordered = T,levels = str_c("G",1:12)))

#散点图
join_cluster_group_18<-join_cluster%>%
  filter(ID%in%ID_16)


names(join_cluster)
ggplot(join_cluster, aes(x=MDS1,y=MDS2))+
  geom_point(aes(color=Group),size=3)+
  geom_point(data=join_cluster_group_18,mapping=aes(x=MDS1,y=MDS2),color="black",shape=17,size=3)+
  geom_text_repel(data=join_cluster_group_18,mapping=aes(x=MDS1,y=MDS2,label=Group),color="black",size=4)+
  scale_color_manual(values = paletteer_d("colorBlindness::PairedColor12Steps"))+
  labs(x="MDS1",y="MDS2",color="Group:")+
  # theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  guides(
    color = guide_legend(
      ncol = 6,
      byrow = TRUE)
  )+
  theme_pubr()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(text = element_text(size=16,color="black"),
        axis.text.x = element_text(size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        legend.text=element_text(size=14)
  )


#柱状图
join_cluster_sum<-join_cluster%>%
  group_by(Group)%>%
  summarise(N=n())
ggplot(data = join_cluster_sum,aes(x=Group,y=N,fill=Group))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = paletteer_d("colorBlindness::PairedColor12Steps"))+
  labs(x="Group",y="Plant number")+
  theme_pubr()+
  theme(panel.grid=element_blank(),
        legend.position = "none")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(text = element_text(size=16,color="black"),
        axis.text.x = element_text(size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        legend.text=element_text(size=14)
  )

#----选取聚类G14的
cluster_df_G14<-cluster_df%>%
  filter(Group=="G14")

cluster_df_G12<-cluster_df%>%
  filter(Group=="G12")
