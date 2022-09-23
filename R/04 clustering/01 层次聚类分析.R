#==========================================================
#01 层次聚类分析
#==========================================================

getwd()

library(TDAkit)
library(tidyverse)
library(vegan)
library(ggtree)
library(tidytree)
library(treeio)
library(paletteer)
library(ggpubr)

data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#各种聚类方法
data_BD_dist<-as.dist(data_BD)
single<-stats::hclust(data_BD_dist,method = "single")
plot(single)

complete<-stats::hclust(data_BD_dist,method = "complete")
plot(complete)

ave<-stats::hclust(data_BD_dist,method = "average")
plot(ave)

cent<-stats::hclust(data_BD_dist,method = "centroid")
plot(cent)

ward.D2<-stats::hclust(data_BD_dist,method = "ward.D2")
plot(ward.D2)

ward.D<-stats::hclust(data_BD_dist,method = "ward.D")
plot(ward.D)

#比较聚类方法间的同表型相关系数
single_coph<-cophenetic(single)
cor(data_BD_dist,single_coph)

complete_coph<-cophenetic(complete)
cor(data_BD_dist,complete_coph)

ave_coph<-cophenetic(ave)
cor(data_BD_dist,ave_coph)#最好

cent_coph<-cophenetic(cent)
cor(data_BD_dist,cent_coph)

ward.D2_coph<-cophenetic(ward.D2)
cor(data_BD_dist,ward.D2_coph)

ward.D_coph<-cophenetic(ward.D)
cor(data_BD_dist,ward.D_coph)


#---选择average确定最佳分类
average<-stats::hclust(data_BD_dist,method = "complete")

plot(x=average$height,y=1006:2)

Height_N<-data.frame(Height=average$height,N=1006:2)%>%
  arrange(N)

data1=Height_N[1:20,]
ggplot(data = data1,aes(x=Height,y=N))+
  geom_point()+
  geom_line(aes(group=1))+
  geom_text(aes(x=Height+35,label=N))+
  geom_hline(yintercept = 6,linetype=2)+
  scale_y_continuous(breaks = seq(2,nrow(data1),by=2))+
  labs(x="Node height",y="Number of clusters")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
  theme(text = element_text(size=16,color="black"),
        axis.text.x = element_text(size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic")
  )




#----K=6

i=6#10
bb<-cutree(average,i)
unique(bb)
length(names(bb))
# aa<-hclust_re$merge
# clustre<-bb#KMDOI18$clustering
clustDF<-as.data.frame(bb)%>%rownames_to_column()%>%rename(group=bb)%>%arrange(group)
unique(clustDF$group)
label<-clustDF$rowname
group<-clustDF$group
unique(group)
cls<-split(label,group)
names(cls)

hclust_re_phy<-as.phylo(average)
hclust_re_phy_gro<-groupOTU(hclust_re_phy,cls)
# hclust_re_phy_gro_df<-as_tibble(hclust_re_phy_gro)
# unique(hclust_re_phy_gro_df$group)
# hclust_re_phy_gro_df_rm<-hclust_re_phy_gro_df%>%filter(group!=0)
# hclust_re_phy_gro_df_rm$group<-factor(hclust_re_phy_gro_df_rm$group,ordered = T,levels = 1:i)
# unique(hclust_re_phy_gro_df_rm$group)
hclust_re_phy_gro_td<-as.treedata(hclust_re_phy_gro)
# hclust_re_phy_gro_td<-tidytree::as.treedata(hclust_re_phy_gro)
# ggtree(hclust_re_phy_gro_td,layout='circular')##一个分为4类,branch.length="none"


ggtree(hclust_re_phy_gro_td,aes(color=factor(group,ordered = T,levels = 1:i)),layout='circular')+#aes(color=factor(group,ordered = T,levels = 1:i)),
  scale_color_manual(values = paletteer_d("ggthemes::Classic_Green_Orange_6"))+
  labs(color="Group:")+
  theme(legend.text=element_text(size=14))



#----映射到二位散点图中
library(MASS)
mds_re2 <- isoMDS(data_BD,k = 2)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:2)
mds_re2_df_2<-mds_re2_df%>%
  rownames_to_column(var = "rowname")

mds_re2_df_2$rowname[which.min(mds_re2_df_2$MDS1)]
mds_re2_df_2$rowname[which.max(mds_re2_df_2$MDS1)]



str(mds_re2_df_2)
str(clustDF)
join_cluster<-left_join(mds_re2_df_2,clustDF)%>%
  mutate(group=factor(group,ordered = T,levels = 1:6))

#柱状图
join_cluster_sum<-join_cluster%>%
  group_by(group)%>%
  summarise(N=n())
ggplot(data = join_cluster_sum,aes(x=group,y=N,fill=group))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = paletteer_d("ggthemes::Classic_Green_Orange_6"))+
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


#散点图
names(join_cluster)
ggplot(join_cluster, aes(x=MDS1,y=MDS2))+
  geom_point(aes(color=group),size=3)+
  scale_color_manual(values = paletteer_d("ggthemes::Classic_Green_Orange_6"))+
  labs(x="MDS1",y="MDS2",color="Group:")+
  # theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  guides(
    color = guide_legend(
      ncol = 8,
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














