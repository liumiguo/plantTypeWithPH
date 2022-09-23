#=====================================================================
#02 材料来源与株型的关系
#=====================================================================
getwd()

library(tidyverse)
library(MASS)

#----导入数据分析表
group<-read.csv("./data/gwas_data/gwas_lines_origin_class.csv")
data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#----MDS分析
mds_re2 <- isoMDS(data_BD,k = 4)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:4)
mds_re2_df_2<-mds_re2_df%>%
  rownames_to_column(var = "ID_T")%>%
  dplyr::mutate(loc=str_locate(ID_T,pattern = "_")[,1],
                ID=str_sub(ID_T,1,loc-1))%>%
  dplyr::select(-loc)

#----合并数据
join_data<-left_join(mds_re2_df_2,group)%>%
  drop_na()

#----绘图
names(join_data)
ggplot(join_data, aes(x=MDS1,y=MDS2))+
  geom_point(aes(color=Subpopulations),size=3,alpha=0.8)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
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


ggplot(join_data, aes(x=MDS1,y=MDS3))+
  geom_point(aes(color=Subpopulations),size=2)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
  labs(x="MDS1",y="MDS3")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

ggplot(join_data, aes(x=MDS1,y=MDS4))+
  geom_point(aes(color=Subpopulations),size=2)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
  labs(x="MDS1",y="MDS4")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )


ggplot(join_data, aes(x=MDS2,y=MDS3))+
  geom_point(aes(color=Subpopulations),size=2)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
  labs(x="MDS2",y="MDS3")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

ggplot(join_data, aes(x=MDS2,y=MDS4))+
  geom_point(aes(color=Subpopulations),size=2)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
  labs(x="MDS2",y="MDS4")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )


ggplot(join_data, aes(x=MDS3,y=MDS4))+
  geom_point(aes(color=Subpopulations),size=2)+
  scale_color_manual(values = c("red","blue","yellow","black"))+
  labs(x="MDS3",y="MDS4")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

