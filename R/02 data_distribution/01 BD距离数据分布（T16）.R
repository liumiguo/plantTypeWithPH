#=====================================================
#BD距离数据分布
#=====================================================

getwd()

library(tidyverse)
library(paletteer)

list.files("./data/T16_PH_data/")
BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#---cluster analysis
data_BD_dist<-as.dist(BD)
complete<-stats::hclust(data_BD_dist,method = "complete")
i=6#10
bb<-cutree(complete,i)
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

#---
G1<-BD[cls[[1]],cls[[1]]]
G1_l<-G1%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0375_16:P1956_16)
G1_l$Group<-1

G2<-BD[cls[[2]],cls[[2]]]
G2_l<-G2%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0376_16:P1961_16)
G2_l$Group<-2

G3<-BD[cls[[3]],cls[[3]]]
G3_l<-G3%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0379_16:P1945_16)
G3_l$Group<-3


G4<-BD[cls[[4]],cls[[4]]]
G4_l<-G4%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0382_16:P1962_16)
G4_l$Group<-4


G5<-BD[cls[[5]],cls[[5]]]
G5_l<-G5%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0386_16:P1410_16)
G5_l$Group<-5

G6<-BD[cls[[6]],cls[[6]]]
G6_l<-G6%>%
  as.data.frame()%>%
  rownames_to_column()%>%
  gather(.,key = ID,value = BD,P0390_16:P1409_16)
G6_l$Group<-6


Result<-Reduce(function(x,y){rbind(x,y)},list(G1_l,G2_l,G3_l,G4_l,G5_l,G6_l))%>%
  mutate(Group=factor(Group,ordered = ,levels = 1:6))

ggplot(data = Result,aes(x=BD))+
  # geom_histogram(aes(fill=Group),bins=50)+
  geom_density(aes(fill=Group),alpha=0.8)+
  scale_fill_manual(values = paletteer_d("ggsci::uniform_startrek"))+
  # facet_grid(Group~.)+
  labs(x="Bottleneck distance")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=14,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        text  = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
  )

ggplot(data = Result,aes(x=Group,y=BD))+
  # geom_histogram(aes(fill=Group),bins=50)+
  geom_violin(aes(fill=Group))+
  scale_fill_manual(values = paletteer_d("ggsci::uniform_startrek"))+
  # facet_grid(Group~.)+
  labs(x="Group",y="Bottleneck distance",fill="Group:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        text  = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 14, color = "black"),
        strip.text.y = element_text(size = 14, color = "black")
  )+
  guides(
    fill = guide_legend(
      ncol = 6,
      byrow = TRUE)
  )



#----总和
# BD_l<-BD%>%
#   as.data.frame()%>%
#   rownames_to_column()%>%
#   gather(.,key = ID,value = BD,P0375_16:P1962_16)
#
#
# ggplot(data = BD_l,aes(x=BD))+
#   geom_histogram(bins=50)+
#   # geom_density()+
#   labs(x="Bottleneck distance")+
#   theme_bw()+
#   theme(panel.grid=element_blank(),
#                   legend.position = "top")+
#   theme(axis.text.x = element_text(size=14,angle = 45,vjust = 1,hjust = 1 ,color="black"),
#         axis.text.y = element_text(size=14,color="black"),
#         text  = element_text(size = 16, color = "black"),
#         strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
#         strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
#   )
