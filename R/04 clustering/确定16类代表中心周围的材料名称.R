#================================================
#确定16类代表中心周围的材料名称
#================================================

getwd()

library(TDAkit)
library(tidyverse)
library(vegan)
library(ggrepel)
library(ggpubr)
library(cluster)

data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")



K<-12
pam_K <- pam(data_BD, K)

#确定主要类型的中间株型的名称
ID_16<-colnames(data_BD)[pam_K$id.med]

#确定分组
clustering<-pam_K$clustering
cluster_df<-as.data.frame(clustering)%>%
  rownames_to_column(var="ID")%>%
  mutate(Group=str_c("G",clustering),
         Group=factor(Group,ordered = T,levels = str_c("G",1:12)))%>%
  arrange(Group)

label<-cluster_df$ID
group<-cluster_df$Group
cls<-split(label,group)



#--G14
G<-12
df<-data.frame(BD=data_BD[,ID_16[G]])%>%
  rownames_to_column("ID")%>%
  filter(ID%in%cls[[G]])%>%
  arrange(BD)



