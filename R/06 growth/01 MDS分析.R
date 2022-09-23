#===========================================================
#01 MDS分析
#===========================================================

getwd()

library(tidyverse)
library(FactoMineR)
library(stats)
library(MASS)

#----导入数据
list.files("./data/T16_PH_data/")
data_BD<-read_rds("./data/T16_PH_data/GWAS_classes12_BottleneckDistance_matrix.rds")

#----解析编号对应的材料信息
gwas_name<-list.files("F:/DataSet_From_Web/GWAS_plant_type/GWAS")
gwas_name_df<-data.frame(gwas_name)%>%
  dplyr::mutate(loc=str_locate(gwas_name,pattern = "\\(")[,1],
                ID=str_sub(gwas_name,1,loc-1),
                LinesName=toupper(str_sub(gwas_name,loc+1,-2)))

gwas_name_df_sum<-gwas_name_df%>%
  group_by(LinesName)%>%
  summarise(N=n())

#----MDS分析
mds_re2 <- isoMDS(data_BD,k = 4)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:4)

#----合并数据
mds_re2_df2<-mds_re2_df%>%
  rownames_to_column()%>%
  dplyr::mutate(loc=str_locate(rowname,pattern = "_")[,1],
                ID=str_sub(rowname,1,loc-1),
                Time=toupper(str_sub(rowname,loc+1,-1)))%>%
  dplyr::select(-loc)


data_join<-left_join(mds_re2_df2,gwas_name_df)
data_join$Time<-as.numeric(data_join$Time)
#-----散点图
str(data_join)
p1<-ggplot(data_join, aes(x=MDS1,y=MDS2))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS1",y="MDS2",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

p2<-ggplot(data_join, aes(x=MDS1,y=MDS3))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS1",y="MDS3",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

p3<-ggplot(data_join, aes(x=MDS1,y=MDS4))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS1",y="MDS4",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )


p4<-ggplot(data_join, aes(x=MDS2,y=MDS3))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS2",y="MDS3",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

p5<-ggplot(data_join, aes(x=MDS2,y=MDS4))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS2",y="MDS4",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )


p6<-ggplot(data_join, aes(x=MDS3,y=MDS4))+
  geom_point(aes(color=LinesName,size=Time),alpha=0.6)+
  #geom_line(aes(group=LinesName))+
  #scale_color_manual(values = rainbow(16))+
  labs(x="MDS3",y="MDS4",color="Lines:",size="Time:")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,angle = 45,vjust = 1,hjust = 1 ,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )

ggpubr::ggarrange(p1,p2,p4,ncol = 3,common.legend = T)#


#----MDS轴的解释率
#特征值为负数说明不是欧式空间，但是如果前几个正值能解释大部分的方差则可以考虑使用前几个轴表示
mds_re_eig<-cmdscale(data_BD,k=3,eig = T)
eig_var<-(mds_re_eig$eig[1:3])/sum(mds_re_eig$eig)
eig_var_cumsum<-cumsum(eig_var)

MDS_df<-data.frame(MDS=str_c("MDS",1:3),
                   eig_var=eig_var,
                   Var_cumsum=eig_var_cumsum)
MDS_df$MDS<-factor(MDS_df$MDS,ordered = T,levels = str_c("MDS",1:3))
ggplot(data = MDS_df,aes(x=MDS,y=eig_var))+
  geom_bar(stat = "identity")+
  geom_text(aes(y=eig_var+0.02,label=round(Var_cumsum,2)))+
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


#----三维散点图
library(tidyverse)
library(scatterplot3d)
library(scales)
library(RColorBrewer)
names(data_join)
colormap <- colorRampPalette(rev(brewer.pal(11,'RdYlGn')))(16)#legend颜色配置
# color <- rep("green", length(data_join$LinesName))
# color <- c(color, rep("red", length(data_join$LinesName)))
num<-as.numeric(as.factor(data_join$LinesName))
scatterplot3d(data_join$MDS1,data_join$MDS2,data_join$MDS3,
              xlab="MDS1",
              ylab="MDS2",
              zlab="MDS3",
              # angle=45,
              cex.symbols = rescale(data_join$Time, c(.5, 4)),
              # col="black",
              color=colormap[num],
              pch=16,
              # highlight.3d=TRUE,
              # color=color,
              # col.axis="blue",
              col.grid="lightblue",
              lwd=5,
              angle = 120)
breaks=seq(1,16,2)
legend("topright",xjust=-2,title = "Time:",legend=breaks,pch=21,
       pt.cex=rescale(breaks, c(.5, 4)),horiz=F,bty="n",ncol=4)

# write_csv(data_join,"./data/16classesgrowth.csv")

#---探索不同维度的代表趋势
#探索第一个维度的变化：株高
class(data_join$MDS1)
MDS1<-data_join$MDS1
names(MDS1)<-data_join$rowname

MDS1_sort<-sort(MDS1)
length(MDS1_sort)
names(MDS1_sort)[c(1,48,96,144,192)]

#探索第二个维度的变化
class(data_join$MDS2)
MDS2<-data_join$MDS2
names(MDS2)<-data_join$rowname

MDS2_sort<-sort(MDS2)
length(MDS2_sort)
names(MDS2_sort)[c(1,48,96,144,192)]

#探索第三个维度的变化
class(data_join$MDS3)
MDS3<-data_join$MDS3
names(MDS3)<-data_join$rowname

MDS3_sort<-sort(MDS3)
length(MDS3_sort)
names(MDS3_sort)[c(1,48,96,144,192)]










