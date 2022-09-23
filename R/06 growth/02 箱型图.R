#===========================================================
#02 箱型图
#===========================================================

getwd()

library(tidyverse)
library(FactoMineR)
library(stats)
library(MASS)

#----导入数据
list.files("./data/T16_PH_data/")
data_list<-read_rds("./data/T16_PH_data/GWAS_classes12_PH_data_filter.rds")

name_list<-names(data_list)
name_list_df<-as.data.frame(name_list)%>%
  dplyr::mutate(loc=str_locate(name_list,pattern = "_")[,1],
                ID=str_sub(name_list,1,loc-1),
                Time=toupper(str_sub(name_list,loc+1,-1)))

#生成一个列表，储存每个时间点对应的顺序
Time_list<-list()
for (i in 1:16) {
  wh_i<-which(name_list_df$Time==i)
  Time_list[[i]]<-wh_i
}


#将列表按照拍摄时间划分
big_list<-list()
# Time<-1

for (i in 1:16) {
  big_list[[i]]<-lapply(Time_list[[i]], function(x){data_list[[x]]})
}


big_list[[1]][[1]]

#按照时间点计算BD距离
matrix_list<-list()
# a<-1
for (a in 1:16) {
  re_mat1<-matrix(ncol = length(big_list[[a]]),nrow = length(big_list[[a]]))
  for (i in seq_len(length(big_list[[a]]))) {
    for (j in seq_len(length(big_list[[a]]))) {
      re_mat1[i,j]<-TDA::bottleneck(big_list[[a]][[i]], big_list[[a]][[j]],dimension = 0)
    }
  }
  matrix_list[[a]]<-re_mat1
}

#将距离矩阵变成长表并合并
T16_matrix_l<-as.data.frame(matrix_list[[16]])%>%
  rownames_to_column()%>%
  melt()

mat_fun<-function(mat){
  BTdf<-as.data.frame(mat)%>%
    rownames_to_column()%>%
    reshape2::melt()
  # BTdf$
  return(BTdf)
}

names(matrix_list)<-str_c("T",1:16)
df_BD<-plyr::ldply(.data = matrix_list,.fun = mat_fun)
df_BD$.id<-factor(df_BD$.id,ordered = T,levels = str_c("T",1:16))
#绘制箱型图
ggplot(data = df_BD,aes(x=.id,y=value))+
  geom_boxplot(fill="grey")+
  labs(x="Time",y="Bottleneck distance")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=14,color="black"),
        axis.text.y = element_text(size=14,color="black"),
        text  = element_text(size = 16, color = "black"),
        strip.text.x = element_text(size = 14, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 14, color = "black", face = "bold.italic")
  )





