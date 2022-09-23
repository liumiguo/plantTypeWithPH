#==================================================================
#03 创建条形码矩阵
#===================================================================

getwd()

library(tidyverse)
library(TDAstats)
library(readxl)
library(R.matlab)
library(TDA)

#设置路径并获得文件夹列表
dir<-"F:/GWAS_plant_type/GWAS_result_GB_distances/T16"
listname<-list.files(dir)
# data<-read_xlsx("demo_PH_data.xlsx")%>%
#   as.matrix()



#将所有的matlab结果导入R并合并成一个大列表
listdata<-list()
for (i in 1:length(listname)) {
  # i<-3
  mat<-readMat(str_c(dir,listname[i],"tmp.mat",sep = "/"))
  tmp.char<-mat$tmp.char[1,1]
  # class(tmp.char)
  str_extract_all(tmp.char,pattern="^\\[")
  tmp.char1<-str_replace_all(tmp.char,"\\[","")
  tmp.char2<-str_replace_all(tmp.char1,"\\)","")

  tmp.char2_vec<-unlist(strsplit(tmp.char2, ','))
  tmp.char2_vec[length(tmp.char2_vec)]<-0
  tmp.char_mat<-matrix(as.numeric(tmp.char2_vec),
                       ncol = 2,
                       byrow=T)
  tmp.char_mat_df<-as.data.frame(tmp.char_mat)
  tmp.char_mat_df$dimension<-0
  tmp.char_mat_df2<-tmp.char_mat_df[,c(3,1,2)]
  tmp.char_mat2<-as.matrix(tmp.char_mat_df2)
  colnames(tmp.char_mat2)<-c("dimension","Birth","Death")
  listdata[[listname[i]]]<-tmp.char_mat2
  print(str_c("正在处理：",listname[i],"还剩：",length(listname)-i))
}


length(listdata)
#导出数据
# write_rds(listdata,"./data/T16_PH_data/GWAS_T16_PH_data.rds")


#计算
# class(listdata[[listname[1]]])
#
# TDA::bottleneck(listdata$P0376_16, listdata$P0376_16,dimension = 0)

#计算维度
dim_list<-plyr::ldply(listdata,.fun = dim)

#过滤掉小点
data<-listdata$P0382_16
data_df<-as.data.frame(data)%>%
  mutate(Diff=abs(Birth-Death))%>%
  filter(Diff>50)%>%
  dplyr::select("dimension","Birth","Death")
data_mat<-as.matrix(data_df)

filterPH<-function(data){
  data_df<-as.data.frame(data)%>%
    mutate(Diff=abs(Birth-Death))%>%
    filter(Diff>50)%>%
    dplyr::select("dimension","Birth","Death")
  data_mat<-as.matrix(data_df)
  return(data_mat)
}

#批量过滤
list_filter<-plyr::llply(.data = listdata,.fun = filterPH)
#检查结果
dim_list_filter<-plyr::ldply(list_filter,.fun = dim)%>%
  filter(V1>15)

#导出数据(过滤后的)
# write_rds(list_filter,"./data/T16_PH_data/GWAS_T16_PH_data_filter.rds")


#绘图
feature.matrix<-list_filter$P0657_16
x.min <- 0
x.max <- max(feature.matrix[, 2])
y.min <- 1
y.max <- nrow(feature.matrix)
feature.df <- as.data.frame(feature.matrix)
feature.df$vertical.pos <- y.min:y.max
str(feature.df)
ggplot(data = feature.df) +
  geom_segment(aes(x = Birth, y = vertical.pos, xend = Death, yend = vertical.pos))+
  scale_y_continuous(breaks=feature.df$vertical.pos)+
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
