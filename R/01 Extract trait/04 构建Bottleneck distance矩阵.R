#===================================================================
#04 构建Bottleneck distance矩阵
#===================================================================

getwd()

library(tidyverse)
library(stats)
library(TDA)
library(MASS)

#导入数据
list.files("./data/T16_PH_data/")
datalist<-read_rds("./data/T16_PH_data/GWAS_T16_PH_data_filter.rds")

datalist[[613]]<-NULL#将第613个去掉，因为与612一致
# datalist[[613]]
#构建两两Bottleneck distance矩阵
re_mat<-matrix(ncol = length(datalist),nrow = length(datalist))

for (i in seq_len(length(datalist))) {
  for (j in seq_len(length(datalist))) {
    re_mat[i,j]<-TDA::bottleneck(datalist[[i]], datalist[[j]],dimension = 0)
  }
}

colnames(re_mat)<-names(datalist)
row.names(re_mat)<-names(datalist)
# write_rds(re_mat,"./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")




#计算MDS
# datalist[[612]]
# datalist[[613]]
# names(datalist)[c(612,613)]
# re_mat[612,613]
mds_x = cmdscale(re_mat)
summary(mds_x)
mds_x = data.frame(mds_x)
xy = cbind(mds_x)

aa<-isoMDS(re_mat)


ggplot(xy, aes(x=X1,y=X2))+geom_point()
