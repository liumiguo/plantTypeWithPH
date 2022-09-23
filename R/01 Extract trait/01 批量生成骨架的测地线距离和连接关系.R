#===============================================================================
#01 批量生成骨架的测地线距离和连接关系
#===============================================================================
getwd()

library(png)
library(igraph)
library(reshape)
library(tidyverse)

#----01 获得图片列表和输入输出路径
dir<-"D:/GWAS_plant_type/GWAS_result_image/T16"
filelist<-list.files(dir,pattern = ".png")
outdir<-"D:/GWAS_plant_type/GWAS_result_GB_distances/T16"

# dir0<-"D:/GWAS_plant_type/GWAS_result_image/T16/problem/ok/png"
# filelist0<-list.files(dir0,pattern = ".png")
filelist<-c("P0870_16.png","P1097_16.png")
#----02 分析代码
for (i in 1:length(filelist)) {
  # i<-1
  filename<-filelist[i]
  #判断是否存在文件夹
  if(!dir.exists(str_c(outdir,str_sub(filename,1,-5),sep = "/"))){
    dir.create(str_c(outdir,str_sub(filename,1,-5),sep = "/"))
  }

  #001 读取图片
  img <- readPNG(paste(dir,"/",filename,sep = ""))#图像被逆时针旋转90
  #002 旋转并提取黑色像素的位置，并记录基准点的位置
  rotate <-function(x) t(apply(x, 2, rev))
  img2<-rotate(img)
  img2_wh = as.data.frame(which(img2==0,arr.ind = T))
  colnames(img2_wh)<-c("x","y")
  #记录基准点的位置，将y轴最低的作为基准点
  img2_wh2<-img2_wh
  base<-which.min(img2_wh2$y)

  #003 构建距离矩阵  计算每个点之间的距离构建距离矩阵
  dt<-as.data.frame(as.matrix(dist(img2_wh2)))
  dt_melt_rm<-filter(melt(rownames_to_column(dt)),value!=0)#第一列是数据矩阵的行号，第二列是数据矩阵的列号

  #004 确定每个点周围的最近的两点
  #计算每个点周围最近的点和距离构造一个连通的无向网络所需的数据框
  ##选择距离每个点周围最近的两个点
  fu<-function(data){
    # data<-dt_melt_rm%>%filter(variable==100)
    wh<-which(data$value%in%sort(data$value)[1:2])
    # wh<-which.min(data$value)
    rownum<-data[wh[1:2],1]
    value<-data[wh[1:2],3]
    re<-c(rownum,value)
    names(re)<-c("rownum1","rownum2","value1","value2")
    return(re)
  }

  #variable是原来的数据矩阵的列编号，表示所有的点的编号，求距离每个点最近的两个点的编号和距离
  dt_melt_rm_fu<-plyr::ddply(.data=dt_melt_rm,.variables="variable",.fun = fu)
  dt_melt_rm_fu2<-dt_melt_rm_fu%>%
    mutate(RV1=str_c(rownum1,"_",value1),#将点的编号和距离组合成字符串
           RV2=str_c(rownum2,"_",value2)
    )%>%
    select(-rownum1,-rownum2,-value1,-value2)%>%#去除掉原有编号
    gather(.,key = RV,value = value2,RV1:RV2)%>%#合并成一列
    mutate(loc=str_locate(value2,pattern = "_")[,1],
           rownum=str_sub(value2,1,loc-1),
           value=str_sub(value2,loc+1,-1))%>%#展开成编号和距离
    select(-RV,-value2,-loc)%>%
    unique(.)#去除重复

  #005 构建网络
  #转化数据格式用于构建一个网络
  dt_melt_rm_fu2$variable<-as.numeric(dt_melt_rm_fu2$variable)
  dt_melt_rm_fu2$rownum<-as.numeric(dt_melt_rm_fu2$rownum)
  dt_melt_rm_fu2$value<-as.numeric(dt_melt_rm_fu2$value)
  names(dt_melt_rm_fu2)<-c("from","to","weight")
  #构建网络
  gra<-graph_from_data_frame(d=dt_melt_rm_fu2[,1:2],directed=F)#从数据框构建网络
  E(gra)$weight<-dt_melt_rm_fu2$weight#增加权重信息

  #转化为简单图
  gra_sim<-igraph::simplify(gra)
  #006计算最短距离
  dis_base<-distances(gra_sim,to=base)#此处以第一个点为例子计算所有的点到该点的GD值
  #构成一个包含最短距离的表格
  Loc_GD<-img2_wh2
  Loc_GD$Geodesic_distance<-dis_base[,1]
  #007 导出结果数据（坐标及对应的GD距离）
  write_csv(Loc_GD,str_c(outdir,str_sub(filename,1,-5),"Loc_GD.csv",sep = "/"))
  write_csv(dt_melt_rm_fu2,str_c(outdir,str_sub(filename,1,-5),"connect.csv",sep = "/"))
  print(str_c("处理完：",filename,";还剩：",length(filelist)-i))
}


