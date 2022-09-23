#==========================================================
#02 检查结果文件中有没有确实值 Inf值 等
#==========================================================

getwd()

library(tidyverse)

#文件路径
dir<-"D:/GWAS_plant_type/GWAS_result_GB_distances/classes16"
filelist<-list.files(dir)

result<-c()
#循环检查结果
for (i in 1:length(filelist)) {
  # i<-112
  file.list_nex<-list.files(str_c(dir,filelist[i],sep = "/"))
  file1<-read.csv(str_c(dir,filelist[i],file.list_nex[1],sep = "/"))
  file2<-read.csv(str_c(dir,filelist[i],file.list_nex[2],sep = "/"))
  if(sum(is.infinite(file1$weight))>0|sum(is.infinite(file2$Geodesic_distance))>0){
    result<-c(result,filelist[i])
  }
  print(i)
  # print(str_c("还剩：",length(filelist)-i))
}

#复制文件到指定位置
result

for (ii in 1:length(result)) {
  # ii<-1
  from<-str_c("D:/GWAS_plant_type/GWAS_result_image/classes16/",result[ii],".png")
  to<-str_c("D:/GWAS_plant_type/GWAS_result_image/classes16/problem/",result[ii],".png")
  file.copy(from,to)
  print(result[ii])
}


