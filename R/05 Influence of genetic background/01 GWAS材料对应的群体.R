#===============================================================================
#01 GWAS材料对应的群体
#===============================================================================

getwd()

library(tidyverse)

#----导入数据分析表
group<-read.csv("./data/gwas_data/杨小红群体划分关系.csv")

#----解析编号对应的材料信息
gwas_name<-list.files("D:/GWAS_plant_type/GWAS")
gwas_name_df<-data.frame(gwas_name)%>%
  dplyr::mutate(loc=str_locate(gwas_name,pattern = "\\(")[,1],
                ID=str_sub(gwas_name,1,loc-1),
                LinesName=toupper(str_sub(gwas_name,loc+1,-2)))

gwas_name_df_sum<-gwas_name_df%>%
  group_by(LinesName)%>%
  summarise(N=n())

#-----合并
group$ID<-NULL
names(gwas_name_df)[4]<-"Taxa"

join_data<-left_join(gwas_name_df,group)%>%
  drop_na()

#----导出结果
# write_csv(join_data,"./data/gwas_data/gwas_lines_origin_class.csv")
