#================================================================================
#03 多元方差分析
#================================================================================

getwd()

library(tidyverse)
library(ggpubr)
library(MASS)

#----导入数据分析表
group<-read.csv("./data/gwas_data/gwas_lines_origin_class.csv")
data_BD<-read_rds("./data/T16_PH_data/GWAS_T16_BottleneckDistance_matrix.rds")

#----MDS分析
mds_re2 <- isoMDS(data_BD,k = 5)
mds_re2_df<-as.data.frame(mds_re2$points)
names(mds_re2_df)<-str_c("MDS",1:5)
mds_re2_df_2<-mds_re2_df%>%
  rownames_to_column(var = "ID_T")%>%
  dplyr::mutate(loc=str_locate(ID_T,pattern = "_")[,1],
                ID=str_sub(ID_T,1,loc-1))%>%
  dplyr::select(-loc)

#----合并数据
join_data<-left_join(mds_re2_df_2,group)%>%
  drop_na()%>%
  as.data.frame()

#-----多元方差分析
names(join_data)
m <- manova(cbind(MDS1,MDS2,MDS3,MDS4,MDS5) ~ Subpopulations,join_data)
summary(m)
summary.aov(m)


#------箱型图
join_data_2<-join_data%>%
  dplyr::select(Subpopulations,MDS1,MDS2,MDS3,MDS4,MDS5)%>%
  gather(.,key = MDS,value = value,MDS1:MDS5)


#该函数可以分析数据框中任何一个指标对处理的显著性，并得出字母
Duncanfun<-function(data,x="value",y="Subpopulations"){
  library(agricolae)
  Formula<-as.formula(paste(x,y,sep = "~"))
  model<-aov(Formula,data=data)
  aa<-duncan.test(model,y,alpha=0.05,console=FALSE)
  Treatment<-rownames(aa$groups)
  Result<-cbind(aa$groups,Treatment)
  Result<-dplyr::arrange(Result,Treatment)
  return(Result)
}
names(join_data_2)
#该代码对数据框进行分组，并对每一个分组进行Duncan分析
Dun<-plyr::ddply(join_data_2,.variables = c("MDS"),.fun = Duncanfun)


###.fun下一个参数是指把该参数传递给.fun

# my_comparisons <- list(c("SS", "NSS"),c("TST","SS"))
# my_comparisons <- list(
#                         # c("Mixed", "NSS")#1
#                        # c("SS", "NSS")#1
#                        # c("TST","SS")#1 2 5
#                        # c("Mixed","SS")#
#                        # c("NSS","TST")# 2 3 4 5
#                        c("Mixed","TST")#2 3 4
#                        )


# symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1),
#                                  symbols = c("****", "***", "**", "*", "ns"))
ggplot(data = join_data_2,aes(x=Subpopulations,y=value))+
  geom_boxplot()+
  geom_text(data = Dun,aes(x=Treatment,y=value+800,label=groups),size=5)+
  # stat_compare_means(comparisons = my_comparisons,
  #                    method="t.test",
  #                    label.y = 750,
  #                    symnum.args=symnum.args)+
  facet_wrap(MDS~.,ncol = 5)+
  # stat_compare_means()+
  theme_bw()+
  theme(panel.grid=element_blank(),
        legend.position = "top")+
  theme(axis.text.x = element_text(size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        text  = element_text(size = 14, color = "black"),
        strip.text.x = element_text(size = 12, color = "black", face = "bold.italic"),
        strip.text.y = element_text(size = 12, color = "black", face = "bold.italic")
  )


