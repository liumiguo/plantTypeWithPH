# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 10:18:37 2022

提取传统的形态指标

@author: ymhua
"""

import os
from plantcv import plantcv as pcv
import cv2
import pandas as pd
%matplotlib

#----路径设置
filepath = 'D:/DataSet_From_Web/GWAS_plant_type/GWAS_mask_image/T16/mask_skeletonize/png'
# os.chdir(filepath)
os.getcwd()

#----提取所有文件名
fileNameList = os.listdir(filepath)
png_NameList = [i for i in fileNameList if i.endswith('png')]


# #----案例
# #设计全局变量保证能够及时看到结果
class options:
    def __init__(self):
        self.image = 'P0381_16.png'
        self.debug = "plot"#None, plot
        self.writeimg= False 
        self.result = "morphology_tutorial_results.txt"
        self.outdir = "."

# Get options
args = options()

# Set debug to the global parameter 
pcv.params.debug = args.debug

#读取图片
img, path, filename = pcv.readimage(filename=os.path.join(filepath, args.image))
# pcv.plot_image(img)

#转换为二值图像
# pcv.visualize.histogram(img)
binary = pcv.threshold.binary(gray_img=img, threshold=142, max_value=255, object_type='light')
pruned_skel, seg_img, edge_objects = pcv.morphology.prune(skel_img=255-binary, size=50)
pcv.plot_image(pruned_skel)


#计算tip数
tip_pts_mask = pcv.morphology.find_tips(skel_img=pruned_skel, mask=None, label="default")

#计算对象形状信息
origin_path="F:/DataSet_From_Web/GWAS_plant_type/GWAS_analysis_image/T16"
mask_path="F:/DataSet_From_Web/GWAS_plant_type/GWAS_mask_image/T16/mask_dilate/analysis_image/tif2jpg"


#图像处理
#导入原始图像和mask图像用于寻找对象
orig_img,_,_ = pcv.readimage(filename=os.path.join(origin_path, args.image.split('.')[0]+'.jpg'))
mask_img,_,_ = pcv.readimage(filename=os.path.join(mask_path, args.image.split('.')[0]+'.jpg'))
mask_img_bin = pcv.threshold.binary(gray_img=mask_img, threshold=142, max_value=255, object_type='light')


#寻找对象
id_obj,obj_hierarchy=pcv.find_objects(img=mask_img, mask=mask_img_bin)
obj, masked = pcv.object_composition(img=mask_img_bin, contours=id_obj, hierarchy=obj_hierarchy)

#计算形状参数
shape_image = pcv.analyze_object(img=orig_img, obj=obj, mask=mask_img_bin, label="default")

#查看结果
tips = len(pcv.outputs.observations['default']['tips']['label'])
convex_hull_area = pcv.outputs.observations['default']['convex_hull_area']['value']
solidity = pcv.outputs.observations['default']['solidity']['value']
perimeter= pcv.outputs.observations['default']['perimeter']['value']
width = pcv.outputs.observations['default']['width']['value']
height = pcv.outputs.observations['default']['height']['value']

#创建DF
df_empty = pd.DataFrame(columns=['ID', 'tips', 'convex_hull_area', 'solidity',
                                 'perimeter','width','height']) 
new = pd.DataFrame({"ID":filename.split('.')[0],
                        "tips":tips,
                        "convex_hull_area":convex_hull_area,
                        "solidity":solidity,
                        "perimeter":perimeter,
                        "width":width,
                        "height":height},index=["0"])

df_empty.append(new)





#--------------------------------------------------------------------
#批量计算形状参数
df_empty = pd.DataFrame(columns=['ID', 'tips', 'convex_hull_area', 'solidity',
                                 'perimeter','width','height']) 



# iii = 1

for iii in range(0,len(png_NameList)):#len(png_NameList)
    #读取图片
    img, path, filename = pcv.readimage(filename=os.path.join(filepath,png_NameList[iii]))
    # pcv.plot_image(img)

    #转换为二值图像
    # pcv.visualize.histogram(img)
    binary = pcv.threshold.binary(gray_img=img, threshold=142, max_value=255, object_type='light')
    pruned_skel, seg_img, edge_objects = pcv.morphology.prune(skel_img=255-binary, size=50)
    # pcv.plot_image(pruned_skel)


    #计算tip数
    tip_pts_mask = pcv.morphology.find_tips(skel_img=pruned_skel, mask=None, label="default")

    #计算对象形状信息
    origin_path="D:/GWAS_plant_type/GWAS_analysis_image/T16"
    mask_path="D:/GWAS_plant_type/GWAS_mask_image/T16/mask_dilate/analysis_image/tif2jpg"


    #图像处理
    #导入原始图像和mask图像用于寻找对象
    orig_img,_,_ = pcv.readimage(filename=os.path.join(origin_path, filename.split('.')[0]+'.jpg'))
    mask_img,_,_ = pcv.readimage(filename=os.path.join(mask_path, filename.split('.')[0]+'.jpg'))
    mask_img_bin = pcv.threshold.binary(gray_img=mask_img, threshold=142, max_value=255, object_type='light')


    #寻找对象
    id_obj,obj_hierarchy=pcv.find_objects(img=mask_img, mask=mask_img_bin)
    obj, masked = pcv.object_composition(img=mask_img_bin, contours=id_obj, hierarchy=obj_hierarchy)

    #计算形状参数
    shape_image = pcv.analyze_object(img=orig_img, obj=obj, mask=mask_img_bin, label="default")

    #查看结果
    tips = len(pcv.outputs.observations['default']['tips']['label'])
    convex_hull_area = pcv.outputs.observations['default']['convex_hull_area']['value']
    solidity = pcv.outputs.observations['default']['solidity']['value']
    perimeter= pcv.outputs.observations['default']['perimeter']['value']
    width = pcv.outputs.observations['default']['width']['value']
    height = pcv.outputs.observations['default']['height']['value']

    #创建DF

    new = pd.DataFrame({"ID":filename.split('.')[0],
                            "tips":tips,
                            "convex_hull_area":convex_hull_area,
                            "solidity":solidity,
                            "perimeter":perimeter,
                            "width":width,
                            "height":height},index=["0"])

    df_empty = df_empty.append(new)
    print('正在处理：'+png_NameList[iii]+';还剩：'+str(len(png_NameList)-iii-1))
    
#导出数据
df_empty.to_csv("gwas_tradtion_shape.csv")
