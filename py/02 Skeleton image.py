# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 10:19:09 2022

提取骨架结构


@author: ymhua
"""

import os
import cv2
from plantcv import plantcv as pcv
# %matplotlib

#----路径设置
filepath = 'D:/GWAS_plant_type/GWAS_mask_image/T13/mask_dilate/ok'
# os.chdir(filepath)
os.getcwd()
# os.listdir(filepath)

#----提取所有文件名
fileNameList = os.listdir(filepath)



# #----案例
# #设计全局变量保证能够及时看到结果
class options:
    def __init__(self):
        self.image = fileNameList[160]
        self.debug = "None"#None, plot
        self.writeimg= False 
        self.result = "morphology_tutorial_results.txt"
        self.outdir = "."

# Get options
args = options()

# Set debug to the global parameter 
pcv.params.debug = args.debug

#读取图片
img, path, filename = pcv.readimage(filename=os.path.join(filepath, args.image))

#转换为二值图像
pcv.visualize.histogram(img)
binary = pcv.threshold.binary(gray_img=img, threshold=142, max_value=255, object_type='light')

#填充噪点
fillImage = pcv.fill(bin_img=binary, size=300)

#转化为骨架图像并优化
skeleton = pcv.morphology.skeletonize(mask=fillImage)
img1, seg_img, edge_objects = pcv.morphology.prune(skel_img=skeleton, size=60, mask=fillImage)
pruned_skel, seg_img, edge_objects = pcv.morphology.prune(skel_img=img1, size=10, mask=fillImage)
pcv.plot_image(pruned_skel)


# 图像取反
pruned_skel_inv = 255-pruned_skel
pcv.plot_image(pruned_skel_inv)


branch_pts_mask = pcv.morphology.find_branch_pts(skel_img=img1, mask=fillImage, label="default")
pcv.plot_image(branch_pts_mask)








#----批量导出图片
# outpath= 'D:/GWAS_plant_type/GWAS_mask_image/T13/mask_skeletonize'

# def pre_skeletion(fileName,outpath):
#     #读取图片
#     img, path, filename = pcv.readimage(filename=fileName)
    
#     #转换为二值图像
#     # pcv.visualize.histogram(img)
#     binary = pcv.threshold.binary(gray_img=img, threshold=142, max_value=255, object_type='light')
    
#     #填充噪点
#     fillImage = pcv.fill(bin_img=binary, size=200)
    
#     #转化为骨架图像并优化
#     skeleton = pcv.morphology.skeletonize(mask=fillImage)
#     img1, seg_img, edge_objects = pcv.morphology.prune(skel_img=skeleton, size=60, mask=fillImage)
#     pruned_skel, seg_img, edge_objects = pcv.morphology.prune(skel_img=img1, size=10, mask=fillImage)
#     # pcv.plot_image(seg_img)

#     #导出结果
#     cv2.imwrite(os.path.join(outpath, fileName),seg_img)


# for i in range(0,len(fileNameList)):
#     # i = 0
#     pre_skeletion(fileName=fileNameList[i],outpath=outpath)
#     print("正在转化第"+str(i+1)+"张图")



#----批量导出纯骨架图片
outpath= 'D:/GWAS_plant_type/GWAS_mask_image/T13/mask_skeletonize'

def skeletion(fileName,outpath):
    #读取图片
    img, path, filename = pcv.readimage(filename=os.path.join(filepath, fileName))
       
    #转换为二值图像
    pcv.visualize.histogram(img)
    binary = pcv.threshold.binary(gray_img=img, threshold=142, max_value=255, object_type='light')
       
    #填充噪点
    fillImage = pcv.fill(bin_img=binary, size=300)
       
    #转化为骨架图像并优化
    skeleton = pcv.morphology.skeletonize(mask=fillImage)
    img1, seg_img, edge_objects = pcv.morphology.prune(skel_img=skeleton, size=60, mask=fillImage)
    pruned_skel, seg_img, edge_objects = pcv.morphology.prune(skel_img=img1, size=10, mask=fillImage)
    # pcv.plot_image(pruned_skel)
       
       
    # 图像取反
    pruned_skel_inv = 255-pruned_skel
    
    #导出结果
    cv2.imwrite(os.path.join(outpath, fileName.split('.')[0]+'.png'),pruned_skel_inv)


for i in range(0,len(fileNameList)):
    # i = 0
    skeletion(fileName=fileNameList[i],outpath=outpath)
    print("正在转化第"+str(i+1)+"张图")