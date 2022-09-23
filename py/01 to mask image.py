# -*- coding: utf-8 -*-
"""
Created on Thu Feb 24 09:31:25 2022

将玉米株型图片转换成mask图片
导出原始mask和对应的膨胀后的图片
@author: ymhua
"""

from plantcv import plantcv as pcv
import matplotlib
import os
import cv2

dirpath = 'D:/GWAS_plant_type/GWAS_analysis_image/T13'
outpath = 'D:/GWAS_plant_type/GWAS_mask_image/T13' 
# os.chdir("D:/GWAS玉米株型/GWAS/P0508(P138)")

filelist = os.listdir(dirpath)

#不需要手动创建mask和mask_dilate文件夹
for i in range(0,len(filelist)):
    ext_mask(filepath=os.path.join(dirpath, filelist[i]),outpath=outpath)
    print("处理好："+filelist[i])








def ext_mask(filepath,outpath):
    #------读取图像
    # filepath = 'D:/GWAS_plant_type/GWAS_analysis_image/T16/P0911_16.jpg'
    img, path, filename = pcv.readimage(filename=filepath)


    #----裁剪图片:获得上半部分
    img_crop1 = pcv.crop(img, 0, 0, 1165, 1100)

    #----提取mask
    # 将RGB图像转化成HSV图像去提取饱和度轮廓
    # Convert RGB to HSV and extract the saturation channel
    # 饱和度高颜色深而艳
    s = pcv.rgb2gray_hsv(rgb_img=img_crop1, channel='v')

    # 对饱和度图像进行二值化（分离植物和背景）
    # 植物鲜艳所以能够和背景区分开
    s_thresh = pcv.threshold.binary(gray_img=s, threshold=85, max_value=255, object_type='light')

    # Median Blur to clean noise 
    # 目的去杂
    s_mblur = pcv.median_blur(gray_img=s_thresh, ksize=5)


    #原图转LAD提取蓝黄色通道并进行二值化（植物更高，所以二值化更容易白）
    b = pcv.rgb2gray_lab(rgb_img=img_crop1, channel='b')

    # 对蓝通道图像进行阈值化
    # pcv.visualize.histogram(b)
    b_thresh = pcv.threshold.binary(gray_img=b, threshold=142, max_value=255, object_type='light')

    # Join the threshold saturation and blue-yellow images with a logical or operation 
    #饱和二值化后在中位模糊后与蓝黄二值化进行或运算（借用这个色彩空间补充上一个色彩空间的信息）
    bs = pcv.logical_or(bin_img1=s_mblur, bin_img2=b_thresh)


    # Fill small objects (reduce image noise) 
    # 填充小的
    ab_fill = pcv.fill(bin_img=bs, size=200)

    # Closing filters out dark noise from an image.
    #闭合处理
    closed_ab = pcv.closing(gray_img=ab_fill)

    # Apply mask (for VIS images, mask_color=white)
    # 对mask后的少背景图进行再一次mask
    masked2 = pcv.apply_mask(img=img_crop1, mask=closed_ab, mask_color='white')

    # Identify objects
    #寻找轮廓
    id_objects, obj_hierarchy = pcv.find_objects(img=masked2, mask=ab_fill)

    # Define the region of interest (ROI) 
    #定义感兴趣的区域
    roi1, roi_hierarchy= pcv.roi.rectangle(img=masked2, x=200, y=900, h=175, w=600)

    # Decide which objects to keep
    # 决定保留什么对象
    roi_objects, hierarchy3, kept_mask, obj_area = pcv.roi_objects(img=img_crop1, roi_contour=roi1, 
                                                                   roi_hierarchy=roi_hierarchy, 
                                                                   object_contour=id_objects, 
                                                                   obj_hierarchy=obj_hierarchy,
                                                                   roi_type='partial')

    kept_mask_dilate=pcv.dilate(kept_mask, 8, 1)

    #----导出图像
    isExists=os.path.exists(os.path.join(outpath, "mask"))
    if not isExists:
        os.makedirs(os.path.join(outpath, "mask"))
    
    isExists2=os.path.exists(os.path.join(outpath, "mask_dilate"))
    if not isExists2:
        os.makedirs(os.path.join(outpath, "mask_dilate"))
    
    cv2.imwrite(os.path.join(outpath, "mask" ,filename),kept_mask)
    cv2.imwrite(os.path.join(outpath, "mask_dilate" ,filename),kept_mask_dilate)


