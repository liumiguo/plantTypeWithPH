%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%批量计算持续同调
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Compute persistent homology and save the barcode
%download javaplex and go to their matlab_examples folder
%先将工作路径选择到javaplex然后运行以下两行代码导入该软件包
load_javaplex
import edu.stanford.math.plex4.*;

%改变工作路径到BD距离所在的文件夹

%获得当前路径下所有文件夹名称
d = dir('.');
isub = [d(:).isdir]; %# returns logical vector
nameFolds = {d(isub).name}';
nameFolds(ismember(nameFolds,{'.','..'})) = [];
class(nameFolds)%查看当前类型

length(nameFolds)


%i = 100
%fullfile('D:/GWAS_plant_type/GWAS_result_GB_distances/T16',cell2mat(nameFolds(i,1)),Loc_GD.csv)
%注意修改其中的文件路径
for iii=1:length(nameFolds)
    %iii = 1
    point_gd=readmatrix(fullfile('F:/DataSet_From_Web/GWAS_plant_type/GWAS_result_GB_distances/classes12',cell2mat(nameFolds(iii,1)),'Loc_GD.csv'));
    GridW=readmatrix(fullfile('F:/DataSet_From_Web/GWAS_plant_type/GWAS_result_GB_distances/classes12',cell2mat(nameFolds(iii,1)),'connect.csv'));
    stream = api.Plex4.createExplicitSimplexStream();
    for i=1:length(point_gd)
             stream.addVertex(i,-point_gd(i,3));
         end
    for i=1:length(GridW)
             stream.addElement([GridW(i,1) GridW(i,2)],max(-point_gd(GridW(i,1),3),-point_gd(GridW(i,2),3)));%添加元素
         end
    stream.finalizeStream();
    persistence = api.Plex4.getModularSimplicialAlgorithm(1, 2);
    intervals = persistence.computeAnnotatedIntervals(stream);
    tmp=intervals.getIntervalsAtDimension(0);
    tmp_char = char(tmp);
    %save('tmp.mat','tmp_char')
    save(fullfile('F:/DataSet_From_Web/GWAS_plant_type/GWAS_result_GB_distances/classes12',cell2mat(nameFolds(iii,1)),'tmp.mat'),'tmp_char');
    %prr = length(nameFolds)-iii
    nameFolds(iii,1)
    %print(prr)
end




