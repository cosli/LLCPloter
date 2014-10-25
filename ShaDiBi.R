rm(list=ls())
###########砂地比##############
#A[j,1]-标注   A[j,2]-旋回顶深    A[j,3]-旋回底深  A[j,4]-符号  A[j,5]-旋回厚度  A[j,6]-砂层厚度	A[j,7]-负砂地比	  A[j,8]-旋回数 A[i,9]-层位
#B[i,1]-砂岩顶深   B[i,2]-砂岩底深
#A为旋回数据框，B为砂层数据框；sumcycle为旋回个数，sumsha为砂层个数
#
#读入旋回数据
A<-read.csv("XuanHui.csv")
#读入砂岩数据
B<-read.csv("ShaYan.csv")
#读入层位数据
C<-read.csv("CengWei.csv")
#计算总旋回数
sumcycle<-length(A[,1])
#计算总层位数
sumlayer<-length(C[,1])
#计算砂层总数
sumsha<-length(B[,1])
#计算旋回厚度
A[5]<-A[3]-A[2]
#初始化各旋回砂层厚度
A[6]<-0
#计算各旋回砂层厚度
for (i in 1:sumsha){
  for (j in 1:sumcycle){
    if ( (B[i,1]>=A[j,2])&&(B[i,2]<=A[j,3]) ) A[j,6]<-A[j,6]+B[i,2]-B[i,1]
    else if ( (B[i,1]<=A[j,2])&&(B[i,2]>=A[j,3]) ) A[j,6]<-A[j,6]+A[j,3]-A[j,2]
    else if ( (B[i,1]<=A[j,2])&&(B[i,2]<=A[j,3])&&(B[i,2]>=A[j,2]) ) A[j,6]<-A[j,6]+B[i,2]-A[j,2]
    else if ( (B[i,1]>=A[j,2])&&(B[i,2]>=A[j,3])&&(B[i,1]<=A[j,3]) ) A[j,6]<-A[j,6]+A[j,3]-B[i,1]
    }}
#计算负砂地比
A[7]<--A[6]/A[5]
#由底至顶写入旋回次序
A[8]<-c(sumcycle:1)
layer<-data.frame()
for (i in 1:sumlayer){
  layer[i,1]<-which.min(abs(A[,2]-C[i,1]))
  layer[i,2]<-which.min(abs(A[,3]-C[i,2]))
  layer[i,3]<-as.character(C[i,3])
}
colnames(layer)<-c("顶","底","层位")
#标识各个旋回所属层位
for (i in 1:sumlayer){
  A[layer[i,1]:layer[i,2],9]<-as.character(layer[i,3])
}
#修正数据框列名
colnames(A)<-c("标注","顶深","底深","符号","旋回厚度","砂层厚度","砂地比","旋回数","层位")
#将数据框Ａ写入文件
write.csv(A,file="ShaDiBi.csv")
#载入ggplot2包
library("ggplot2")
#计算各个层位顶底
sep<-layer
sep[1]<-sumcycle+1-sep[1]
sep[2]<-sumcycle+1-sep[2]
colnames(layer)<-c("顶","底","层位")
#绘图并另存为png文件
p1<-ggplot(A,aes(旋回数,砂地比))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="砂地比变化曲线")+xlab("时间")
p3<-p2+geom_vline(x=sep[2:sumlayer,1],lty=2)
for (i in 1:sumlayer){
  p3<-p3+geom_text(x=mean(as.numeric(sep[i,1:2])),y=min(A[7]),hjust=0.5,vjust=1,label=sep[i,3],size=4)
}
p3
ggsave(file="ShaDibi2.png",dpi=600)