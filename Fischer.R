#载入ggplot2包
library("ggplot2")
rm(list=ls())
##########Fischer图解#################
#A[i,1]-标注   A[i,2]-旋回顶深    A[i,3]-旋回底深  A[i,4]-符号  A[i,5]-旋回厚度	A[i,6]-偏移	A[i,7]-累积偏移	A[i,8]-旋回数 A[i,9]-层位
#average为旋回厚度的平均值(小数点后4位);sumcycle为总的旋回数
#
#读入旋回数据
A<-read.csv("XuanHui.csv")
#读入层位数据
C<-read.csv("CengWei.csv")
#计算总旋回数
sumcycle<-length(A[,1])
#计算总层位数
sumlayer<-length(C[,1])
#计算偏移
A[5]<-A[3]-A[2]
average<-round(mean(A[,5]),4)
A[6]<-A[5]-average
#计算累积偏移
A[sumcycle,7]<-A[sumcycle,6]
for (i in (sumcycle-1):1){
  A[i,7]<-sum(A[sumcycle:i,6])
}
#由底至顶写入旋回次序
A[8]<-sumcycle:1
#计算每一层位最底部旋回所处数据框位置,1096.5等数字是各个层位的底界
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
colnames(A)<-c("标注","旋回顶深","旋回底深","符号","旋回厚度","偏移","累积偏移","旋回数","层位")
#将数据框Ａ写入文件
write.csv(A,file="Fischer.csv")
#计算各个层位顶底
sep<-layer
sep[1]<-sumcycle+1-sep[1]
sep[2]<-sumcycle+1-sep[2]
colnames(layer)<-c("顶","底","层位")
#绘图并另存为png文件
p1<-ggplot(A,aes(旋回数,累积偏移))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="Fischer图解")+xlab("时间")
p3<-p2+geom_vline(xintercept=sep[2:sumlayer,1],lty=2)
for (i in 1:sumlayer){
  p3<-p3+geom_text(x=mean(as.numeric(sep[i,1:2])),y=min(A[7]),hjust=0.5,vjust=1,label=sep[i,3],size=4)
}
p3
ggsave(file="Fischer.png",dpi=600)
