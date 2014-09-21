rm(list=ls())
###########砂地比##############
#A[j,1]-标注   A[j,2]-旋回顶深    A[j,3]-旋回底深  A[j,4]-符号  A[j,5]-旋回厚度  A[j,6]-砂层厚度	A[j,7]-砂地比	  A[j,8]-旋回数 A[i,9]-层位
#B[i,1]-砂岩顶深   B[i,2]-砂岩底深
#A为旋回数据框，B为砂层数据框；sumseq为旋回个数，sumsha为砂层个数
#x为旋回数向量，y为砂地比向量
#
#读入旋回数据
A<-read.csv("XuanHui.csv")
#读入砂岩数据
B<-read.csv("ShaYan.csv")
#读入层位数据
C<-read.csv("CengWei.csv")
#计算旋回总数
sumseq<-length(A[,1])
#计算砂层总数
sumsha<-length(B[,1])
#计算旋回厚度
A[5]<-A[3]-A[2]
#初始化各旋回砂层厚度
A[6]<-0
#计算各旋回砂层厚度
for (i in 1:sumsha){
  for (j in 1:sumseq){
    if ( (B[i,1]>=A[j,2])&&(B[i,2]<=A[j,3]) ) A[j,6]<-A[j,6]+B[i,2]-B[i,1]
    else if ( (B[i,1]<=A[j,2])&&(B[i,2]>=A[j,3]) ) A[j,6]<-A[j,6]+A[j,3]-A[j,2]
    else if ( (B[i,1]<=A[j,2])&&(B[i,2]<=A[j,3])&&(B[i,2]>=A[j,2]) ) A[j,6]<-A[j,6]+B[i,2]-A[j,2]
    else if ( (B[i,1]>=A[j,2])&&(B[i,2]>=A[j,3])&&(B[i,1]<=A[j,3]) ) A[j,6]<-A[j,6]+A[j,3]-B[i,1]
    }}
#计算砂地比
A[7]<-A[6]/A[5]
#由底至顶写入旋回次序
A[8]<-c(sumseq:1)
#计算每一层位最底部旋回所处数据框位置,1096.5等数字是各个层位的底界
SQmU<-which.min(abs(A[,3]-C[1,2]))
SQmL1<-which.min(abs(A[,3]-C[2,2]))
SQmL2<-which.min(abs(A[,3]-C[3,2]))
SQgU<-which.min(abs(A[,3]-C[4,2]))
SQgL<-which.min(abs(A[,3]-C[5,2]))
#标识各个旋回所属层位
A[1:SQmU,9]<-c("SQmU")
A[(SQmU+1):SQmL1,9]<-c("SQmL1")
A[(SQmL1+1):SQmL2,9]<-c("SQmL2")
A[(SQmL2+1):SQgU,9]<-c("SQgU")
A[(SQgU+1):SQgL,9]<-c("SQgL")
#修正数据框列名
colnames(A)<-c("标注","顶深","底深","符号","旋回厚度","砂层厚度","砂地比","旋回数","层位")
#将数据框Ａ写入文件
write.csv(A,file="ShaDiBi.csv")
#载入ggplot2包
library("ggplot2")
#计算各个层位顶底
gL_sep<-c(1,A[SQgU+1,8])
gU_sep<-c(A[SQgU,8],A[SQmL2+1,8])
mL2_sep<-c(A[SQmL2,8],A[SQmL1+1,8])
mL1_sep<-c(A[SQmL1,8],A[SQmU+1,8])
mU_sep<-c(A[SQmU,8],A[1,8])
#绘图并另存为png文件
p1<-ggplot(A,aes(旋回数,砂地比))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="砂地比变化曲线")+xlab("时间")
p3<-p2+geom_vline(x=c(gL_sep[2],gU_sep[2],mL2_sep[2],mL1_sep[2]),lty=2)
p3+geom_text(x=ave(gL_sep)[1],y=min(A[7]),hjust=0.5,vjust=1,label="SQgL",size=4)+
  geom_text(x=ave(gU_sep)[1],y=min(A[7]),hjust=0.5,vjust=1,label="SQgU",size=4)+
  geom_text(x=ave(mL2_sep)[1],y=min(A[7]),hjust=0.5,vjust=1,label="SQmL2",size=4)+
  geom_text(x=ave(mL1_sep)[1],y=min(A[7]),hjust=0.5,vjust=1,label="SQmL1",size=4)+
  geom_text(x=ave(mU_sep)[1],y=min(A[7]),hjust=0.5,vjust=1,label="SQmU",size=4)
ggsave(file="ShaDibi.png",dpi=600)