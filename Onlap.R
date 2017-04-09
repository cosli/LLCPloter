#载入ggplot2包
library("ggplot2")
rm(list=ls())
##########上超点成图#################
#A[i,1]-编号   A[i,2]-层位    A[i,3]-CDP  A[i,4]-双程旅行时间  A[i,5]-断距  A[i,6]-负双程旅行时间（已校正）A[i,7]-delta t
#读入上超点数据
ONLAP<-read.csv("onlappoints.csv")
#校正双程旅行时间
ONLAP[6]<-ONLAP[4]-ONLAP[5]
#将双程旅行时间变为相反数
ONLAP[6]<-(-ONLAP[6])
#计算delta t
ONLAP[1,7]<-0
for (i in 2:length(ONLAP[,1])){
  ONLAP[i,7]<-ONLAP[i,6]-ONLAP[i-1,6]
}
#修改列名
colnames(ONLAP)<-c("编号","层位","CDP","双程旅行时间","断距","负双程旅行时间","t")
#计算各个层位顶底
sumlayer<-length(levels(ONLAP[,2]))
sep<-data.frame()
for (i in 1:sumlayer){
  sep[i,1]<-min(which(ONLAP[,2]==levels(ONLAP[,2])[i]))
  sep[i,2]<-max(which(ONLAP[,2]==levels(ONLAP[,2])[i]))
  sep[i,3]<-levels(ONLAP[,2])[i]
}
sep<-sep[order(sep[,1]),]
colnames(sep)<-c("顶","底","层位")

#绘图并另存为png文件（上超点横向迁移）
q1<-ggplot(ONLAP,aes(编号,CDP))
q2<-q1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="上超点横向迁移图")+xlab("时间")
q3<-q2+geom_vline(xintercept=sep[2:sumlayer,1],lty=2)
for (i in 1:length(levels(ONLAP[,2]))){
  q3<-q3+geom_text(x=mean(as.numeric(sep[i,1:2])),y=min(ONLAP[3]),hjust=0.5,vjust=1,label=sep[i,3],size=4)
}
q3
ggsave(file="Onlap-H.png",dpi=600)

#绘图并另存为png文件（上超点的纵向偏移）
p1<-ggplot(ONLAP,aes(编号,t))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="上超点纵向迁移图")+xlab("时间")+ylab("\u0394 t")
p3<-p2+geom_vline(xintercept=sep[2:sumlayer,1],lty=2)
for (i in 1:length(levels(ONLAP[,2]))){
  p3<-p3+geom_text(x=mean(as.numeric(sep[i,1:2])),y=min(ONLAP[7]),hjust=0.5,vjust=1,label=sep[i,3],size=4)
}
p3
ggsave(file="Onlap-V.png",dpi=600)

###仅保留部分数据
p1<-ggplot(ONLAP,aes(编号,t))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+coord_cartesian(xlim=c(18,36))+labs(title="C-D上超点纵向迁移图")+xlab("时间")+ylab("\u0394 t")
p3<-p2+geom_vline(xintercept=sep[2:sumlayer,1],lty=2)
for (i in 1:length(levels(ONLAP[,2]))){
  p3<-p3+geom_text(x=mean(as.numeric(sep[i,1:2])),y=min(ONLAP[7]),hjust=0.5,vjust=1,label=sep[i,3],size=4)
}
p3
ggsave(file="Onlap-V-xlim.png",dpi=600)