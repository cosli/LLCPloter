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
#载入ggplot2包
library("ggplot2")
#计算各个层位顶底
gL_sep<-c(min(which(ONLAP[,2]==c("SQgL"))),max(which(ONLAP[,2]==c("SQgL"))))
gU_sep<-c(min(which(ONLAP[,2]==c("SQgU"))),max(which(ONLAP[,2]==c("SQgU"))))
mL2_sep<-c(min(which(ONLAP[,2]==c("SQmL2"))),max(which(ONLAP[,2]==c("SQmL2"))))
mL1_sep<-c(min(which(ONLAP[,2]==c("SQmL1"))),max(which(ONLAP[,2]==c("SQmL1"))))
mU_sep<-c(min(which(ONLAP[,2]==c("SQmU"))),max(which(ONLAP[,2]==c("SQmU"))))

#绘图并另存为png文件（上超点横向迁移）
q1<-ggplot(ONLAP,aes(编号,CDP))
q2<-q1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="上超点横向迁移图")+xlab("时间")
q3<-q2+geom_vline(x=c(gL_sep[2],gU_sep[2],mL2_sep[2],mL1_sep[2]),lty=2)
q3+geom_text(x=ave(gL_sep)[1],y=min(ONLAP[3]),hjust=0.5,vjust=1,label="SQgL",size=4)+
  geom_text(x=ave(gU_sep)[1],y=min(ONLAP[3]),hjust=0.5,vjust=1,label="SQgU",size=4)+
  geom_text(x=ave(mL2_sep)[1],y=min(ONLAP[3]),hjust=0.5,vjust=1,label="SQmL2",size=4)+
  geom_text(x=ave(mL1_sep)[1],y=min(ONLAP[3]),hjust=0.5,vjust=1,label="SQmL1",size=4)+
  geom_text(x=ave(mU_sep)[1],y=min(ONLAP[3]),hjust=0.5,vjust=1,label="SQmU",size=4)
ggsave(file="Onlap-H.png",dpi=600)

#绘图并另存为png文件（上超点的纵向偏移）
p1<-ggplot(ONLAP,aes(编号,t))
p2<-p1+geom_point(aes(colour=层位))+geom_smooth(method="loess",span=0.1,se=TRUE)+labs(title="上超点纵向迁移图")+xlab("时间")+ylab("\u0394 t")
p3<-p2+geom_vline(x=c(gL_sep[2],gU_sep[2],mL2_sep[2],mL1_sep[2]),lty=2)
p3+geom_text(x=ave(gL_sep)[1],y=min(ONLAP[7]),hjust=0.5,vjust=1,label="SQgL",size=4)+
  geom_text(x=ave(gU_sep)[1],y=min(ONLAP[7]),hjust=0.5,vjust=1,label="SQgU",size=4)+
  geom_text(x=ave(mL2_sep)[1],y=min(ONLAP[7]),hjust=0.5,vjust=1,label="SQmL2",size=4)+
  geom_text(x=ave(mL1_sep)[1],y=min(ONLAP[7]),hjust=0.5,vjust=1,label="SQmL1",size=4)+
  geom_text(x=ave(mU_sep)[1],y=min(ONLAP[7]),hjust=0.5,vjust=1,label="SQmU",size=4)
ggsave(file="Onlap-V.png",dpi=600)
