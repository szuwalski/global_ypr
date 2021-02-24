#==make EBS pollock
library(dplyr)
library(reshape2)
load('data/Codyebswp.Rdata')
names(M)
M$arithmeanF
tmp<-read.table("data/pollock_wt.txt")
trmp<-matrix(tmp,ncol=16,byrow=TRUE)
bleh<-trmp[,-1]
rownames(bleh)<-trmp[,1]
M$avg_age_mature
murph<-unlist(c(t(bleh)))

take_sel<-M$sel_fsh
rownames(take_sel)<-seq(1964,2020)
for(x in 1:nrow(take_sel))
 take_sel[x,]<-take_sel[x,]/max(take_sel[x,])
take_sel<-take_sel[which(rownames(take_sel)==1990):nrow(take_sel),]
murph2<-unlist(c(t(take_sel)))


write.csv(cbind(rep(seq(1,15),length(murph)/15),rep(seq(1990,2020),each=15),murph,murph2),'ug.csv')


#==make EBS cod
tmp<-read.table("data/cod_wt.txt")
trmp<-matrix(tmp,ncol=22,byrow=TRUE)
bleh<-trmp[,-1]
rownames(bleh)<-trmp[,1]
nat_m<-0.3244
murph<-unlist(c(t(bleh)))
m_50<-4.88
m_slope<- -0.965
maturity<- 1/(1+exp(m_slope*(seq(1,21)-m_50)))

tmp<-read.table("data/cod_sel.txt")
trmp<-matrix(tmp,ncol=22,byrow=TRUE)
bleh<-trmp[,-1]
rownames(bleh)<-trmp[,1]
murph2<-unlist(c(t(bleh)))

write.csv(cbind(rep(seq(1,21),length(murph)/21),rep(seq(1977,2020),each=21),murph2,murph,
                rep(maturity,length(murph)/21),rep(nat_m,length(murph))),'ug.csv')

#==make sablefish
ages<-seq(1,20)
m_50<-6.6
m_slope<- -0.84
maturity_fem<- 1/(1+exp(m_slope*(ages-m_50)))

weight_at_age<-log(3.16) + 2.96*log(1-exp(-0.356*(ages+1.13)))
