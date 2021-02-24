
#===================================================
# analysis over many stocks
#===================================================
source("functions.R")
specs<-data.frame(read.csv("AFSC_ypr.csv"))
proj_yr<-100
input_f<-seq(0.00,1.5,0.01)

unq_stock<-unique(specs$species)
big_out<-data.frame(ypr=NULL,sbpr=NULL,f=NULL,year=NULL,stock=NULL)
big_out_sum<-data.frame(max_ypr=NULL,fmax=NULL,B40=NULL,F40=NULL,year=NULL,stock=NULL)
for(x in 1:length(unq_stock))
{
  specs_use<-dplyr::filter(specs, species == unq_stock[x])
  unq_years<-unique(specs_use$year)
  for(y in 1:length(unq_years))
  {
    #==ypr and sbpr curves
    specs_use2<-dplyr::filter(specs_use, year == unq_years[y])
    outs<-ypr_age(specs_in=specs_use2,proj_yr=100,input_f=input_f)
    big_out<-rbind(big_out,data.frame(ypr=outs$ypr,sbpr=outs$sbpr,f=outs$f,year=rep(unq_years[y],length(input_f)),
                                      stock=rep(unq_stock[x],length(input_f))))
    #==reference points
    #=find f_max and max_ypr
    max_ypr<-max(outs$ypr)
    f_max<-outs$f[which(outs$ypr==max(outs$ypr))]

    #=find B40 and F40
    B_40<-outs$sbpr[1]*.40
    F_40<-outs$f[which(abs(outs$sbpr-B_40)==min(abs(outs$sbpr-B_40)))]
    
    big_out_sum<-rbind(big_out_sum,data.frame(max_ypr=max_ypr,f_max=f_max,B40=B_40,F40=F_40,year=unq_years[y],
                                              stock=unq_stock[x]))
  }
}
big_out$year<-as.character(big_out$year)
big_out_sum$year<-as.character(big_out_sum$year)


#================================
# Plots
#================================
library(ggplot2)
.THEME    = theme_bw(base_size = 12, base_family = "") +
  theme(strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white"))

#==YPR
p<-ggplot(big_out)+
  geom_line(aes(x=f,y=ypr,color=year))+
  geom_point(data=big_out_sum,aes(x=f_max,y=max_ypr,color=year))+
  facet_wrap(~stock, scales ='free') +
  .THEME
png(paste("Alaska/plots/YPR.png",sep=""),height=6,width=8,res=500,units='in')
print(p)
dev.off()

#==SBPR
p<-ggplot(big_out)+
  geom_line(aes(x=f,y=sbpr,color=year))+
  geom_point(data=big_out_sum,aes(x=F40,y=B40,color=year))+
  facet_wrap(~stock, scales ='free') +
  .THEME
png(paste("Alaska/plots/SBPR.png",sep=""),height=6,width=8,res=500,units='in')
print(p)
dev.off()
#==dots inthese figures are the reference points


