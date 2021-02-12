
#===================================================
# analysis over many stocks
#===================================================
source("functions.R")
specs<-data.frame(read.csv("age_specs.csv"))
proj_yr<-100
input_f<-seq(0.00,1.5,0.01)

unq_stock<-unique(specs$species)
big_out<-data.frame(ypr=NULL,sbpr=NULL,f=NULL,year=NULL,stock=NULL)
for(x in 1:length(unq_stock))
{
  specs_use<-dplyr::filter(specs, species == unq_stock[x])
  unq_years<-unique(specs_use$year)
  for(y in 1:length(unq_years))
  {
    specs_use2<-dplyr::filter(specs_use, year == unq_years[y])
    outs<-ypr_age(specs_in=specs_use2,proj_yr=100,input_f=input_f)
    big_out<-rbind(big_out,data.frame(ypr=outs$ypr,sbpr=outs$sbpr,f=outs$f,year=rep(unq_years[y],length(input_f)),
                                      stock=rep(unq_stock[x],length(input_f))))
  }
}
big_out$year<-as.character(big_out$year)




library(ggplot2)
.THEME    = theme_bw(base_size = 12, base_family = "") +
  theme(strip.text.x = element_text(margin= margin(1,0,1,0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white"))

p<-ggplot(big_out)+
  geom_line(aes(x=f,y=ypr,color=year))+
  facet_wrap(~stock, scales ='free') +
  .THEME

print(p)
#write.csv(big_out,"big_out.csv")
