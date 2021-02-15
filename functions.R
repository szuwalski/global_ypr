

ypr_age<-function(specs_in,proj_yr,input_f)
{
 eq_yield<-rep(0,length(input_f))
 eq_sbpr<-rep(0,length(input_f))
for(f in 1:length(input_f))
{
 n_at_age<-matrix(ncol=nrow(specs_in),nrow=proj_yr)
 y_at_age<-matrix(ncol=nrow(specs_in),nrow=proj_yr)
 
 n_at_age[1,1]<-1
 for(z in 2:ncol(n_at_age))
   n_at_age[1,z]<-n_at_age[1,z-1]*exp(-specs_in$nat_m[z-1])
 
 for(x in 2:nrow(n_at_age))
 {
   n_at_age[x,1]<-1
  for(y in 2:(ncol(n_at_age)-1)) 
  {
    n_at_age[x,y]<-n_at_age[x-1,y-1]*exp(-(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y]))
    y_at_age[x,y]<-n_at_age[x-1,y-1]* (input_f[f]*specs_in$fish_sel[y]/(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y]))*
      (1-exp(-(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y])))
  }
   # plus group
   y<-ncol(n_at_age)
   n_at_age[x,y]<-n_at_age[x-1,y-1]*exp(-(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y])) + n_at_age[x-1,y]*exp(-(specs_in$nat_m[y]+input_f[f]*specs_in$fish_sel[y]))
   y_at_age[x,y]<-n_at_age[x-1,y-1]* (input_f[f]*specs_in$fish_sel[y]/(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y]))*(1-exp(-(specs_in$nat_m[y-1]+input_f[f]*specs_in$fish_sel[y]))) +
                  n_at_age[x-1,y]  * (input_f[f]*specs_in$fish_sel[y]/(specs_in$nat_m[y]+input_f[f]*specs_in$fish_sel[y]))  *(1-exp(-(specs_in$nat_m[y]+input_f[f]*specs_in$fish_sel[y])))
   
   
 }
 eq_yield[f]<-sum(y_at_age[proj_yr-1,]*specs_in$weight,na.rm=T)
 eq_sbpr[f]<-sum(n_at_age[proj_yr-1,]*specs_in$weight*specs_in$prob_mature,na.rm=T)
} 
 list(ypr=eq_yield,sbpr=eq_sbpr,f=input_f)
  
}




