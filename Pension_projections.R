install.packages("patchwork")
library(gridExtra)
library(patchwork)
library(ggplot2)
#-----------------------------------control------------------------------------
#step3: 
#dth ass:improve CPM; valuation:random binomial(improve CPM)

exp_ran=0# mortality experience: 1 for random mort exp, 0 for deterministic mort exp 
ass_mort=1 # assumed mortality: 1 for improved table, 0 for base CPM table
scale=100 # increase # of member(1 or 100)


#--------------------------------base mort table--------------------------------
basetable=read.csv("CPM.csv",header=T)#age,qx

#-------------------------------impr mort table---------------------------------
imptable=read.csv("CPM_B_M.csv",header=T,check.names=FALSE)
imptable_adv=1-imptable
imptable_adv[,1]=imptable[,1]
mort_table=matrix(0,nrow=98,ncol=149)
colnames(mort_table)=c('age',seq(2021,2168,1))

for (i in 1 : 98){
  mort_table[i,1]=17+i

for (j in 2:149){
  if(j<=11){
  mort_table[i,j]=basetable[i,2]*prod(imptable_adv[i,2:(6+j)])}
  else{mort_table[i,j]=mort_table[i,j-1]*imptable_adv[i,17]}
}

}

#----------------------------------plan member---------------------------------
plan=read.csv("pension plan_mortv2.csv",header=T)

#----------------------------------fund return_fixed---------------------------------

#ret_sim=read.csv("ret.csv",header = T)

#-----------------------------------simulation---------------------------------
Ct_dist=matrix(0,nrow=110,ncol=10000)
Ft_dist=matrix(0,nrow=110,ncol=10000)
FR_dist=matrix(0,nrow=110,ncol=10000)
NC_dist=matrix(0,nrow=110,ncol=10000)

for (z in 1:10000){
#----------------------------------fund return---------------------------------
# set.seed(717)
ret_sim=runif(110,0.03,0.09)
#ret_sim=matrix(runif(110000,0.03,0.09),110,1000)
#write.csv(ret,file="ret.csv")
ret=ret_sim#[,1]#0.06#

#-------------------------------pension plan projection------------------------
AL=matrix(0,nrow=110,ncol=max(plan[,1])) #AL,NC,Pen for each inf
NC=matrix(0,nrow=110,ncol=max(plan[,1]))
Pen=matrix(0,nrow=110,ncol=max(plan[,1]))
inf=matrix(0,nrow=110,ncol=max(plan[,1]))
# inf_ass=matrix(0,nrow=110,ncol=max(plan[,1]))
k=0.2#0

for (j in 1 : 6){

#-----------------------------single member info @entry-------------------------
entry_age=plan[j,2]
No=plan[j,4]# Number of employees
sy=plan[j,3]#service year
employed=plan[j,5] #indicator, 1 for active membership, 0 for retired or expired
entry_yr=plan[j,7]
NPA=60
benefit=300# per yr pension for each yr of service
int=0.06 # discount rate

#---------------------------AL,NC for each member inf-------------------------
fact=matrix(0,nrow=110,ncol=9)
colnames(fact)=c('t','age','px','qx','lx','disc','Dx','ben','sy')                           
for (i in 1 : 110){
  fact[i,1]=i-1
  fact[i,2]=entry_age+i-1
  
  if(fact[i,2]>115){
    fact[i,4]==1 
  }else{if(ass_mort==1){
    fact[i,4]=mort_table[mort_table[,1]==fact[i,2],fact[i,1]+2+entry_yr]
    }else{
      fact[i,4]=basetable[basetable[,1]==fact[i,2],2]}
  }
  fact[i,3]=1-fact[i,4]
  
  if(i==1){
    fact[i,5]=1 
  }else{fact[i,5]=fact[i-1,5]*fact[i-1,3]}
  
  if(i==1){
    fact[i,6]=1 
  }else{fact[i,6]=fact[i-1,6]/(1+int)}
 
   fact[i,7]=fact[i,6]*fact[i,5]
  
  if(fact[i,2]>=NPA){
    fact[i,8]=benefit 
  }else{fact[i,8]=0}
  
  if(employed==0){
    fact[i,9]=sy 
  }else{ if(fact[i,2]>NPA){
    fact[i,9]=fact[i-1,9] 
  }else{fact[i,9]=sy+i-1}
  }
    
}  
  


for (i in 1 : 110){
  if(i<=entry_yr){
    AL[i,j]=0 
  }else{
  AL[i,j]=crossprod(fact[(i-entry_yr):110,7],fact[(i-entry_yr):110,8])*fact[(i-entry_yr),9]/fact[(i-entry_yr),6]/fact[(i-entry_yr),5]
  }
  if(i>115-entry_age+1+entry_yr){
  AL[i,j]=0  
  }
  }


for (i in 1 : 110){
  if(i<=entry_yr){
    NC[i,j]=0 
  }else{
  if(fact[(i-entry_yr),2]>=NPA){
    NC[i,j]=0
  }else{
    NC[i,j]=AL[i+1,j]*fact[i+1-entry_yr,6]/fact[(i-entry_yr),6]*fact[i+1-entry_yr,5]/fact[(i-entry_yr),5]-AL[i,j]
}}
}
#---------------------------Actual experience(random improve CPM)--------------------
fact_val=matrix(0,nrow=110,ncol=5)
colnames(fact_val)=c('t','age','px','qx','lx') 

for (i in 1:110){
fact_val[i,1]=i-1
fact_val[i,2]=entry_age+i-1


if(fact_val[i,2]>115){
  fact_val[i,4]==1 
}else{if(ass_mort==1){
  fact_val[i,4]=mort_table[mort_table[,1]==fact_val[i,2],fact_val[i,1]+2+entry_yr]
  }else{
    fact_val[i,4]=basetable[basetable[,1]==fact_val[i,2],2]}
}

fact_val[i,3]=1-fact_val[i,4]

if(i==1){
  fact_val[i,5]=No*scale 
}else{
  if(exp_ran==1){
  fact_val[i,5]=rbinom(1,fact_val[i-1,5],fact_val[i-1,3])
    
  }else{
  
  fact_val[i,5]=fact_val[i-1,5]*fact_val[i-1,3]}
  }


}
 for (i in 1:110){
    if(i-1<entry_yr){
    Pen[i,j]=0
    inf[i,j]=0
    
    
    }else{
    inf[i,j]=fact_val[(i-entry_yr),5]
    Pen[i,j]=fact[(i-entry_yr),8]*fact[(i-entry_yr),9]
    }

 }
#-----------------------------inf_ass for compare------------------------------
#        for (i in 1 : 110){
#         inf_ass[i,j]=fact[i,5]*No*scale
# }


}

#----------------------------Act experience(same w/ asspt)----------------------
# 
#    for (i in 1 : 110){
#      inf[i,j]=fact[i,5]
#      Pen[i,j]=fact[(i-entry_yr),8]*fact[(i-entry_yr),9]
#    }
# }
#------------------------------pension plan valuation---------------------------
AL_tt=rep(0,110)
NC_tt=rep(0,110)
Pen_tt=rep(0,110)
inf_tt=rep(0,110)
# inf_ass_tt=rep(0,110) #test

for (i in 1 : 110){
AL_tt[i]=crossprod(AL[i,],inf[i,])
NC_tt[i]=crossprod(NC[i,],inf[i,])
Pen_tt[i]=crossprod(Pen[i,],inf[i,])
}
inf_tt=rowSums(inf)
# inf_ass_tt=rowSums(inf_ass)#test
sum(AL_tt)
sum(NC_tt)
sum(Pen_tt)
sum(inf_tt)

Pensproj=matrix(0,nrow=110,ncol=9)
colnames(Pensproj)=c('yr','ALt','Ft','NCt','Ct','Bt','Rt','FRt','inf')

for (i in 1 : 110){
   Pensproj[i,1]=i-1
   Pensproj[i,2]=AL_tt[i]
   Pensproj[i,4]=NC_tt[i]
   Pensproj[i,6]=Pen_tt[i]
   Pensproj[i,7]=ret[i]#ret
   Pensproj[i,9]=inf_tt[i]
   
   if(i==1){
     Pensproj[i,3]=AL_tt[i]}
   else{Pensproj[i,3]=(Pensproj[i-1,3]+Pensproj[i-1,5]
                       -Pensproj[i-1,6])*(1+Pensproj[i-1,7])}
   
   Pensproj[i,5]=NC_tt[i]+k*(AL_tt[i]-Pensproj[i,3])
   Pensproj[i,8]=Pensproj[i,3]/Pensproj[i,2]
}  
sum(Pensproj[1:75,3])
sum(Pensproj[1:75,5])
# for simulation:
Ct_dist[,z]=Pensproj[,5]
Ft_dist[,z]=Pensproj[,3]
FR_dist[,z]=Pensproj[,8]
NC_dist[,z]=Pensproj[,4]
}


#-----------------------------------ggplot--------------------------------------
# pp=as.data.frame(Pensproj)
# scale=100
max_yr=90#80

title=if(ass_mort==0){"mortality assumption: base CPM2014"
}else{"mortality assumption: CPM2014 improvement table"}

subtitle="mortality experience: same as assumption"
# p1 random binomial variables same as assumption
# p1=ggplot(pp[pp$yr<max_yr,],aes(yr,ALt))+geom_line(aes(colour="ALt"),size=0.3)+geom_line(aes(y=Ft,colour="Ft"),size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("ALt"="red","Ft"="black"))+
#   labs(title=title,subtitle=subtitle,x="years",y="ALt/Ft")+theme_bw()+
#   theme(legend.position = c(0.85,0.7),legend.background = element_blank(),legend.title = element_text(size = 0.3),
#         plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8))+
#   scale_y_continuous(breaks=seq(-100000*scale,900000*scale,200000*scale),limits=c(-100000*scale,900000*scale))
# 
# p2=ggplot(pp[pp$yr<max_yr,],aes(yr,NCt))+geom_line(size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+
#   labs(x="years",y="NCt")+theme_bw()+
#   theme(plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8),
#         axis.title =element_text(size=10))+
#   scale_y_continuous(breaks=seq(0,20000*scale,5000*scale),limits=c(0,20000*scale))
# 
# p3=ggplot(pp[pp$yr<=max_yr,],aes(yr,Ct))+geom_line(size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+
#   labs(x="years",y="Ct")+theme_bw()+
#   theme(plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8),
#         axis.title =element_text(size=10))+
#   scale_y_continuous(breaks=seq(-2000000,2500000,1000000),limits=c(-2000000,2500000))
# p4=ggplot(pp[pp$yr<=max_yr,],aes(yr,Bt))+geom_line(size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+labs(x="years",y="Bt")+theme_bw()+
#   theme(plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8),
#         axis.title =element_text(size=10))+
#   scale_y_continuous(breaks=seq(0,8000000,2000000),limits=c(0,8000000))
# 
# p5=ggplot(pp[pp$yr<=(max_yr-1),],aes(yr,FRt))+geom_line(size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+
#   labs(x="years",y="FRt")+theme_bw()+
#   theme(plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8),
#         axis.title =element_text(size=10))#+
#   #scale_y_continuous(breaks=seq(0.9,10,1),limits=c(0.9,10))
# p6=ggplot(pp[pp$yr<=max_yr,],aes(yr,Rt))+geom_line()+
#   scale_x_continuous(breaks=seq(0,90,10))+
#   labs(x="years",y="Rt")+theme_bw()+
#   theme(axis.title =element_text(size=10))
# 
# p7=ggplot(pp[pp$yr<=73,],aes(yr,FRt))+geom_line(size=0.3)+
#   scale_x_continuous(breaks=seq(0,90,10))+
#   labs(x="years",y="FRt")+theme_bw()+
#   theme(plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8),
#         axis.title =element_text(size=10))+
#   scale_y_continuous(breaks=seq(0.8,1.2,0.2),limits=c(0.8,1.2))
# 
# #grid.arrange(p1,p2,p3,p4,p7,ncol = 1)
# comb=p1+p2+p3+p4+p5+p7+plot_layout(ncol=2)
# # ggsave(comb, file='integrated.pdf', width=2.5, height=7.5)
# comb
# p1
# ggsave(p1,file="AL_FV_base_scaled.pdf",width = 7,height = 3.5,units = "in")
# p2
# ggsave(p2,file="NC_base_scaled.pdf",width = 7,height = 3.5)
# p3
# ggsave(p3,file="Ct_base_scaled.pdf",width = 7,height = 3.5)
# p4
# ggsave(p4,file="Bt_base_scaled.pdf",width = 7,height = 3.5)
# p5
# ggsave(p5,file="FRt_base_scaled.pdf",width = 7,height = 3.5)
# p6
# ggsave(p6,file="Rt.pdf",width = 7,height = 3.5)
# p7
# ggsave(p7,file="FRt_sel_base_scaled.pdf",width = 7,height = 3.5)
#-----------------------------------Fan plot------------------------------------
Ct_dist_sort=t(apply(Ct_dist,1,sort,decreasing=T))
Ft_dist_sort=t(apply(Ft_dist,1,sort,decreasing=T))
FR_dist_sort=t(apply(FR_dist,1,sort,decreasing=T))
NC_dist_sort=t(apply(NC_dist,1,sort,decreasing=T))


Ct_final=cbind.data.frame(seq(0,90,1),Ct_dist_sort[1:91,])
colnames(Ct_final)=c('yr',paste("x",1:10000,sep=""))

Ft_final=cbind.data.frame(seq(0,90,1),Ft_dist_sort[1:91,])
colnames(Ft_final)=c('yr',paste("x",1:10000,sep=""))

FR_final=cbind.data.frame(seq(0,90,1),FR_dist_sort[1:91,])
colnames(FR_final)=c('yr',paste("x",1:10000,sep=""))

NC_final=cbind.data.frame(seq(0,90,1),NC_dist_sort[1:91,])
colnames(NC_final)=c('yr',paste("x",1:10000,sep=""))

FR_range=FR_final$x500-FR_final$x9500
FR_range_final=cbind.data.frame(seq(0,90,1),FR_range)
colnames(FR_range_final)=c('yr','range_int')
write.csv(FR_range_final,file="FR_int_range.csv")

d1=ggplot(Ct_final[Ct_final$yr<=max_yr,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="years",y="Ct")+theme_bw()+
  theme(plot.title=element_text(size=8),
                plot.subtitle = element_text(size = 8))+
  scale_y_continuous(breaks=seq(-20000*scale,25000*scale,10000*scale),limits=c(-20000*scale,25000*scale))+
  scale_x_continuous(breaks=seq(0,80,10))

d2=ggplot(Ft_final[Ft_final$yr<=max_yr,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(title=title,subtitle=subtitle,x="years",y="Ft")+theme_bw()+
  theme(plot.title=element_text(size=8),
        plot.subtitle = element_text(size = 8))+
  scale_y_continuous(breaks=seq(-100000*scale,900000*scale,200000*scale),limits=c(-100000*scale,900000*scale))+
  scale_x_continuous(breaks=seq(0,80,10))

d3=ggplot(FR_final[FR_final$yr<=60,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="years",y="FRt")+theme_bw()+
  theme(plot.title=element_text(size=8),
        plot.subtitle = element_text(size = 8))+
  scale_y_continuous(breaks=seq(0.9,1.1,0.05),limits=c(0.9,1.1))

d4=ggplot(NC_final[NC_final$yr<=35,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="years",y="NCt")+theme_bw()+
  theme(plot.title=element_text(size=8),
        plot.subtitle = element_text(size = 8))+
  scale_y_continuous(breaks=seq(0,20000*scale,5000*scale),limits=c(0,20000*scale))+
  scale_x_continuous(breaks=seq(0,35,10))


d1
ggsave("Ct_int_dist.pdf",width = 7,height = 3.5)
d2
ggsave("Ft_int_dist.pdf",width = 7,height = 3.5)
d3
ggsave("FR_int_dist.pdf",width = 7,height = 3.5)
d4
ggsave("NC_int_dist.pdf",width = 7,height = 3.5)
d2+d3+d4+d1+plot_layout(ncol=2)

# write.csv(inf_tt,file="inf_tt.csv")
# write.csv(inf_ass_tt,file="inf_ass_tt.csv")


# comp=cbind(Pensproj[,1],inf_tt,inf_ass_tt)
# colnames(comp)=c('yr','inf','inf_ass')
# comp=as.data.frame(comp)
# g1=ggplot(comp[comp$yr<=90,],aes(yr,inf))+geom_line(aes(colour="inf_rnd"))+geom_line(aes(y=inf_ass,colour="inf_ass"))+
#   scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("inf_rnd"="red",inf_ass="black"))+
#   labs(title=title,subtitle=subtitle,x="years",y="inf")+theme_bw()+
#   theme(legend.position = c(0.9,0.7),legend.background = element_blank(),
#         plot.title=element_text(size=8),
#         plot.subtitle = element_text(size = 8))
#
# ggsave(g1,file="inf_1000.pdf",width = 7,height = 3.5)



n <- 10
my.probs <- dbinom(c(0:n),n,.8)
my.proportions <- (0:n)/n
dt=as.data.frame(cbind(my.proportions,my.probs))
g1=ggplot(dt,aes(my.proportions,my.probs))+geom_line(linetype="dashed")+geom_point(pch=1)+labs(subtitle="distribution, size=10",x="proportions",y="probabilities")+
  scale_x_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  theme_bw()
n <- 1000
my.probs2 <- dbinom(c(0:n),n,.8)
my.proportions2 <- (0:n)/n
dt2=as.data.frame(cbind(my.proportions2,my.probs2))
g2=ggplot(dt2,aes(my.proportions2,my.probs2))+geom_line(linetype="dashed")+geom_point(pch=1)+labs(subtitle="distribution, size=1000",x="proportions",y="probabilities")+
  scale_x_continuous(breaks=seq(0,1,0.2),limits=c(0,1))+
  theme_bw()
g1+g2+plot_layout(ncol=2)

#####
range_int=read.csv("FR_int_range.csv",header=T)
range_total=read.csv("FR_total_range.csv",header=T)
range=cbind(range_int,range_total[,2])
colnames(range)=c('yr','range_int','range_total')
range=as.data.frame(range)
g1=ggplot(range[range$yr<=75,],aes(yr,range_int))+geom_line(aes(colour="random fund return"),size=0.5)+geom_line(aes(y=range_total,colour="random fund return and death"),size=0.5)+
  scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("random fund return"="red","random fund return and death"="black"))+
  labs(x="years",y="Range")+theme_bw()+
  theme(legend.background = element_blank(),
        plot.title=element_text(size=8),
        plot.subtitle = element_text(size = 8))
g1

install.packages("MASS")
library(MASS)
mean=c(0.05,0.15)
sigma=matrix(c(0.15,-0.1,-0.1,0.3),2,2)
fund_return=mvrnorm(n=1000,mean,sigma)
