library(patchwork)
library(ggplot2)
library(MASS)
#-----------------------------------control------------------------------------
ctrl_rand_mort=1# mortality experience: 1 for random mort exp, 0 for deterministic mort exp 
ass_mort=1 # assumed mortality: 1 for improved table, 0 for base CPM table
scale=100 # increase # of member(1 or 100)
ctrl_opengroup=1 #0 for close group, 1 for open group
ctrl_sample_path=0# 1 for sample path, 0 for simulation
ctrl_fixed_fund_return=0  #in simulation:1 for fixed fund return when value randomness in deaths, 
                          #0 otherwise
ctrl_source_fundret=0 # for 10000 fund returns in simulation: 
                      #0 for r.v. generated in R, 1 for reading the table(
                      #only effective when ctrl_fixed_fund_return=0)

ctrl_corr=1# 0 for w/o correlation, 1 for w/ correlation in multi normal dist

#------------------assumption for fund return(Multinormal)-----------------------
mean=c(0.05,0.15)
if(ctrl_corr==0){
sigma=matrix(c(0.15,0,0,0.3),2,2)
}else{sigma=matrix(c(0.15,-0.1,-0.1,0.3),2,2)}
prop=0.5
#update the file paths when reading the table
if(ctrl_source_fundret==1&&ctrl_fixed_fund_return==0){
fund_return=read.csv("C:\\F\\pension_projection\\Rt_unsort_close_lowvar_dist_total_w cor.csv",header = T)
}
if(ctrl_fixed_fund_return==1){
  fund_return=read.csv("C:\\F\\pension_projection\\Rt_close_lowvar_dist_total_w cor.csv",header = T)}
#-----------------------------------plot feature---------------------------------
max_yr=90
title=if(ctrl_sample_path==0&&ctrl_fixed_fund_return==1){"fund returns: deterministic returns"
}else{if(ctrl_corr==1){
  "fund returns: random multivariate normal variables w/ correlation"
  }else{"fund returns: random multivariate normal variables w/o correlation"}}

subtitle=if(ctrl_rand_mort==0){"mortality experience: same as assumption"
}else{"mortality experience: random binomial variables"}

#---------------------------------file names-------------------------------------
#define different file names of outputs under different scenario
if(ctrl_opengroup==0){e="close"
}else{e="open"}
if(ctrl_sample_path==0){f="dist"
}else{f="single"}
if(ctrl_rand_mort==0){g="int"
}else{if(ctrl_fixed_fund_return==1){g="mort"
}else{g="total"}}
if(ctrl_corr==0){h="wo corr"
}else{h="w cor"}

pdfname=function(x){
  paste(x,"_",e,"_",f,"_",g,"_",h,".pdf",sep="",collapse = "")
}
csvname=function(x){
  paste("C:\\F\\pension_projection\\",x,"_",e,"_",f,"_",g,"_",h,".csv",sep="",collapse = "")
}

#--------------------------------base mort table--------------------------------
basetable=read.csv("CPM.csv",header=T)

#-------------------------------impr mort table---------------------------------
imptable=read.csv("CPM_B_M.csv",header=T,check.names=FALSE)
imptable_adv=1-imptable
imptable_adv[,1]=imptable[,1]
mort_table=matrix(0,nrow=98,ncol=209)
colnames(mort_table)=c('age',seq(2021,2228,1))

for (i in 1 : 98){
  mort_table[i,1]=17+i

for (j in 2:209){
  if(j<=11){
  mort_table[i,j]=basetable[i,2]*prod(imptable_adv[i,2:(6+j)])}
  else{mort_table[i,j]=mort_table[i,j-1]*imptable_adv[i,17]}
}

}

#----------------------------------plan member---------------------------------
plan=read.csv("pension plan_mortv2.csv",header=T)


#-----------------------------------simulation---------------------------------
Ct_dist=matrix(0,nrow=110,ncol=10000)
Ft_dist=matrix(0,nrow=110,ncol=10000)
FR_dist=matrix(0,nrow=110,ncol=10000)
NC_dist=matrix(0,nrow=110,ncol=10000)
Rt_dist=matrix(0,nrow=110,ncol=10000)
 if(ctrl_sample_path==1){a=1
 }else{a=10000}
 if(ctrl_opengroup==1){b=96
 }else{b=6}
for (z in 1:a){
#----------------------------------fund return---------------------------------
if(ctrl_sample_path==1){
set.seed(717)
  fund_return=mvrnorm(n=110,mean,sigma)
ret=prop*fund_return[,1]+prop*fund_return[,2]
}else{if(ctrl_fixed_fund_return==1){
    ret=fund_return[,(a/2)]
  }else{if(ctrl_source_fundret==0){
    fund_return=mvrnorm(n=110,mean,sigma)
    ret=prop*fund_return[,1]+prop*fund_return[,2]
    }else{ret=fund_return[,z]}}
}
#-------------------------------pension plan projection------------------------
AL=matrix(0,nrow=150,ncol=max(plan[,1])) #AL,NC,Pen for each inf
NC=matrix(0,nrow=150,ncol=max(plan[,1]))
Pen=matrix(0,nrow=150,ncol=max(plan[,1]))
inf=matrix(0,nrow=150,ncol=max(plan[,1]))
inf_ass=matrix(0,nrow=150,ncol=max(plan[,1]))
k=0.2

if(ctrl_sample_path==1){set.seed(717)}
for (j in 1 : b){
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
fact=matrix(0,nrow=150,ncol=9)
colnames(fact)=c('t','age','px','qx','lx','disc','Dx','ben','sy')                           
for (i in 1 : 150){
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
  


for (i in 1 : 150){
  if(i>115-entry_age+1+entry_yr){
  AL[i,j]=0  
  } else{ 
  if(i<=entry_yr){
    AL[i,j]=0 
  }else{
  AL[i,j]=crossprod(fact[(i-entry_yr):150,7],fact[(i-entry_yr):150,8])*fact[(i-entry_yr),9]/fact[(i-entry_yr),6]/fact[(i-entry_yr),5]
  }}

  }


for (i in 1 : 150){
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
fact_val=matrix(0,nrow=150,ncol=5)
colnames(fact_val)=c('t','age','px','qx','lx') 

for (i in 1:150){
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
  if(ctrl_rand_mort==1){
  fact_val[i,5]=rbinom(1,fact_val[i-1,5],fact_val[i-1,3])
    
  }else{
  
  fact_val[i,5]=fact_val[i-1,5]*fact_val[i-1,3]}
  }


}
 for (i in 1:150){
    if(i-1<entry_yr){
    Pen[i,j]=0
    inf[i,j]=0
    
    
    }else{
    inf[i,j]=fact_val[(i-entry_yr),5]
    Pen[i,j]=fact[(i-entry_yr),8]*fact[(i-entry_yr),9]
    }

 }
#-----------------------------inf_ass for compare------------------------------
       for (i in 1 : 150){
        inf_ass[i,j]=fact[i,5]*No*scale
}


}

#------------------------------pension plan valuation---------------------------
AL_tt=rep(0,110)
NC_tt=rep(0,110)
Pen_tt=rep(0,110)
inf_tt=rep(0,110)
inf_ass_tt=rep(0,110)

for (i in 1 : 110){
AL_tt[i]=crossprod(AL[i,],inf[i,])
NC_tt[i]=crossprod(NC[i,],inf[i,])
Pen_tt[i]=crossprod(Pen[i,],inf[i,])
}
inf_tt=rowSums(inf)
inf_ass_tt=rowSums(inf_ass)


Pensproj=matrix(0,nrow=110,ncol=9)
colnames(Pensproj)=c('yr','ALt','Ft','NCt','Ct','Bt','Rt','FRt','inf')
for (i in 1 : 110){
   Pensproj[i,1]=i-1
   Pensproj[i,2]=AL_tt[i]
   Pensproj[i,4]=NC_tt[i]
   Pensproj[i,6]=Pen_tt[i]
   Pensproj[i,7]=ret[i]
   Pensproj[i,9]=inf_tt[i]
   
   if(i==1){
     Pensproj[i,3]=AL_tt[i]}
   else{Pensproj[i,3]=(Pensproj[i-1,3]+Pensproj[i-1,5]
                       -Pensproj[i-1,6])*(1+Pensproj[i-1,7])}
   
   Pensproj[i,5]=NC_tt[i]+k*(AL_tt[i]-Pensproj[i,3])
   Pensproj[i,8]=Pensproj[i,3]/Pensproj[i,2]
}  

# for simulation:
Ct_dist[,z]=Pensproj[,5]
Ft_dist[,z]=Pensproj[,3]
FR_dist[,z]=Pensproj[,8]
NC_dist[,z]=Pensproj[,4]
Rt_dist[,z]=Pensproj[,7]
}


#-----------------------------------ggplot--------------------------------------

# y scales need to be changed accordingly.

if(ctrl_sample_path==1){
pp=as.data.frame(Pensproj)

p1=ggplot(pp[pp$yr<max_yr,],aes(yr,ALt))+geom_line(aes(colour="ALt"),size=0.3)+geom_line(aes(y=Ft,colour="Ft"),size=0.3)+
  scale_x_continuous(breaks=seq(0,100,10))+scale_colour_manual(name="",values = c("ALt"="red","Ft"="black"))+
  labs(title=title,subtitle=subtitle,x="times",y="ALt/Ft")+theme_bw()+
  theme(legend.position = c(0.9,0.8),legend.background = element_blank(),legend.title = element_text(size = 0.3),
        plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))#+
  #scale_y_continuous(breaks=seq(-100000*scale,900000*scale,200000*scale),limits=c(-100000*scale,900000*scale))
p1
p2=ggplot(pp[pp$yr<max_yr,],aes(yr,NCt))+geom_line(size=0.3)+
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(x="times",y="NCt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12),
        axis.title =element_text(size=12))#+
  #scale_y_continuous(breaks=seq(0,20000*scale,5000*scale),limits=c(0,20000*scale))
p2
p3=ggplot(pp[pp$yr<=max_yr,],aes(yr,Ct))+geom_line(size=0.3)+
  scale_x_continuous(breaks=seq(0,100,10))+
  labs(x="times",y="Ct")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12),
        axis.title =element_text(size=12))#+
  #scale_y_continuous(breaks=seq(-2000000,2500000,1000000),limits=c(-2000000,2500000))
p3
p4=ggplot(pp[pp$yr<=max_yr,],aes(yr,Bt))+geom_line(size=0.3)+
  scale_x_continuous(breaks=seq(0,100,10))+labs(x="times",y="Bt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12),
        axis.title =element_text(size=12))#+
  #scale_y_continuous(breaks=seq(0,8000000,2000000),limits=c(0,8000000))
p4


p5=ggplot(pp[pp$yr<=(max_yr-1),],aes(yr,FRt))+geom_line(size=0.3)+
  scale_x_continuous(breaks=seq(0,90,10))+
  labs(x="times",y="FRt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12),
        axis.title =element_text(size=12))
p5

p6=ggplot(pp[pp$yr<=max_yr,],aes(yr,Rt))+geom_line()+
  scale_x_continuous(breaks=seq(0,90,10))+
  labs(x="times",y="Rt")+theme_bw()+
  theme(axis.title =element_text(size=12))
p6

p7=ggplot(pp[pp$yr<=70,],aes(yr,FRt))+geom_line(size=0.3)+
  scale_x_continuous(breaks=seq(0,90,10))+
  labs(x="times",y="FRt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12),
        axis.title =element_text(size=12))+
  scale_y_continuous(breaks=seq(0,3.4,0.4),limits=c(0,3.4))
p7

p1
ggsave(p1,file=pdfname("AL_FV"),width = 7,height = 3.5)
p2
ggsave(p2,file=pdfname("NC"),width = 7,height = 3.5)
p3
ggsave(p3,file=pdfname("Ct"),width = 7,height = 3.5)
p4
ggsave(p4,file=pdfname("Bt"),width = 7,height = 3.5)
p5
ggsave(p5,file=pdfname("FRt"),width = 7,height = 3.5)
p6
ggsave(p6,file=pdfname("Rt"),width = 7,height = 3.5)
p7
if(ctrl_opengroup==0){
ggsave(p7,file=pdfname("FRt_sel"),width = 7,height = 3.5)
  comb=p1+p2+p3+p4+p5+p7+plot_layout(ncol = 2)
  ggsave(comb,filename = pdfname("comb"),width=12,height = 9)
  }else{
comb=p1+p2+p3+p4+p5+plot_layout(ncol = 2)
ggsave(comb,filename = pdfname("comb"),width=12,height = 9)}

#-----------------------------------Fan plot------------------------------------
}else{Ct_dist_sort=t(apply(Ct_dist[1:91,],1,sort,decreasing=F))
Ft_dist_sort=t(apply(Ft_dist[1:91,],1,sort,decreasing=F))
FR_dist_sort=t(apply(FR_dist[1:91,],1,sort,decreasing=F))
NC_dist_sort=t(apply(NC_dist[1:91,],1,sort,decreasing=F))
Rt_dist_sort=t(apply(Rt_dist[1:91,],1,sort,decreasing=F))

Ct_final=cbind.data.frame(seq(0,90,1),Ct_dist_sort[1:91,])
colnames(Ct_final)=c('yr',paste("x",1:10000,sep=""))

Ft_final=cbind.data.frame(seq(0,90,1),Ft_dist_sort[1:91,])
colnames(Ft_final)=c('yr',paste("x",1:10000,sep=""))

FR_final=cbind.data.frame(seq(0,90,1),FR_dist_sort[1:91,])
colnames(FR_final)=c('yr',paste("x",1:10000,sep=""))

NC_final=cbind.data.frame(seq(0,90,1),NC_dist_sort[1:91,])
colnames(NC_final)=c('yr',paste("x",1:10000,sep=""))

Rt_final=cbind.data.frame(seq(0,90,1),Rt_dist_sort[1:91,])
colnames(Rt_final)=c('yr',paste("x",1:10000,sep=""))

FR_range=FR_final$x9500-FR_final$x500
FR_range_final=cbind.data.frame(seq(0,90,1),FR_range)
colnames(FR_range_final)=c('yr','range')
write.csv(FR_range_final,file=csvname("FR_range"),row.names = FALSE)

d1=ggplot(Ct_final[Ct_final$yr<=max_yr,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="times",y="Ct")+theme_bw()+
  theme(plot.title=element_text(size=12),
                plot.subtitle = element_text(size = 12))+
  scale_y_continuous(breaks=seq(-300000*scale,100000*scale,100000*scale),limits=c(-300000*scale,100000*scale))+
  scale_x_continuous(breaks=seq(0,90,10))
d1
d2=ggplot(Ft_final[Ft_final$yr<=max_yr,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(title=title,subtitle=subtitle,x="times",y="Ft")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))+
  scale_y_continuous(breaks=seq(-200000*scale,3000000*scale,1000000*scale),limits=c(-200000*scale,3000000*scale))+
  scale_x_continuous(breaks=seq(0,90,10))
d2
d3=ggplot(FR_final[FR_final$yr<=60,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="times",y="FRt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))+
  scale_y_continuous(breaks=seq(0.8,3,0.2),limits=c(0.8,3))+
  scale_x_continuous(breaks=seq(0,90,10))
d3
d4=ggplot(NC_final[NC_final$yr<=35,],aes(yr,x500))+geom_line(size=0.1)+geom_line(aes(y=x2500),size=0.1)+
  geom_line(aes(y=x5000),size=0.1)+geom_line(aes(y=x7500),size=0.1)+
  geom_line(aes(y=x9500),size=0.1)+labs(x="times",y="NCt")+theme_bw()+
  theme(plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))+
  scale_y_continuous(breaks=seq(0,20000*scale,5000*scale),limits=c(0,20000*scale))+
  scale_x_continuous(breaks=seq(0,90,10))
d4



ggsave(d1,file=pdfname("Ct"),width = 7,height = 3.5)

ggsave(d2,file=pdfname("Ft"),width = 7,height = 3.5)

ggsave(d3,file=pdfname("FR"),width = 7,height = 3.5)

ggsave(d4,file=pdfname("NC"),width = 7,height = 3.5)



write.csv(Ct_final,file=csvname("Ct"),row.names = FALSE)
write.csv(Ft_final,file=csvname("Ft"),row.names = FALSE)
write.csv(FR_final,file=csvname("FR"),row.names = FALSE)
write.csv(NC_final,file=csvname("NC"),row.names = FALSE)

if(ctrl_fixed_fund_return==0&&ctrl_rand_mort==1){
  write.csv(Rt_dist,file=csvname("Rt_unsort"),row.names = FALSE) 
  write.csv(Rt_dist_sort,file=csvname("Rt"),row.names = FALSE) 
}
}




#------------------------ actual vs assumed # of members in force -------------------
title="mortality assumption: CPM2014 improvement table"

subtitle="mortality experience: random binomial variables"

comp=cbind(Pensproj[,1],inf_tt[1:110],inf_ass_tt[1:110])
colnames(comp)=c('yr','inf','inf_ass')
comp=as.data.frame(comp)
if(scale==1){
f1=ggplot(comp[comp$yr<=90,],aes(yr,inf))+geom_line(aes(colour="actual number"))+geom_line(aes(y=inf_ass,colour="assumed number"))+
  scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("actual number"="red","assumed number"="black"))+
  labs(title=title,subtitle=subtitle,x="times",y="Number of members in force")+theme_bw()+
  theme(legend.position = c(0.8,0.9),legend.background = element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))
}else{
f1=ggplot(comp[comp$yr<=90,],aes(yr,inf))+geom_line(aes(colour="actual number"))+geom_line(aes(y=inf_ass,colour="assumed number"))+
  scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("actual number"="red","assumed number"="black"))+
  labs(x="times",y="Number of members in force")+theme_bw()+
  theme(legend.position = c(0.8,0.9),legend.background = element_blank())}
f1
ggsave(f1,file=paste("inf_",scale*10,".pdf",sep="",collapse = ""),width = 7,height = 3.5)

#--------------------------------------plot of range-------------------------------

#file paths should be updated accordingly:

range_int=read.csv("C:\\F\\pension_projection\\FR_range_open_dist_int_w cor.csv",header=T)
range_total=read.csv("C:\\F\\pension_projection\\FR_range_open_dist_total_w cor.csv",header=T)
range_mort=read.csv("C:\\F\\pension_projection\\FR_range_open_dist_mort_w cor.csv",header=T)
range=cbind(range_int,range_mort[,2],range_total[,2])
colnames(range)=c('yr','range_int','range_mort','range_total')
range=as.data.frame(range)
g1=ggplot(range[range$yr<=max_yr,],aes(yr,range_int))+geom_line(aes(colour="random fund returns \nand deterministic mortality"),size=0.3)+geom_line(aes(y=range_total,colour="random fund returns \nand random mortality"),size=0.3)+
  geom_line(aes(y=range_mort,colour="deterministic fund returns \nand random mortality"),size=0.3)+
  scale_x_continuous(breaks=seq(0,90,10))+scale_colour_manual(name="",values = c("random fund returns \nand deterministic mortality"="red","random fund returns \nand random mortality"="black",
                                                                                 "deterministic fund returns \nand random mortality"="blue"))+
  labs(x="times",y="Range of funded ratios from the 5th to the 95th percentile")+theme_bw()+
  theme(legend.background = element_blank(),
        plot.title=element_text(size=12),
        plot.subtitle = element_text(size = 12))+
  scale_y_continuous(breaks=seq(0,150,50),limits=c(0,150))

g1
ggsave(g1,file=pdfname("range"),width = 7,height = 5)
