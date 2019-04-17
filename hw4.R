kt<-read.csv(file.choose(), header=TRUE,stringsAsFactors = FALSE)
#Exercise1
timetrend1<-kt[5:13,5]
logwage1<-kt[5:13,3]#extract information of personid=2
plot(timetrend1,logwage1,type = "p",main = "ppid=2")
timetrend2<-kt[15:26,5]
logwage2<-kt[15:26,3]#extract information of personid=4
plot(timetrend2,logwage2,type = "p",main = "ppid=4")
timetrend3<-kt[30:41,5]
logwage3<-kt[30:41,3]#extract information of personid=6
plot(timetrend3,logwage3,type = "p",main = "ppid=6")
timetrend4<-kt[45:58,5]
logwage4<-kt[45:58,3]#extract information of personid=8
plot(timetrend4,logwage4,type = "p",main = "ppid=8")
timetrend5<-kt[72:83,5]
logwage5<-kt[72:83,3]#extract information of personid=12
plot(timetrend5,logwage5,type = "p",main = "ppid=12")
#Exercise2
educ<-as.numeric(kt$EDUC)
logwage<-as.numeric(kt$LOGWAGE)
potexper<-as.numeric(kt$POTEXPER)
re<-as.data.frame(cbind(logwage,educ,potexper))
library(nlme)
gls(logwage~educ+potexper,data = re)#random effect model

#Exercise3
#between estimator
library("dplyr")
numperiod<-as.matrix(table(kt$PERSONID))# table the number of time trends of each individual.
ppid<-kt$PERSONID
fe<-cbind(ppid,re)
fe_sum<-fe %>%
  group_by(ppid) %>%
  summarise(logwgesum  = sum(logwage),
            educsum= sum(educ),
            potexpersum=sum(potexper))

febtw            = data.frame(fe_sum)# compute the sum of logwage, educ and potexper of each individual
logwage_avg<-febtw$logwgesum/numperiod
educ_avg<-febtw$educsum/numperiod
potexper_avg<-febtw$potexpersum/numperiod
febtw_full<-data.frame(febtw$ppid,logwage_avg,educ_avg,potexper_avg)# average matrix
lm(logwage_avg~educ_avg+potexper_avg,febtw_full)
colnames(febtw_full)=c("PERSONID","logwage_avg","educ_avg","potexper_avg")

#within estimator
merge<-kt%>% left_join(febtw_full, by="PERSONID")# merge the original database with ppid's average
logwage_wtin<-merge[,3]-merge[,11]
educ_wtin<-merge[,2]-merge[,12]
potexper_wtin<-merge[,4]-merge[,13]
fewtn<-data.frame(merge$PERSONID,logwage_wtin,educ_wtin,potexper_wtin)
lm(logwage_wtin~educ_wtin+potexper_wtin-1,fewtn)

#first time difference estimator
rep<-kt[1,]
ktchange<-data.frame(rbind(rep,kt))
ktchange<-ktchange[1:17919,]
diff<-kt-ktchange
diff<-diff[2:17919,]
dif<-diff[!diff[,1]==1,]# drop the invalid row like (2,1)-(1,4)
logwage_3<-dif$LOGWAGE
educ_3<-dif$EDUC
potexper_3<-dif$POTEXPER
fe_3<-data.frame(logwage_3,educ_3,potexper_3)
lm(logwage_3~educ_3+potexper_3,fe_3)

#Exercise 4
#likelihood and optimzie, estimate the fixed effect
rs<-sample(1:2178,100)
kt_select<-kt[,1:4]
kt_select<-kt_select[kt_select[,1] %in% rs,]#random select 100 personid
y<-as.matrix(kt_select$LOGWAGE)
X<-as.matrix(kt_select[,c(2,4)])
beta_func<-function(beta){
  return(-sum(y*log(pnorm(X%*%beta)))+sum((1-y)*log(1-pnorm(X%*%beta))))
}# Generate the likelihood function
start<-c(0,0)
beta_func(start)
beta=optim(par = start,beta_func)$par# optimize the likelihood function
kt_select_avr<-febtw_full[febtw_full[,1] %in% rs,]
y_ppid<-as.matrix(kt_select_avr$logwage_avg)
x_ppid<-as.matrix(kt_select_avr[,3:4])
alpha<-y_ppid-x_ppid%*%beta

#fixed effects on the invariant variables
inkt<-kt[,c(1,2,3,4,6,7,8,9,10)]
in_kt<-inkt[!duplicated(inkt[,1]),]
in_kt<-in_kt[in_kt$PERSONID %in% rs,]
inv1<-in_kt$ABILITY
inv2<-in_kt$MOTHERED
inv3<-in_kt$FATHERED
inv4<-in_kt$BRKNHOME
inv5<-in_kt$SIBLINGS
y_inv<-alpha
in_ktfull<-data.frame(cbind(y_inv,inv1,inv2,inv3,inv4,inv5))
lm(y_inv~inv1+inv2+inv3+inv4+inv5,in_ktfull)

#alternative method to compute standard errors
#Explain
#The errors are potentially serially correlated like over t
# and heteroskedastic
library(sandwich)
mod<-lm(logwage_wtin~educ_wtin+potexper_wtin-1,fewtn)
se<-diag(vcovHC(mod,type="HC3"))^0.5
se
#Use bootstrap

boot = matrix(0,nrow=49,ncol=6)
for (b in 1:49) {
  i = matrix(sample(unique(inkt[,1]),100,replace=TRUE),ncol=1) #subsampling
  d = matrix(0,ncol=9,nrow=1)
  for (c in 1:100){
    d = rbind(d,as.matrix(inkt[inkt[,1]%in%i[c,],]))
  }
  dat = as.data.frame(d[2:nrow(d),])
  y = as.matrix(dat[,3])
  X = as.matrix(dat[,c(2,4)])
  beta_boot = optim(c(0,0),beta_func)$par #return the beta
  mean_boot = aggregate(cbind(LOGWAGE,EDUC,POTEXPER) ~ PERSONID, data = dat, mean)
  alpha_boot = matrix(mean_boot[,2] - (mean_boot[,3]*beta_boot[1]+mean_boot[,4]*beta_boot[2]),ncol=1)
  dat_uni = dat[!duplicated(dat$PERSONID),]
  X2_boot = as.matrix(dat_uni[,3:7])
  model = lm(alpha_boot ~ X2_boot)$coef
  boot[b,] = model
}
sd_boot = cbind(sd(boot[,1]),sd(boot[,2]),sd(boot[,3]),sd(boot[,4]),sd(boot[,5]),sd(boot[,6]))
colnames(sd_boot) = c("Intercept","ABILITY","MOTHERED","FATHERED","BRKNHOME","SIBLINGS")
rownames(sd_boot) = c("corrected_sd_bootstrap")
sd_boot
  










