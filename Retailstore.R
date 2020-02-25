getwd()
setwd("C://Users//lenovo//Documents//R")
strain = read.csv("store_train.csv", stringsAsFactors = F)
stest = read.csv("store_test.csv", stringsAsFactors = F)

#strain$Total_Sales=strain$sales0+strain$sales1+strain$sales2+strain$sales3+strain$sales4
#stest$Total_Sales=stest$sales0+stest$sales1+stest$sales2+stest$sales3+stest$sales4

stest$store=NA
strain$data = 'train'
stest$data = 'test'

sall = rbind(strain,stest)

library(dplyr)
glimpse(sall)
library(tidyr)
sall=sall %>% separate(Areaname,into=c("Area","Region"),sep=",")
length(unique(sall$Region))
length(unique(sall$Area))
length(unique(sall$countyname))
length(unique(sall$state_alpha))

lapply(sall,function(x) length(unique(x)))

names(sall)[sapply(sall,function(x) is.character(x))]

glimpse(sall)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

cat_cols=c("Region", "countyname" , 
           "countytownname" ,"state_alpha","store_Type" , "Area")

#char_logical=sapply(sall,is.character) 
#cat_cols=names(sall)[char_logical] 
#cat_cols=cat_cols[!(cat_cols %in% c('data','store'))] 
#cat_cols

for(col in cat_cols){ 
  sall=CreateDummies(sall,col,100)
}

sapply(sall,function(x) sum(is.na(x)))
sapply(strain,function(x) sum(is.na(x)))

sall=sall[!((is.na(sall$store)) & sall$data=='train'), ] 


for(col in names(sall)){
  if(sum(is.na(sall[,col]))>0 & !(col %in% c("data","store"))){
    sall[is.na(sall[,col]),col]=mean(sall[sall$data=='train',col],na.rm=T)
  }
} 

sall=sall %>% select(-storecode)

strain=sall %>% filter(data=='train') %>% select(-data) 
stest=sall %>% filter(data=='test') %>% select(-data,-store)

set.seed(2)
s=sample(1:nrow(strain),0.8*nrow(strain))
strain1=strain[s,]
strain2=strain[-s,]

library(car) 

##checking VIF
lm_fit=lm(store~.-Id,data=strain1)

sort(vif (lm_fit),decreasing = T)[1:3]
summary(lm_fit)

lm_fit=lm(store~.-Id-sales0,data=strain1)
sort(vif (lm_fit),decreasing = T)[1:3]
summary(lm_fit)

lm_fit=lm(store~.-Id-sales0-sales2,data=strain1)
sort(vif (lm_fit),decreasing = T)[1:3]
summary(lm_fit)

lm_fit=lm(store~.-Id-sales0-sales2-sales3,data=strain1)
sort(vif (lm_fit),decreasing = T)[1:3]
summary(lm_fit)
##running regression

strain$store=as.numeric(strain$store==1) 

fit = glm(store~.-Id-sales0-sales2-sales3,data=strain1, family='binomial')
fit=step(fit)
summary(fit)

fit = glm(store~.-Id-sales0-sales2-sales3-state_alpha_VT,data=strain1, family='binomial')
fit=step(fit)
summary(fit)

fit = glm(store~.-Id-sales0-sales2-sales3-state_alpha_VT-state_alpha_GA,data=strain1, family='binomial')
fit=step(fit)
summary(fit)

strain2$score=predict(fit,newdata=strain2,type="response")

library(pROC)
auc(roc(strain2$store,strain2$score))#0.7865
formula(fit)
##running on entire traindata

lm_fitfinal=lm(store~.-Id,data=strain)
sort(vif (lm_fitfinal),decreasing = T)[1:3]

lm_fitfinal=lm(store~.-Id-sales0,data=strain)
sort(vif (lm_fitfinal),decreasing = T)[1:3]

lm_fitfinal=lm(store~.-Id-sales0-sales2,data=strain)
sort(vif (lm_fitfinal),decreasing = T)[1:3]

lm_fitfinal=lm(store~.-Id-sales0-sales2-sales3,data=strain)
sort(vif (lm_fitfinal),decreasing = T)[1:3]

fitfinal = glm (store~.-Id-sales0-sales2-sales3,data=strain, family='binomial')
fitfinal=step(fitfinal)
summary(fitfinal)

fitfinal = glm (store~.-Id-sales0-sales2-sales3-state_alpha_CT,data=strain, family='binomial')
fitfinal=step(fitfinal)
summary(fitfinal)

formula(fitfinal)

## required to for final submission
test.prob.score= predict(fitfinal,newdata = stest,type='response')
write.csv(test.prob.score,"Retail_logistics.csv",row.names = F)


## trial run 2

lm_fit=lm(store~.-Id,data=strain)

sort(vif (lm_fit),decreasing = T)[1:3]

lm_fit=lm(store~.-Id-sales0,data=strain)
sort(vif (lm_fit),decreasing = T)[1:3]

lm_fit=lm(store~.-Id-sales0-sales2,data=strain)
sort(vif (lm_fit),decreasing = T)[1:3]

lm_fit=lm(store~.-Id-sales0-sales2-sales3,data=strain)
sort(vif (lm_fit),decreasing = T)[1:3]

strain$store=as.numeric(strain$store==1) 

fit = glm(store~.-Id-sales0-sales2-sales3,data=strain, family='binomial')
fit=step(fit)
summary(fit)
formula(fit)

fit = glm(store~.-Id-sales0-sales2-sales3-state_alpha_VT-state_alpha_GA,data=strain, family='binomial')
fit=step(fit)
summary(fit)

strain$score=predict(fit,newdata=strain,type="response")

library(pROC)
auc(roc(strain$store,strain$score)) #0.799


## running Decision tree--------------------------

sall$store= as.factor(sall$store)
sall.tree=tree(store~.-Id,data=strain1)

##validation 

val.score=predict(sall.tree, newdata= strain2, type='vector')
val.score
dim(strain1)
dim(strain2)
pROC::roc(strain2$store,val.score)$auc
sall.final.tree=tree(store~.-Id,data=strain)
test.prob.score= predict(sall.final.tree,newdata = stest,type='vector')
write.csv(test.prob.score,"Retail_dtree.csv",row.names = F)

