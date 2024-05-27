#Requires already run-through BRT models for the work, analyzes their association graphs, and generates recommended feature directions based on them.
#It is also worth checking visually, but in the big picture, it recognizes things.
#The script file itself should be copied to the same directory as the BRT directories, and the initial file defining the model structure (mudelid.txt) and the initial data file (QuantitativeSamplesBiomassesKeySpeciesWithCopernicusDataAndDepth.rds) should also be copied there, otherwise, it cannot be guaranteed that the order of the BRT directories fully matches the order in the models.txt file.

library(gbm)
library(pdp)
library(data.table)
library(stringr)

kordaja=1.2 #A threshold value above which a difference is recognised; for example, 1.2 is equivalent to 20%
eemaldada=FALSE #Do we remove those rows/models where no output was received?

setwd("E:\\RActions\\Obama\\JSDM\\benthos\\BRT_models_monthly_env_input") #catalogue where BRT models were generated 
andmed=readRDS("QuantitativeSamplesBiomassesKeySpeciesWithCopernicusDataAndDepth.rds") #data file
mudelid=read.table("mudelid.txt",header=F,dec=".",sep=";",colClasses=c("numeric","character","character")) #a file defining the dependent and independent variables in the model

tk=getwd()
alamkataloogid=list.dirs(full.names=F,recursive=F)

#reordering
jrk=alamkataloogid #for the new order
tykid=str_split(alamkataloogid,"_")
maximal=max(unlist(lapply(tykid,function(x){nchar(x)[1]})))
for (i  in 1:length(tykid)){
  if (nchar(tykid[[i]])[1]!=maximal){juurde=maximal-nchar(tykid[[i]][1]);jrk[i]=paste0("model",paste0(rep("0",juurde),collapse=""),substr(tykid[[i]][1],6,nchar(tykid[[i]][1])))}
}
alamkataloogid=alamkataloogid[order(jrk)] #new order


#main cycle
for (i in 1:length(alamkataloogid)){
tykk=str_split(alamkataloogid[i],"_")
liiginimi=paste(tykk[[1]][-1],collapse="_")
liiginr=which(names(andmed)==liiginimi)
setwd(alamkataloogid[i])
setwd("brt")
failid=list.files()
if(length(failid)==0){setwd(tk);if(eemaldada==T){mudelid=mudelid[mudelid$V1!=liiginr,]};next} #if there is no model output then remove or leave according to the initial parameter
load("brt_model.Rdata")
setDT(andmed4)
tyybid=NULL #seoste suunad
for (i in 1:length(nimed_brt)){
#print(i)
q1=as.numeric(quantile(andmed4[,nimed_brt[i],with=FALSE],1/3,na.rm=T))
q2=as.numeric(quantile(andmed4[,nimed_brt[i],with=FALSE],2/3,na.rm=T))
funkt=partial(mudel_brt,pred.var=nimed_brt[i],n.trees=optim,quantiles=T)
#plot(funkt,type="l")
k1=mean(funkt[1:3,"yhat"] )
k2=mean(funkt[4:6,"yhat"] )
k3=mean(funkt[7:9,"yhat"] )
tyyp="0"
if((k2/k1>kordaja)&(k2/k3>kordaja)){tyyp="0"}else{
if(k1/k3>kordaja){tyyp="-1"}else{
if(k3/k1>kordaja){tyyp="1"}
} 
}
tyybid=c(tyybid,tyyp)
}
jrknumbrid=rep(0,length(nimed_brt))
for (k in 1:length(nimed_brt)){
jrknumbrid[k]=which(names(andmed)==nimed_brt[k])
}
tyybid=paste(tyybid[order(jrknumbrid)],collapse=",")
jrknumbrid=paste(sort(jrknumbrid),collapse=",")
mudelid[mudelid$V1==liiginr,"V2"]=jrknumbrid
mudelid[mudelid$V1==liiginr,"V3"]=tyybid
setwd(tk)
}

write.table(file="mudelid_suundadega.txt",mudelid,dec=".",sep=";",quote=F,row.names = F,col.names = F) #output file
