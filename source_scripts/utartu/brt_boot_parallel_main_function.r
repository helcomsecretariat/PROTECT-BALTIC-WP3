#the main function

fun_brt_boot <- function(){
  gbmtuumi=min(ceiling(detectCores()/4),2)
  setwd(abikataloog)
  suppressWarnings(sink())
  closeAllConnections()
  try(dev.off(),silent=T)
  andmed=data.frame(unclass(andmed),stringsAsFactors = T)
  names(andmed)<-str_replace_all(names(andmed), c(" " = "." , "," = "" , "\\(" = "" , "\\)" = "")) #Remove forbidden characters from the field names.
  if(valjaennustus==TRUE){names(andmedvalja)<-str_replace_all(names(andmedvalja), c(" " = "." , "," = "" , "\\(" = "" , "\\)" = ""))}
  if(sisendfailina!=TRUE){
    #OVERVIEW OF THE DATASET
    print("Variable types are categorical (FAC), binary numeric (BIN),")
    print("integer (INT) and numerical (NUM)")
    print(paste("In the dataset there are",dim(andmed)[1],"datarows"))
    print("Variables in the dataset (with variable types) are:")
	zeroone=as.logical(unlist(lapply(andmed,function(x) setequal(x,c(0,1))))) #checks whether a numeric identifier may contain only the values 0 and 1
	nolevels=as.numeric(unlist(lapply(andmed,function(x) length(unique(x))))) #number of unique values
    classes=as.character(unlist(lapply(andmed,function(x) class(x)[1]))) #types of fields
    type=rep(NA,length(nolevels))
    type[classes=="factor"]="FAC"
    type[classes=="character"]="CHAR"
    type[classes=="integer"&nolevels==2]="BIN"
    type[classes=="integer"&nolevels!=2]="INT"
    type[classes=="numeric"&zeroone==F]="NUM"
	type[classes=="numeric"&zeroone==T]="BIN"
    print(paste(names(andmed),type))
    Sys.sleep(1)
    #DEPENDENT VARIABLES
    dep = readline("Enter the number(s) of the dependent variable(s) (separated by commas):")
    dep = unlist(strsplit(dep, ",")) #remove commas
    dep = unlist(lapply(dep,function(x) eval(parse(text=x)))) #considering colons
    dep = sort(unique(dep))
    print(paste("Dependent variable(s)",paste(names(andmed)[dep],collapse=", ")))
    #SPECIES VRIABLE
    spec = readline("Enter the number of of the stratification variable  (or 0 if no stratification is desired):")
    if (identical(spec,"0")){andmed2=andmed;specnos=1}else{ 
      spec = as.numeric(spec)
      print("The following strata are present:")
      print(levels(andmed[,spec]))
      specnos = readline("Choose the desired stratum/strata (separated by commas):")
      specnos = unlist(strsplit(specnos, ",")) #remove commas
      specnos = unlist(lapply(specnos,function(x) eval(parse(text=x)))) #considering colons
      specnos = sort(unique(specnos)) #factor numbers of all species of interest in the dataset "andmed" 
      if (length(specnos)==1){print(paste("Selected stratum is",levels(andmed[,spec])[specnos]))}else{
        print(paste("Selected strata are",paste(levels(andmed[,spec])[specnos],collapse=", ")))
      }
      andmed2=andmed[andmed[,spec]%in%(levels(andmed[,spec])[specnos]),] #throw out unnecessary lines
      andmed2 = data.frame(lapply(andmed2, function(x) if(is.factor(x)) factor(x) else x)) #ordering of factors
    }
    #RESTRICTIONS
    assign("kitsendused",NULL,envir=environment(fun_brt_boot)) #Later used for writing output
    print(paste("In the selected data subset there are",dim(andmed2)[1],"datarows"))
    andmed3=fun_piirang(andmed2) #restrictions have already been made here
    rm(andmed2)
    #INDEPENDENT VARIABLES
    print("Variables in the dataset:")
    nolevels=as.numeric(unlist(lapply(andmed3,function(x) length(unique(x))))) #number of unique values
    classes=as.character(unlist(lapply(andmed3,function(x) class(x)[1]))) #types of fields
	zeroone=as.logical(unlist(lapply(andmed3,function(x) setequal(x,c(0,1))))) #checks whether a numeric identifier may contain only the values 0 and 1
	type=rep(NA,length(nolevels))
    type[classes=="factor"]="FAC"
    type[classes=="character"]="CHAR"
    type[classes=="integer"&nolevels==2]="BIN"
    type[classes=="integer"&nolevels!=2]="INT"
    type[classes=="numeric"&zeroone==F]="NUM"
    type[classes=="numeric"&zeroone==T]="BIN"
   	print(paste(names(andmed3),type))
    indep = readline("Enter the number(s) of the independent variable(s) (separated by commas):")
    indep = unlist(strsplit(indep, ",")) #komad ära
    indep = unlist(lapply(indep,function(x) eval(parse(text=x)))) #consider colons
    indep = sort(unique(indep))
    print(paste("Independent variables are ",paste(names(andmed3)[indep],collapse=", ")))
    direction = readline("Enter the direction(s) of the independent variable(s) effect(s) (1=positive -1=negative 0=arbitrary; separated by commas):")
    direction = unlist(strsplit(direction, ",")) #komad ära
    print(paste("The directions of the independent variables are ",paste(paste(names(andmed3)[indep],direction,sep=" "),collapse=", ")))
    #NOW THE DATASET IS TOGETHER AND IT'S TIME TO GET DOWN TO BUSINESS
    #print("Nüüd joonistan graafikud!")
    #mgcv select=TRUE, method=REML
    #Marra, G. and S.N. Wood (2011) Practical variable selection for generalized additive models. Computational Statistics and Data Analysis 55,2372-2387.
    indep_names=(names(andmed3)[indep]) #names of independent variables
    if(length(indep)==1){direction=c(direction,0);indep_names=c(indep_names,"konstanttunnus");if(valjaennustus==T){andmedvalja$konstanttunnus=1}} #gbm with one independent variable tends to work incorrectly with cv, so we add a constant variable.
  }else{
    nolevels=as.numeric(unlist(lapply(andmed,function(x) length(unique(x))))) #number of unique values
    classes=as.character(unlist(lapply(andmed,function(x) class(x)[1]))) #types of fields
	zeroone=as.logical(unlist(lapply(andmed,function(x) setequal(x,c(0,1))))) #checks whether a numeric identifier may contain only the values 0 and 1
	type=rep(NA,length(nolevels))
    type[classes=="factor"]="FAC"
    type[classes=="character"]="CHAR"
    type[classes=="integer"&nolevels==2]="BIN"
    type[classes=="integer"&nolevels!=2]="INT"
    type[classes=="numeric"&zeroone==F]="NUM"
    type[classes=="numeric"&zeroone==T]="BIN"
   	specnos=1 #if file input
    spec="0" #if file input
    andmed3=andmed #if file input
    dep=sisendfail[,1] #if file input
  }
  for (liik in specnos){ 
    if (length(specnos)!=1){
      print(paste("Selected stratum is:",levels(andmed[,spec])[liik]))
      andmed3a=andmed3[andmed3[,spec]%in%(levels(andmed[,spec])[liik]),]}else{andmed3a=andmed3} #data3a only one species in the dataset.
    andmed3a = data.frame(lapply(andmed3a, function(x) if(is.factor(x)) factor(x) else x)) #ordering of factors
    for (i in 1:length(dep)){
      tryCatch({
        withTimeout({
          print(paste("Dependent variable is:",names(andmed3a)[dep[i]]))
          if(sisendfailina==TRUE){
            indep=sisendfail[i,2]
            indep = unlist(strsplit(indep, ",")) #komad ära
            indep = unlist(lapply(indep,function(x) eval(parse(text=x)))) #consider colons
            direction=sisendfail[i,3]
            direction = as.numeric(unlist(strsplit(direction, ","))) #commas and quotation marks away
            indep_names=(names(andmed3)[indep])
            print(paste("The directions of the independent variables are ",paste(paste(names(andmed3)[indep],direction,sep=" "),collapse=", ")))
            if(length(indep)==1){direction=c(direction,0);indep_names=c(indep_names,"konstanttunnus");if(valjaennustus==T){andmedvalja$konstanttunnus=1}} #additional field 
          } 
          print(paste("Sample size:",dim(andmed3a)[1]))
          #kataloogide loomine töökataloogi alla
          if(sisendfailina==TRUE){kataloog=paste("model",i,"_",names(andmed3a)[dep[i]],sep="")}else{
            if (identical(spec,"0")){kataloog=names(andmed3a)[dep[i]]}else{kataloog=paste(levels(andmed[,spec])[liik],"_",names(andmed3a)[dep[i]],sep="")} #if there is no species
          }
          dir.create(kataloog) #creating a save directory
          tkataloog=getwd()
          setwd(paste(tkataloog,kataloog,sep="/"))
          dir.create("brt")
          ##############################################################
          print("GBM boosted regression trees")
          andmed4=andmed3a[,c(indep,dep[i])] #data4 contains only the fields required for data3a
          if(length(indep)==1){andmed3a$konstanttunnus=1;andmed4$konstanttunnus=1;} #gbm with one independent variable tends to work incorrectly with cv, so we add one constant variable
          andmed4=andmed4[!is.na(andmed4[,names(andmed)[dep[i]]]),] #if there are missing values then we throw them out
          andmed4 = data.frame(lapply(andmed4, function(x) if(is.factor(x)) factor(x) else x)) #ordering of factors
          #gbm tends to break down when the data matrix contains fields that are not actually in use
          valem_brt=as.formula(paste(names(andmed)[dep[i]],"~."))
          if (type[dep[i]]%in%c("FAC","BIN")) jaotus="bernoulli" else jaotus="gaussian"
          if (type[dep[i]]=="FAC"){class(andmed4[,names(andmed)[dep[i]]])="integer";andmed4[,names(andmed)[dep[i]]]=andmed4[,names(andmed)[dep[i]]]-1}
          #if necessary, also changes the factor to 0-1 field
          if (jaotus=="bernoulli"){mudel_brt=try(gbm(valem_brt,distribution=jaotus,data=andmed4,shrinkage = lrvalue,cv.folds=10,n.trees=20000,interaction.depth=tree.complexity,class.stratify.cv=TRUE,var.monotone=direction,n.cores=gbmtuumi),silent=T)}else{
            mudel_brt=try(gbm(valem_brt,distribution=jaotus,data=andmed4,shrinkage = lrvalue,cv.folds=10,n.trees=20000,interaction.depth=tree.complexity,var.monotone=direction,n.cores=gbmtuumi),silent=T)}
          if(class(mudel_brt)=="try-error"){setwd(tkataloog);print(mudel_brt[1]);next}
          optim=try(gbm.perf(mudel_brt, plot.it = F, overlay = TRUE,method="cv"),silent=T) #find the optimal number of trees
          if(class(optim)=="try-error"){setwd(tkataloog);print(optim[1]);next}
          #model is ready
          nimed_brt=as.character(summary(mudel_brt,n.trees=optim,plotit=F)$var[1:(dim(andmed4)[2]-1)]) #based on the optimal model
          tulemused_brt=round(as.numeric(summary(mudel_brt,n.trees=optim,plotit=F)$rel.inf[1:(dim(andmed4)[2]-1)])) #based on the optimal model
          #r2 on the basis of cv
          if (jaotus=="bernoulli"){
            konst_ennustus=max(table(andmed4[,names(andmed)[dep[i]]])/length(andmed4[,names(andmed)[dep[i]]])) #frequency of most popular category
            ennustus=round(exp(mudel_brt$cv.fitted)/(1+exp(mudel_brt$cv.fitted)))
            mudel_ennustus=mean(ennustus==andmed4[,names(andmed)[dep[i]]])
            r2_brt=1-(1-mudel_ennustus)/(1-konst_ennustus) #r2 ready
          }else{
            konst_ennustus=var(andmed4[,names(andmed)[dep[i]]])*(length(andmed4[,names(andmed)[dep[i]]])-1) #zero model
            ennustus=mudel_brt$cv.fitted
            mudel_ennustus=sum((andmed4[,names(andmed)[dep[i]]]-ennustus)**2)
            r2_brt=1-mudel_ennustus/konst_ennustus #r2 ready
          }
          ################# gbm output
          #koondtulemused
          if (round(r2_brt,2)<=0){print("The combined explanatory power of the independent variables is below 1%")}else{
            print(paste("Dependent variable is:",names(andmed3a)[dep[i]]))
            print(paste("R2 equals",round(r2_brt,3), "and variable importance is"))
            print(cbind(nimed_brt,tulemused_brt))
            #sõltuvusgraafikud
            
            minjamax=NULL #here we keep in mind the minimum and maximum value for each independent continuous variable
            kategarv=NULL #here we keep in mind the number of different levels for each independent categorical variable
            
            for (j in 1:length(nimed_brt)){
              if(class((andmed4[nimed_brt[j]])[,1])[1]=="factor"){kategarv=c(kategarv,length(levels(andmed4[nimed_brt[j]][,1])))}else{
                minjamax=c(minjamax,min(andmed4[nimed_brt[j]][,1],na.rm=T),max(andmed4[nimed_brt[j]][,1],na.rm=T))}
            }
            
            ##########################
            print("Parallel bootstrap has started")
            #paralleelarvutuse kasutuselevõtt
            cl=makeCluster(max((detectCores())/2-2,1))
            registerDoParallel(cl)
            #bootstrapi algus
            datan=dim(andmed4)[1] #Sample size
            kaasa=c("lrvalue","tree.complexity","valjaennustus","andmedvalja","solmi")
            if(valjaennustus==F){kaasa=c("lrvalue","tree.complexity","solmi","valjaennustus")}
            se_abi = foreach(i=1:bootn, .combine='+', .inorder=F, .packages=c("gbm","pdp"), .export=kaasa, .errorhandling="remove") %dopar% {
              ennustusboot=NULL #here we gather everything together
              andmedboot=andmed4[sample(datan,replace=T),]
              andmedboot = data.frame(lapply(andmedboot, function(x) if(is.factor(x)) factor(x) else x)) #ordering of factors
              if (jaotus=="bernoulli"){mudel_brt_boot=gbm(valem_brt,distribution=jaotus,data=andmedboot,shrinkage = lrvalue,cv.folds=10,n.trees=20000,interaction.depth=tree.complexity,class.stratify.cv=TRUE,n.cores=gbmtuumi)}else{
                mudel_brt_boot=gbm(valem_brt,distribution=jaotus,data=andmedboot,shrinkage = lrvalue,cv.folds=10,n.trees=20000,interaction.depth=tree.complexity,n.cores=gbmtuumi)}
              optimboot=gbm.perf(mudel_brt_boot, plot.it = F, overlay = TRUE,method="cv") 
              if(valjaennustus==T){ennustusboot=predict(mudel_brt_boot,andmedvalja,type="response",n.trees=optimboot)}
              mm_loendur=1 #which starting point to look at for min and max vectors
              k_loendur=1 #where to look in the categorarv vector
              skipped=rep(0,length(nimed_brt)) #Is the partial calculation missing for any field (e.g., because not all levels are in the sample)?
              for (j in 1:length(nimed_brt)){
                if(class((andmed4[nimed_brt[j]])[,1])[1]=="factor"){
                  if(length(levels(andmedboot[nimed_brt[j]][,1]))<kategarv[k_loendur]){skipped[j]=1;ennustusboot=c(ennustusboot,rep(0,kategarv[k_loendur]));k_loendur=k_loendur+1}else{ #if a level is not represented, zeros are written and remembered. 
                    abi1boot=partial(mudel_brt_boot,pred.var=nimed_brt[j],n.trees=optimboot,type="auto",prob=T,train=andmedboot)
                    ennustusboot=c(ennustusboot,abi1boot[,2])
                    k_loendur=k_loendur+1
                  }
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1boot=partial(mudel_brt_boot,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optimboot,type="auto",prob=T,train=andmedboot) 
                  ennustusboot=c(ennustusboot,abi1boot[,2])
                  mm_loendur=mm_loendur+2 #in pairs
                }
              }
              ennustusboot=c(ennustusboot,skipped)
              tagastus=cbind(ennustusboot,ennustusboot**2) #predictions and their squares and partial dependence plot values and number of skipped times for each independent field
            }
            #end of bootstrap
            stopCluster(cl)
			on.exit(try(stopCluster(cl),silent=T))
            setwd(paste(tkataloog,kataloog,"brt",sep="/"))
            save(mudel_brt,optim,nimed_brt,se_abi,bootn,solmi,andmed4,file="brt_model.Rdata")
            
            #end of parallel calculation
            ###################
            
            kogup=dim(se_abi)[1] #number of returned vector rows
            skipped=se_abi[(kogup-length(nimed_brt)+1):kogup,1]
            eff_kordustearv=bootn-skipped #the actual number of bootstrap iterations performed for each indep field
            se_abi=se_abi[1:(kogup-length(nimed_brt)),] #let's throw the skipped part away here
            pikk_eff_kordustearv=NULL #for each partial output row
            k_loendur=1 #where to look in the categoryarv vector
            for (j in 1:length(nimed_brt)){
              if(class((andmed4[nimed_brt[j]])[,1])[1]=="factor"){
                pikk_eff_kordustearv=c(pikk_eff_kordustearv,rep(eff_kordustearv[j],kategarv[k_loendur]))
                k_loendur=k_loendur+1 
              }else{pikk_eff_kordustearv=c(pikk_eff_kordustearv,rep(eff_kordustearv[j],solmi))
              }
            }
            kogup=dim(se_abi)[1] #number of vector rows returned after truncation
            se_abi_partial=se_abi[(kogup-length(pikk_eff_kordustearv)+1):kogup,] #take only the part needed for partial curves
            se_partial=round(sqrt((pikk_eff_kordustearv*se_abi_partial[,2]-se_abi_partial[,1]**2)/(pikk_eff_kordustearv*(pikk_eff_kordustearv-1))),4) #partial curve SE in one row
            
            
            
            pdf(file="depcurve_brt.pdf")
            mm_loendur=1 #which starting point to look at in the minjamax vector
            k_loendur=1 #where to look in the categoryarv vector
            yldloendur=1
            for (j in 1:length(nimed_brt)){
              if (jaotus=="bernoulli"){
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",
                                                                                                                   round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="p")
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }else{
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""))
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }
            }
            dev.off()
            
            pdf(file="depcurve_brt_joint.pdf")
            mm_loendur=1 #which starting point to look at in the minjamax vector
            k_loendur=1 #where to look in the categoryarv vector
            yldloendur=1
            par(mfrow=c(3,3))
            for (j in 1:(min(length(nimed_brt),9))){
              if (jaotus=="bernoulli"){
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",
                                                                                                                   round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="p")
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }else{
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""))
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }
            }
            dev.off()
            
            win.metafile("depcurve_brt%02d.wmf", pointsize = 10)
            mm_loendur=1 #millist alguskohta peab minjamax vektoris vaatama
            k_loendur=1 #millist kohta peab kategarv vektoris vaatama
            yldloendur=1
            for (j in 1:length(nimed_brt)){
              if (jaotus=="bernoulli"){
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",
                                                                                                                   round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="p")
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }else{
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""))
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }
            }
            dev.off()
            
            win.metafile(file="depcurve_brt_joint.wmf",pointsize=10)
            mm_loendur=1 #which starting point to look at in the minjamax vector
            k_loendur=1 #where to look in the categoryarv vector
            yldloendur=1
            par(mfrow=c(3,3))
            for (j in 1:(min(length(nimed_brt),9))){
              if (jaotus=="bernoulli"){
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",
                                                                                                                   round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="p")
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("prob of",levels(andmed3a[,dep[i]])[1]),ylim=c(0,1),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }else{
                if(class((andmed3a[nimed_brt[j]])[,1])[1]=="factor"){
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(abi1[,2],xaxt="n",ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""))
                  points(abi1[,2]+se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  points(abi1[,2]-se_partial[yldloendur:(yldloendur+kategarv[k_loendur]-1)],pch="*")
                  axis(side=1,labels=abi1[,1],at=1:length(abi1[,1]))
                  yldloendur=yldloendur+kategarv[k_loendur]
                  k_loendur=k_loendur+1
                }else{
                  gridframe=data.frame(seq(minjamax[mm_loendur],minjamax[mm_loendur+1],length=solmi))
                  names(gridframe)=nimed_brt[j]
                  abi1=partial(mudel_brt,pred.var=nimed_brt[j],pred.grid=gridframe,n.trees=optim,type="auto",prob=T,train=andmed4)
                  plot(y=abi1[,2],x=abi1[,1],ylab=paste("expectation of",names(andmed3a)[dep[i]]),ylim=c(min(andmed4[,names(andmed)[dep[i]]]),max(andmed4[,names(andmed)[dep[i]]])),main=paste(names(andmed3a)[dep[i]],"R2 =",round(r2_brt,3)),xlab=paste(nimed_brt[j]," ",tulemused_brt[j],"%",sep=""),type="l")
                  lines(y=abi1[,2]+se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  lines(y=abi1[,2]-se_partial[yldloendur:(yldloendur+solmi-1)],x=abi1[,1],lty=2)
                  rug(unlist(unique(andmed3a[nimed_brt[j]])))
                  yldloendur=yldloendur+solmi
                  mm_loendur=mm_loendur+2
                }
              }
            }
            dev.off()
            
            if(valjaennustus==T){
              andmedvalja$ennustus_brt=round(predict(mudel_brt,andmedvalja,type="response",n.trees=optim),4)
              andmedvalja$ennustus_se_brt=round(sqrt((bootn*se_abi[(1:dim(andmedvalja)[1]),2]-se_abi[(1:dim(andmedvalja)[1]),1]**2)/(bootn*(bootn-1))),4) #see on juba eelenevalt bootstrapiga leitud
              fwrite(andmedvalja,"brt_prediction.txt",sep=";",dec=".") #predict data
              if(piltvalja==T){
				toonid=c("blue","white","red")
				varvisamm=100
                crsvalue=paste("+init=epsg:",epsgvalue,sep="")
                writeRaster(rasterFromXYZ(andmedvalja[,c("X","Y","ennustus_brt")],  crs = CRS(crsvalue)), filename = "brt_ennustus.tif", format = 'GTiff',overwrite=T)
                writeRaster(rasterFromXYZ(andmedvalja[,c("X","Y","ennustus_se_brt")],  crs = CRS(crsvalue)), filename = "brt_ennustus_se.tif", format = 'GTiff',overwrite=T)
                win.metafile("brt_prediction.emf")
                color.gradient_brt <- function(x, colors=toonid,colsteps=varvisamm ) {
                  return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(andmedvalja$ennustus_brt),max(andmedvalja$ennustus_brt),length.out=colsteps)) ] )
                }
                plot(Y~X,pch=".",data=andmedvalja,col=color.gradient_brt(ennustus_brt))
                dev.off()
                if (min(andmedvalja$ennustus_brt)>0){
                  win.metafile("brt_prediction_log.emf")
                  color.gradient_brt_log <- function(x, colors=toonid, colsteps=varvisamm) {
                    return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(log(andmedvalja$ennustus_brt)),max(log(andmedvalja$ennustus_brt)),length.out=colsteps)) ] )
                  }
                  plot(Y~X,pch=".",data=andmedvalja,col=color.gradient_brt_log(log(ennustus_brt)))
                  dev.off()
                }
                win.metafile("brt_prediction_se.emf")
                color.gradient_brt <- function(x, colors=toonid, colsteps=varvisamm) {
                  return(colorRampPalette(colors) (colsteps) [ findInterval(x, seq(0,max(andmedvalja$ennustus_se_brt),length.out=colsteps)) ] )
                }
                plot(Y~X,pch=".",data=andmedvalja,col=color.gradient_brt(ennustus_se_brt))
                dev.off()
              }
              
            }
          }
          #####################################################
          #summary output
          setwd(paste(tkataloog,kataloog,sep="/")) 
          sink("summary.txt")
          print(paste("Dependent variable:",names(andmed)[dep[i]]))
          if (!identical(spec,"0")){print(paste("Stratum:",levels(andmed[,spec])[liik]))}
          if (!sisendfailina){print(kitsendused)}
          print(paste("Independent variables and their fixed directions:",paste(paste(indep_names,direction,sep=" "),collapse=", ")))
          print(paste("Sample size used:",dim(andmed4)[1]))
          resultmat=matrix("",nrow=4,ncol=1)
          resultmat[1,1]=c("BRT")
          resultmat[2,1]=paste("lr=",lrvalue,", interaction.depth=",tree.complexity,", trees=",optim,sep="")
          resultmat[3,1]=paste("R2=",round(r2_brt,3),sep="")
          print(resultmat)
          sink()
        }, timeout = timer)
      }, TimeoutException = function(ex) {
        message("Fitting this model takes too long. Stopped trying.")
      })
      setwd(tkataloog)
    }
  }
  if (!sisendfailina){rm(kitsendused,envir = environment(fun_brt_boot))}
  rm(andmed4)
  print("The code has SUCCESSFULLY terminated!")
}




#NARROWING RECURSION SUPPORT FUNCTION
fun_piirang=function(andmed3){ #additional restrictive fields
  print("Variables in the dataset:")
  nolevels=as.numeric(unlist(lapply(andmed3,function(x) length(unique(x))))) #number of unique values
  classes=as.character(unlist(lapply(andmed3,function(x) class(x)[1]))) #field types
  zeroone=as.logical(unlist(lapply(andmed3,function(x) setequal(x,c(0,1))))) #checks whether a numeric identifier may contain only the values 0 and 1
  type=rep(NA,length(nolevels))
  type[classes=="factor"]="FAC"
  type[classes=="character"]="CHAR"
  type[classes=="integer"&nolevels==2]="BIN"
  type[classes=="integer"&nolevels!=2]="INT"
  type[classes=="numeric"&zeroone==F]="NUM"
  type[classes=="numeric"&zeroone==T]="BIN"
  print(paste(names(andmed3),type))
  const1 = readline("Enter the number of the constraining variable (or 0 if there are no more constraints):")
  const1 = as.numeric(const1)
  if (const1==0){return(andmed3)} else{
    print("Constraining variable has the following levels:")
    print(levels(andmed3[,const1]))
    levs = readline("Enter the numbers of the suitable levels (separated by commas):")
    levs = unlist(strsplit(levs, ",")) #remove commas
    levs = unlist(lapply(levs,function(x) eval(parse(text=x)))) #consider colons
    levs = sort(unique(levs))
    lause=(paste("Suitable levels of the constraining variable",names(andmed3)[const1],"are", paste(levels(andmed3[,const1])[levs],collapse=", ")))
    print(lause)
    koos=cbind(get("kitsendused",environment(fun_brt_boot)),lause,deparse.level=0)
    assign("kitsendused", koos, envir = environment(fun_brt_boot))
    andmed3=andmed3[andmed3[,const1]%in%levels(andmed3[,const1])[levs],]
    andmed3 = data.frame(lapply(andmed3, function(x) if(is.factor(x)) factor(x) else x))
    print(paste("The selected subset has",dim(andmed3)[1],"datarows"))
    andmed3=fun_piirang(andmed3)
  }
  return(andmed3)
}
