filter_pca<-function(eigen_values=NULL,wtdata=NULL,date_time_name='date_time',target_name='alarm',exclude_variables=NULL,sdv=NULL,mean=NULL,zero_sdv_columns=NULL){
  iam=match.call()[[1]]
  
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  # Sources
  
  #Bk date_time
  if(!is.na(date_time_name)&&!is.null(date_time_name)){
    date_time_bk<-wtdata[,date_time_name]
    wtdata[,date_time_name]<-NULL
  }
  #Bk target
  if(!is.na(target_name)&&!is.null(target_name)&&(target_name %in% names(wtdata))){
    target_variable_bk<-wtdata[,target_name]
    wtdata[,target_name]<-NULL
  }
  #bk ld_id
  if('ld_id' %in% names(wtdata)){
    ld_id_column<-wtdata$ld_id
    wtdata$ld_id<-NULL
  }
  
  #Exclude columns
  if(!is.null(exclude_variables)&&!is.na(exclude_variables)){
    if(length(exclude_variables)==1) exclude_variables<-unlist(strsplit(exclude_variables,split = ','))
    if(any(names(wtdata) %in% exclude_variables)){
      excluded_variables<-wtdata[,(names(wtdata) %in% exclude_variables),drop=F]
      wtdata[,(names(wtdata) %in% exclude_variables)]<-NULL
    }
  }
  
  
  #Check if all columns all numeric
  classes<-unlist(lapply(wtdata,function(x) is.numeric(x)))
  if(!all(classes)) return(list(error=TRUE,data=NULL,msg=paste0('\nColumns: ',paste0(colnames(wtdata)[!classes],collapse = ','),' are not numeric type')))
  
  #Remove columns with >10% NA
  per<-0.1
  to_remove<-apply(wtdata,2,function(c){sum(is.na(c))>(per*length(c))})
  if(sum(to_remove)>0){
    if(verbose) cat(paste0("\nThis variables have more than ",100*per," of NA's: ",paste0(colnames(wtdata)[to_remove],collapse=',')))
    wtdata[,to_remove]<-NULL
  }
  
  #Standarize save for test
  if(is.null(sdv)||is.null(mean)){
    sdv<-apply(wtdata,2,sd,na.rm=T)
    zero_sdv_columns<-names(wtdata)[(sdv==0)]
    sdv<-sdv[!(sdv==0)]
    if(sum(sdv==0)>0){
      mean<-apply(wtdata[,!(names(wtdata) %in% zero_sdv_columns)],2,base::mean,na.rm=T)
    }else{
      mean<-apply(wtdata,2,base::mean,na.rm=T)
    }
  }
  
  if(exists('zero_sdv_columns',inherits = F)&&!is.null(zero_sdv_columns)&&!is.na(zero_sdv_columns)&&length(zero_sdv_columns)>0){
    if(verbose) cat(paste0("\nThis variables have zero standard deviation: ",paste0(zero_sdv_columns,collapse=',')))
    wtdata<-wtdata[,!(names(wtdata) %in% zero_sdv_columns)]
  }
  #Standarize save for test
  for(c in 1:ncol(wtdata)){
    wtdata[,c]<-((wtdata[,c]-mean[c])/sdv[c])
  }
  
  cat(paste0("\nInput features: ",paste0(colnames(wtdata),collapse=','),'\n'))
  colnames_bk<-colnames(wtdata)
  
  wtdata<-as.matrix(wtdata)
  selected_rows<-complete.cases(wtdata)
  wtdata<-wtdata[selected_rows,]
  
  #Update selected_rows
  if(exists("date_time_bk",inherits = F)) date_time_bk<-date_time_bk[selected_rows]
  if(exists("target_variable_bk",inherits = F)) target_variable_bk<-target_variable_bk[selected_rows]
  if(exists("ld_id_column",inherits = F)) ld_id_column<-ld_id_column[selected_rows]
  if(exists("excluded_variables",inherits = F)) excluded_variables<-excluded_variables[selected_rows,]
  
  if(is.null(eigen_values)||is.na(eigen_values)){ #Create new model
    pr<-prcomp(x=wtdata,scale=F,center=F)
    #biplot(x = pr)
    #ggbiplot(pr,obs.scale = 1, var.scale=1,groups=alarm,ellipse=FALSE,circle=FALSE,alpha = 0.1,varname.size = 3)
    
    
    pc1<-pr$x[,1]
    pc2<-pr$x[,2]
    correlations <- t(pr$rotation)*pr$sdev
    variables_pc1_correlation_sorted<-sort(correlations[1,],decreasing = TRUE)
    variables_pc2_correlation_sorted<-sort(correlations[2,],decreasing = TRUE)
    var<-pr$sdev^2
    pve<-var/sum(var)
    eigen_values<-NULL 
  }
  
  wtdata<-pr
  
  #Add the backup columns
  if(exists("date_time_bk",inherits = F)) wtdata[,date_time_name]<-date_time_bk
  if(exists("target_variable_bk",inherits = F)) wtdata[,target_name]<-target_variable_bk
  if(exists("ld_id_column",inherits = F)) wtdata$ld_id<-ld_id_column
  #Exclude columns
  if(exists("excluded_variables",inherits = F)) wtdata[,colnames(excluded_variables)]<-excluded_variables
  return(list(error=FALSE,data=list(eigen_values=eigen_values,wtdata=wtdata,sdv=sdv,mean=mean,zero_sdv_columns,selected_rows=selected_rows,per_variance=per_variance)))
}
