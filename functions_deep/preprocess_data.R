clean_registers_others_ot <- function(wtdata=NULL,seconds_ot_marging=NULL,selected_rows=NULL,date_time_name='date_time',target_columns=c('pre_alarm')){
  aggregated_targets_column<-apply(wtdata[,target_columns,drop=F],1,function(t) any(t==1))
  if('ot_all' %in% names(wtdata)){
    if(!is.null(seconds_ot_marging)&&!is.na(seconds_ot_marging)&&seconds_ot_marging>0){
      others_ots_dt<-unique(wtdata[((wtdata$ot_all==1)&(aggregated_targets_column==0)),c(date_time_name,'ld_id')])
      train_ot_all_marging<-lapply(1:nrow(others_ots_dt),function(i) data.frame(ld_id=others_ots_dt$ld_id[i],date_time=seq.POSIXt(others_ots_dt[i,date_time_name]-as.difftime(seconds_ot_marging, units="secs"),by=paste0(seconds_to_aggregate,' sec'),others_ots_dt[i,date_time_name]+as.difftime(seconds_ot_marging, units="secs"),tz='UTC',origin='1970-01-01')))
      train_ot_all_marging<-do.call("rbind",train_ot_all_marging)
      train_ot_all_marging<-unique(train_ot_all_marging)
      colnames(train_ot_all_marging)[colnames(train_ot_all_marging)=='date_time']<-date_time_name
      train_ot_all_marging[,date_time_name]<-as.POSIXct(train_ot_all_marging[,date_time_name],tz = 'UTC',origin='1970-01-01')
      train_excluded_by_marging<-NULL
      for(ld in unique(train_ot_all_marging$ld_id)){
        tmp_rows<-which((aggregated_targets_column==0)&(wtdata$ld_id==ld)&(wtdata[,date_time_name] %in% train_ot_all_marging[train_ot_all_marging$ld_id==ld,date_time_name]))
        train_excluded_by_marging<-c(train_excluded_by_marging,tmp_rows)
      }
      if(length(train_excluded_by_marging)>0) selected_rows<-selected_rows[!(selected_rows %in% train_excluded_by_marging)]
    }
  }
  return(list(error=F,warning=F,data=selected_rows,msg='ok'))
}

preprocess_data<-function(lstm=F,wtdata=NULL,selected_columns=NULL,selected_turbines=NULL,max_date_time=max(wtdata[,date_time_name]),target_name=NULL,use_alarms=F,use_ots=F,use_health=F,anticipation=0,margin=15,seconds_to_aggregate=60*60*24,marging_after_pre_alarm=60*60*24*7,seconds_ot_marging=60*60*24,na_columns_per_exclude=20,normalize=T,stat=NULL,registers_before_pre_alarm=F,cut_in=NULL,windspd_name=NULL,date_time_name='date_time',normalization=T,standarization=F,exclude_columns='date_time,ld_id,health_status,ot,ot_block_code,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot_all,ot_all_block_code,pre_alarm'){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,warning=F,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('unbalanced','dbscan','RcppDL')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  set.seed(1)
  
  warning<-F
  msg<-'ok'
  
  #checks
  if(use_health&&(!('health_status' %in% names(wtdata))||length(unique(wtdata$health_status,na.rm = T))==1)) return(list(error=T,warning=F,data=NULL,msg='health_status column doesn\'t exists on wtdata or health_status is unique'))
  if(use_ots&&(!('ot' %in% names(wtdata))||length(unique(wtdata$ot,na.rm = T))==1)) return(list(error=T,warning=F,data=NULL,msg='ot column doesn\'t exists on wtdata or ot is unique'))
  if(use_alarms&&(!('alarm' %in% names(wtdata))||length(unique(wtdata$alarm,na.rm = T))==1)) return(list(error=T,warning=F,data=NULL,msg='alarm column doesn\'t exists on wtdata or alarm is unique'))
  if(!is.null(cut_in)&&is.null(windspd_name)){
    warning<-T
    msg<-'windspd_name is not speficied, not filtered by wind speed'
    cut_in<-NULL
    windspd_name<-NULL
  }
  if(!is.null(cut_in)&&!(windspd_name %in% names(wtdata))){
    warning<-T
    msg<-paste0(windspd_name,' column doesn\'t exists on wtdata, not filtered by wind speed')
    cut_in<-NULL
    windspd_name<-NULL
  }
  
  #Original
  selected_columns_original<-selected_columns
  
  #Automatic separation
  if(use_health||is.null(selected_turbines)||length(selected_turbines)<=1){
    if(use_health){
      selected_turbines<-unique(wtdata$ld_id[!is.null(wtdata$health_status)&!is.na(wtdata$health_status)])
    }else{ #train_machines is percentage
      #count pre_alarms per ld_id
      bad_lds<-unique(wtdata$ld_id[wtdata[,target_name]==T])
      number_of_bad<-ceiling(train_machines*length(bad_lds)/100)
      bad_lds<-sample(bad_lds,size = number_of_bad)
      good_lds<-unique(wtdata$ld_id[(wtdata[,target_name]==F)&!(wtdata$ld_id %in% bad_lds)])
      number_of_good<-ceiling(train_machines*length(good_lds)/100)
      good_lds<-sample(good_lds,size = number_of_good)
      selected_turbines<-c(good_lds,bad_lds)
    }
  }
  
  if(is.null(selected_turbines)||!(any(selected_turbines %in% unique(wtdata$ld_id)))) return(list(error=T,warning=F,data=NULL,msg='There is no train machines in wtdata'))
  
  #Filter selected turbines wtdata only
  wtdata<-wtdata[wtdata$ld_id %in% abs(selected_turbines),]
  
  wtdata[,date_time_name]<-as.POSIXct(wtdata[,date_time_name],tz='UTC',origin='1970-01-01')
  if(!is.null(max_date_time)) 
    selected_rows<-which((wtdata$ld_id %in% abs(selected_turbines))&(wtdata[,date_time_name]<=max_date_time))
  else
    selected_rows<-which((wtdata$ld_id %in% abs(selected_turbines)))
  lds<-unique(wtdata$ld_id)
  
  if(is.null(target_name)) target_name<-'pre_alarm'
  wtdata[,target_name]<-F
  
  
  for(ld in lds){
    current_turbine<-wtdata[wtdata$ld_id==ld,]
    if(use_health&&any(current_turbine$health_status=='bad',na.rm = T)){
      current_bad<-current_turbine[(current_turbine$health_status=='bad'),date_time_name]
      #max_dt<-max(current_bad,na.rm = T)
      #bad_dates<-seq.POSIXt(max_dt-as.difftime(anticipation, units="days"),by=paste0(seconds_to_aggregate,' sec'),max_dt,tz='UTC',origin='1970-01-01')
      wtdata[(wtdata$ld_id==ld)&(wtdata[,date_time_name] %in% current_bad),target_name]<-T
    }else if((use_ots&&('ot' %in% names(current_turbine))&&any(current_turbine$ot==1,na.rm = T))||(use_alarms&&('alarm' %in% names(current_turbine))&&any(current_turbine$alarm==1,na.rm = T))){
      current_bad<-NULL
      if(use_ots) current_bad<-current_turbine[(current_turbine$ot==1),date_time_name]
      if(use_alarms) current_bad<-unique(c(current_bad,current_turbine[(current_turbine$alarm==1),date_time_name]))
      if(!is.null(current_bad)){
        bad_dates<-as.POSIXct(sapply(1:length(current_bad),function(dt) seq.POSIXt(current_bad[dt]-as.difftime(anticipation+margin-1, units="days"),by=paste0(seconds_to_aggregate,' sec'),current_bad[dt]-as.difftime(anticipation, units="days"), tz='UTC',origin='1970-01-01')),tz = 'UTC',origin='1970-01-01')
        bad_dates<-unique(bad_dates)
        wtdata[(wtdata$ld_id==ld)&(wtdata[,date_time_name] %in% bad_dates),target_name]<-T
      }
    }
  }
  rm(list=ls()[ls() %in% c('current_turbine','max_dt','bad_dates','current_bad')]) #Free space
  
  #Visualize results of pre_alarm
  # tmpwtdata<-wtdata
  # tmpwtdata$ld_id<-as.factor(tmpwtdata$ld_id)
  # tmpwtdata$status<-'none'
  # tmpwtdata$status[selected_rows]<-'selected'
  # tmpwtdata$status[tmpwtdata$ot==1]<-'ot'
  # tmpwtdata$status[tmpwtdata$pre_alarm==1]<-'pre_alarm'
  # tmpwtdata$status[tmpwtdata$pre_alarm&tmpwtdata$ot==1]<-'pre_alarm_ot'
  # tmpwtdata<-tmpwtdata[tmpwtdata$ld_id %in% selected_turbines,]
  # ggplotly(ggplot(tmpwtdata,aes(x=date_time,y=ld_id,color=status))+geom_point())
  
  #Cleanup registers with other ots
  #Target columns
  target_columns<-'pre_alarm'
  if(use_ots) target_columns<-c(target_columns,'ot')
  if(use_alarms) target_columns<-c(target_columns,'alarm')
  rs<-clean_registers_others_ot(wtdata=wtdata,seconds_ot_marging=seconds_ot_marging,selected_rows=selected_rows,date_time_name=date_time_name,target_columns=target_columns)
  selected_rows<-rs$data
  
  #If train_machines have negative sign (use only for pre_alarm==T) remove pre_alarm==F of these cases
  if(any(selected_turbines<0)){
    for(ld in selected_turbines[selected_turbines<0]){
      exclude_rows<-which((wtdata$ld_id[selected_rows]==abs(ld))&(wtdata[selected_rows,target_name]==F))
      selected_rows<-selected_rows[!(selected_rows %in% exclude_rows)]
    }
  }
  
  #Exclude register before pre alarm per turbine.
  if(!use_health&&!is.null(registers_before_pre_alarm)&&!registers_before_pre_alarm){
    aggregated_targets_column<-apply(wtdata[,target_columns,drop=F],1,function(t) any(t==1))
    for(ld in unique(wtdata$ld_id[selected_rows])){
      current_pre_alarms<-which((wtdata$ld_id==ld)&aggregated_targets_column)
      if(length(current_pre_alarms)>0){
        pre_alarm_end<-max(wtdata$date_time[current_pre_alarms],na.rm = T)
        exclude_positions<-which((wtdata$date_time<=pre_alarm_end)&(wtdata$ld_id==ld)&(wtdata[,target_name]==F))
        selected_rows<-selected_rows[!(selected_rows %in% exclude_positions)]
      }
    }
  }

  ### Data cleaning part ###
  if(!is.null(exclude_columns)&&length(exclude_columns)==1) exclude_columns<-unlist(strsplit(x = exclude_columns,','))
  
  #Backup ld_id,date_time,pre_alarm
  backup<-wtdata[,(colnames(wtdata) %in% exclude_columns),drop=F]
  if(!is.null(stat)){
    if(!is.null(selected_columns)&&any(!(selected_columns %in% stat$name)))
      return(list(error=T,warning=F,data=NULL,msg=paste0('\n',iam,': the selected columns:',paste0(selected_columns[!(selected_columns %in% stat$name)],collapse = ','),' , is not available on the normalized columns:', paste0(stat$name,collapse = ','))))
    else
      selected_columns<-as.character(stat$name)
  }else if(is.null(selected_columns)){
    selected_columns<-colnames(wtdata)[!(colnames(wtdata) %in% exclude_columns)]
  }
  wtdata<-wtdata[,colnames(wtdata) %in% selected_columns]

  #Remove columns if all are NA
  na_columns_names<-NULL
  na_columns<-((colSums(is.na(wtdata[selected_rows,colnames(wtdata) %in% selected_columns]))>=length(selected_rows))|(colSums(is.na(wtdata[selected_rows,colnames(wtdata) %in% selected_columns]))>(length(selected_rows)*na_columns_per_exclude/100)))
  if(any(na_columns)){
    na_columns_names<-colnames(wtdata[selected_rows,colnames(wtdata) %in% selected_columns])[na_columns]
    if(!is.null(selected_columns_original)&&(na_columns_names %in% selected_columns_original)) return(list(error=T,warning=F,data=NULL,msg=paste0("\n",iam,": Some of the required columns are NA:",paste0(na_columns_names[na_columns_names %in% selected_columns_original],collapse = ','))))
    selected_columns<-selected_columns[!(selected_columns %in% na_columns_names)]
    wtdata<-wtdata[,selected_columns]
  }
  
  #Complete cases
  complete_cases<-which(complete.cases(wtdata[,colnames(wtdata) %in% selected_columns]))
  selected_rows<-selected_rows[selected_rows %in% complete_cases]
  #selected_columns<-colnames(wtdata)[colnames(wtdata)!=target_name]
  
  #Cut-in
  if(!is.null(cut_in)&&!is.null(windspd_name)) selected_rows<-selected_rows[selected_rows %in% which(wtdata[,windspd_name]>=cut_in)]
  
  zero_sdv_columns_names<-NULL
  na_columns_names<-NULL
  
  #check for constants 
  sdv<-apply(wtdata[selected_rows,colnames(wtdata) %in% selected_columns],2,sd,na.rm=T)
  zero_sdv_columns<-(sdv==0)
  if(any(zero_sdv_columns)){
    zero_sdv_columns_names<-colnames(wtdata[,colnames(wtdata) %in% selected_columns])[zero_sdv_columns]
    selected_columns<-selected_columns[!(selected_columns %in% zero_sdv_columns_names)]
  }
  
  # #check of zsdv
  # if(standarization){
  #   if(is.null(stat)){
  #     #Normalize
  #     sdv<-apply(wtdata[selected_rows,colnames(wtdata) %in% selected_columns],2,sd,na.rm=T)
  #     mean<-apply(wtdata[selected_rows,colnames(wtdata) %in% selected_columns],2,mean,na.rm=T)
  #     tmp<-lapply(selected_columns,function(c) {(wtdata[,c] - mean[names(mean)==c])/sdv[names(sdv)==c]})
  #     tmp<-do.call('cbind',tmp)
  #     wtdata[,colnames(wtdata) %in% selected_columns]<-tmp
  #     stat<-lapply(names(mean),function(n) data.frame(name=n,mean=mean[names(mean)==n],sdv=sdv[names(sdv)==n]))
  #     stat<-do.call('rbind',stat)
  #   }else{
  #     wtdata[,colnames(wtdata) %in% selected_columns]<-sapply(selected_columns, function(n) {(wtdata[,n] - stat$mean[stat$name==n])/stat$sdv[stat$name==n]}) 
  #   }
  # }
  # 
  # if(normalization){
  #   if(is.null(stat)){
  #     tmp<-lapply(selected_columns,function(c) {c(min(wtdata[,c],na.rm = T),(max(wtdata[,c],na.rm = T)))})
  #     tmp<-do.call('rbind',tmp)
  #     stat<-data.frame(name=selected_columns,min=tmp[,1],max=tmp[,2],stringsAsFactors = F)
  #     tmp<-lapply(selected_columns,function(col) {(wtdata[,col] - stat$min[stat$name==col])/(stat$max[stat$name==col]-stat$min[stat$name==col])})
  #     tmp<-do.call('cbind',tmp)
  #     wtdata[,colnames(wtdata) %in% selected_columns]<-tmp
  #   }else{
  #     tmp<-lapply(selected_columns,function(col) {(wtdata[,col] - stat$min[stat$name==col])/(stat$max[stat$name==col]-stat$min[stat$name==col])})
  #     tmp<-do.call('cbind',tmp)
  #     wtdata[,colnames(wtdata) %in% selected_columns]<-tmp
  #   }
  # }
  
  #Add backup_variables
  wtdata<-cbind(wtdata,backup)
  
  return(list(error=F,warning=warning,data=list(wtdata=wtdata,normalization=normalization,standarization=standarization,selected_rows=selected_rows,selected_turbines=selected_turbines,stat=stat,zero_sdv_columns_names=zero_sdv_columns_names,na_columns_names=na_columns_names,selected_columns=selected_columns),msg=msg))
}
