#Clean all
rm(list = ls())
before_vars<-ls()

iam='main_fs'
#Dependencia basica
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
  source('functions_common/dependencyLoader.R')
}

# Sources
# devtools::install_github("rstudio/keras")
# install_keras()
libraries<-c('plotly','ggplot2','xtable','keras','tensorflow','RColorBrewer','RMySQL')
sources_common<-paste0("functions_common/",c('formatter_get_tableinfo.R','db_get_event_description.R','load_wtdata.R','close_protocol.R','db_query.R','filter_custom.R','feature_selection.R','check_acciona_rules.R','check_acciona_rules_cm3.R'))
dep<-dependencyLoader(c(libraries,sources_common))
if(dep$error)  stop(paste0("\n",iam,":on call dependencyLoader\n",dep$msg))
debug_mode<-FALSE

db_config<-if(Sys.info()['nodename']=='alexsobremesa') data.frame(user="user",password="password",dbname="yourHistoricalBD",host="yourHost",port=10003) else data.frame(user="user",password="password",dbname="yourHistoricalBD",host="127.0.0.1",port=3306)

id<-26  #id model 12 izco gen day disptemp

query<-paste0('select * from 1_cast_config_compatible where id=',id)
rs<-db_query(query=query,db_config=db_config)
if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",dep$msg))
wt_query<-rs$data

if(!is.null(wt_query$additional_model_params)) eval(parse(text = wt_query$additional_model_params[1]))

#LSTM
use_lstm<-ifelse(wt_query$type[1]=='deep_lstm',T,F)
#anticipation<-ifelse(is.null(wt_query$anticipation[1]),0,wt_query$anticipation[1])
use_alarms<-grepl(pattern = 'alarm',x = wt_query$target_name[1])
use_ots<-grepl(pattern = 'ot',x = wt_query$target_name[1])
use_health<-grepl(pattern = 'health',x = wt_query$target_name[1])
date_time_name<-'date_time'
include_variables<-wt_query$include_variables[1]
if(!exists('tmpfolder',inherits = F)) tmpfolder<-'tmp_fs'

ifelse(!dir.exists(tmpfolder), dir.create(tmpfolder), FALSE)

#anticipation<-ifelse(is.null(wt_query$anticipation[1]),0,wt_query$anticipation[1])
rs  <-  load_wtdata(wt_query=wt_query,
                    date_time_name=date_time_name,
                    target_name=NULL,
                    filter_exclude=paste(date_time_name,"nrecords,ot_block_code,ot_all,ot_all_block_code,production,weekly_production,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                    update_filter_ranges=F,
                    filter_verbose=F,
                    db_config=db_config,
                    parallel=T)
if(rs$error) stop(paste0("\n",iam,":on call load_wtdata\n\t",rs$msg))

wtdata<-rs$data$wtdata
outliers<-rs$data$outliers
rm(rs)
target_name<-wt_query$target_name[1]

#OT block code code to string
if('ot_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_dic_name<-rs$data$ot_table_dic_name
  if(!is.null(ot_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_dic_name,all_info=TRUE, target = "ot", db_config=db_config)
    ot_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$ot_block_code<-gsub(wtdata$ot_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
    }
  }
}

if('ot_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_all_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_dic_name<-rs$data$ot_table_dic_name
  if(!is.null(ot_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_dic_name,all_info=TRUE, target = "ot", db_config=db_config)
    ot_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$ot_all_block_code<-gsub(wtdata$ot_all_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
    }
  }
}

#alarm block code to string
if('alarm_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarms_table_dic_name<-rs$data$alarms_table_dic_name
  if(!is.null(alarms_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), alarms_table_dic_name,all_info=TRUE, target = "alarm", db_config=db_config)
    alarm_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$alarm_block_code<-gsub(wtdata$alarm_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_walm])
    }
  }
}

#alarm block code to string
if('alarm_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_all_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarms_table_dic_name<-rs$data$alarms_table_dic_name
  if(!is.null(alarms_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), alarms_table_dic_name,all_info=TRUE, target = "alarm", db_config=db_config)
    alarm_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$alarm_all_block_code<-gsub(wtdata$alarm_all_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_walm])
    }
  }
}

if(scale_reference_ld){
  #Scale versus reference and move min values to offset 0.
  scale_excluded_columns<-c('ld_id','date_time','ot','alarm','ot_all','alarm_all','ot_block_code','alarm_block_code','ot_all_block_code','alarm_all_block_code')
  reference_ld_id<-abs(as.numeric(unlist(strsplit(wt_query$train_machines[1],split = ','))[1]))
  others_ld_id<-unique(wtdata$ld_id[wtdata$ld_id!=reference_ld_id])
  scale_factors<-data.frame(ld_id=as.numeric(),variable=as.character(),offset=as.numeric(),min=as.numeric(),max=as.numeric())
  for(c in 1:ncol(wtdata)){
    if(!(colnames(wtdata)[c] %in% scale_excluded_columns)&&is.numeric(wtdata[,c])){
      min_ref<-min(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      max_ref<-max(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      original_min_ref<-min_ref
      original_max_ref<-max_ref
      offset_ref<-ifelse(min_ref<0,abs(min_ref),-1*min_ref)
      scale_factors<-rbind(scale_factors,data.frame(reference_ld_id=reference_ld_id,ld_id=reference_ld_id,variable=colnames(wtdata)[c],offset=offset_ref,min=min_ref,max=max_ref))
      wtdata[wtdata$ld_id==reference_ld_id,c]<-wtdata[wtdata$ld_id==reference_ld_id,c]+offset_ref
      min_ref<-min(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      max_ref<-max(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      for(ld in others_ld_id){
        min_cur<-min(wtdata[wtdata$ld_id==ld,c],na.rm = T)
        max_cur<-max(wtdata[wtdata$ld_id==ld,c],na.rm = T)
        offset_cur<-ifelse(min_cur<0,abs(min_cur),-1*min_cur)
        scale_factors<-rbind(scale_factors,data.frame(reference_ld_id=reference_ld_id,ld_id=ld,variable=colnames(wtdata)[c],offset=offset_cur,min=min_cur,max=max_cur))
        wtdata[wtdata$ld_id==ld,c]<-wtdata[wtdata$ld_id==ld,c]+offset_cur
        wtdata[wtdata$ld_id==ld,c]<-((((wtdata[wtdata$ld_id==ld,c]*100)/max_cur)*max_ref)/100)
      }
    }
  }
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
#to_save<-c('wtdata','wt_query','outliers','date_time_name','query','scale_reference_ld','scale_factors','anticipation','use_alarms','use_ots','use_health','tmpfolder','target_name')
save(list=to_save,file=paste0(tmpfolder,'/wtdata_',id,'_',wt_query$wp_code[1],'_',wt_query$seconds_to_aggregate[1],'s_ant',anticipation,ifelse(use_health,'_health',''),ifelse(use_alarms,'_alarms',''),ifelse(use_ots,'_ots',''),'.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

############################## Train Test Separation ###################################
#Por temperatura izco
#Grupo1 nov,dic,Enero,febrero,marzo,abril,mayo Grupo 2 Junio,julio,agosto,sept,oct
#wtdata2<-wtdata[,'date_time',drop=F] %>% mutate(month = format(date_time,"%m"))
#wtdata2$month<-as.numeric(wtdata2$month)
#wtdata_inv<-wtdata[wtdata2$month %in% c(1,2,3,4,5,11,12),]
setwd("~/Dropbox/phd/experimentos/deep_learning")
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs_moncaymulti_rbm'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
target_name<-tmp_env$target_name
anticipation<-tmp_env$anticipation
margin<-if(is.null(tmp_env$margin)) 30 else tmp_env$margin
use_alarms<-tmp_env$use_alarms
use_ots<-tmp_env$use_ots
use_health<-tmp_env$use_health
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
use_lstm<-tmp_env$use_lstm
include_variables<-tmp_env$include_variables
if(use_health){
  train_machines<-unique(wtdata$ld_id[!is.na(wtdata$health_status)])
}else if(!is.null(tmp_env$wt_query$train_machines[1]) && grepl(pattern = '[-]?[0-9]+,',x = tmp_env$wt_query$train_machines[1])){
  train_machines<-as.numeric(unlist(strsplit(tmp_env$wt_query$train_machines[1],split = ',')))
}else{
  train_machines<-tmp_env$wt_query$train_machines[1]
}
balance<-if(exists('balance')) tmp_env$balance else tmp_env$balance_per
balance_by<-tmp_env$balance_by
wp_id<-tmp_env$wt_query$wp_id[1]
db_config<-tmp_env$db_config
rm(tmp_env)
gc(verbose = F)

#train_machines<-50 #50% for train 50% for test.
#train_machines<-abs(train_machines)
#db_config$host<-'yourHost'
#db_config$port<-10003

#Get windspd name
if(!exists('formatter_get_tableinfo')) source('functions_common/formatter_get_tableinfo.R')
rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wp_id,db_config=db_config)
if(rs$error) stop(paste0('error in formatter_get_tableinfo:',rs$msg))
wind_variable_name<-rs$data$wind_variable_name
cut_in<-4
#max_date_time<-as.POSIXct('2016-12-01',tz='UTC')
max_date_time<-NULL
if(!exists('preprocess_data')) source('functions_deep/preprocess_data.R')
rs<-preprocess_data(wtdata=wtdata,selected_turbines=train_machines,
                    target_name='pre_alarm',use_alarms=use_alarms,
                    use_ots=use_ots,use_health=use_health,anticipation=anticipation,
                    margin=margin,seconds_to_aggregate=seconds_to_aggregate,
                    marging_after_pre_alarm=60*60*24*7,
                    seconds_ot_marging=60*60*24,
                    registers_before_pre_alarm=,
                    na_columns_per_exclude=10,
                    cut_in=cut_in,
                    max_date_time=max_date_time,
                    windspd_name=wind_variable_name)
if(rs$error) stop(paste0('error in preprocess_data: ',rs$msg))
if(rs$warning) warning(paste0('---warning in preprocess_data: ',rs$msg))  
wtdata<-rs$data$wtdata
selected_rows<-rs$data$selected_rows
train_machines<-rs$data$selected_turbines
zero_sdv_columns_names<-rs$data$zero_sdv_columns_names
na_columns_names<-rs$data$na_columns_names
selected_columns<-rs$data$selected_columns
exclude_columns<-c('date_time','pre_alarm_0_anti','pre_alarm','ld_id','health_status','alarm','alarm_block_code','alarm_all','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code','alarm_block_code','alarm_all_block_code')
target_name<-'pre_alarm'
### Code for debug selected.
# wtdata$pre_alarm<-F
# for(ld in unique(pre_alarm$ld_id)){
#   dates<-pre_alarm$date_time[pre_alarm$ld_id==ld&pre_alarm$pre_alarm==1]
#   if(length(dates)>0){
#     wtdata$pre_alarm[(wtdata$ld_id==ld)&(wtdata$date_time %in% dates)]<-T
#   }
# }
# wtdata$selected<-F
# for(s in rs$data$selected_rows){
#   wtdata$selected[(wtdata$date_time == rs$data$wtdata$date_time[s])&(wtdata$ld_id==rs$data$wtdata$ld_id[s])]<-T
# }
# wtdata$status<-'no_selected'
# wtdata$status[(wtdata$selected==T)]<-'selected'
# wtdata$status[(wtdata$pre_alarm==T)]<-'pre_alarm'
# wtdata$status[(wtdata$ot==T)]<-paste0(wtdata$status[(wtdata$ot==T)],'_ot')
# df<-wtdata[,c('date_time','ld_id','status')]
# df<-df[df$ld_id %in% abs(train_machines),]
# df$ld_id<-as.factor(df$ld_id)
# ggplot(df,aes(x=date_time,y=ld_id,color=status))+geom_point()
# ggplotly(ggplot(df,aes(x=date_time,y=ld_id,color=status))+geom_point())

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(tmpfolder,'/prepare_train.RData'),compress = 'xz')
rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)

############################################ Feature selection #################################################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/prepare_train.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
selected_rows<-tmp_env$selected_rows
selected_columns<-tmp_env$selected_columns
#target_name<-if('target_name' %in% names(tmp_env)) tmp_env$target_name else 'pre_alarm'
target_name<-'pre_alarm'
anticipation<-tmp_env$anticipation
exclude_columns<-tmp_env$exclude_columns
rm(tmp_env)
gc(verbose = F)

#Src,libs
source('functions_common/feature_selection.R', echo=TRUE)

fs_params = list()
fs_params = rbind(fs_params,list(algorithm='pvalue',na_columns_per_exclude=10,complete_cases=F,params=data.frame(method="simulation",nsim=1000,pvalue=0.05,stringsAsFactors = F),cor_threshold=0.95,discretize=F,standarize=F))
fs_params = rbind(fs_params,list(algorithm='caretrfe',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.10),cor_threshold=0.95,discretize=F,standarize=T))
fs_params = rbind(fs_params,list(algorithm='boruta',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25,maxRuns=50,field='medianImp'),cor_threshold=0.95,discretize=F,standarize=T))
fs_params = rbind(fs_params,list(algorithm='cmim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='disr',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='jmi',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='jmim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='mim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='mrmr',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_params = rbind(fs_params,list(algorithm='njmim',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95,discretize=T,standarize=T))
fs_results<-list()
for(i in 1:nrow(fs_params)){
  cat(paste0('Algoritm ',i,' of ',nrow(fs_params),' :',fs_params[i,]$algorithm,'\n'))
  #Remove all data with ot_all=1 for train
  rs<-feature_selection(wtdata=wtdata[selected_rows,],
                        exclude_columns=exclude_columns,
                        target_name=target_name,
                        algorithm=fs_params[i,]$algorithm,
                        date_time_name='date_time',
                        cor_threshold=fs_params[i,]$cor_threshold,
                        na_columns_per_exclude=fs_params[i,]$na_columns_per_exclude,
                        complete_cases=fs_params[i,]$complete_cases,
                        params=fs_params[i,]$params,
                        discretize=fs_params[i,]$discretize,
                        parallel=T,
                        standarize = fs_params[i,]$standarize,
                        logfile=NULL)
  if(rs$error) stop(paste0("\non call feature_selection\n\t",rs$msg))
  tmp<-rs$data
  tmp$wtdata<-NULL #Free space
  tmp$algorithm<-fs_params[[i,'algorithm']]
  tmp$params<-fs_params[[i,'params']]
  tmp$complete_cases<-fs_params[[i,'complete_cases']]
  tmp$na_columns_per_exclude<-fs_params[[i,'na_columns_per_exclude']]
  tmp$cor_threshold<-fs_params[[i,'cor_threshold']]
  fs_results<-rbind(fs_results,tmp)
  rm(rs)
  gc(verbose = F)
}

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

##ALL the scores
#Compare selections
scores<-NULL
x_cut<-NULL
x<-NULL
for(i in 1:nrow(fs_results)){
  tmp<-fs_results[[i,'scores_variables']]
  algo_name<-fs_results[[i,'algorithm']]
  if(algo_name=='pvalue'){
    value<-1-(round((max(tmp$score[tmp$selected],na.rm = T)-min(tmp$score,na.rm = T))/(max(tmp$score,na.rm = T)-min(tmp$score,na.rm = T)),digits = 3))
  }else{
    value<-round((min(tmp$score[tmp$selected],na.rm = T)-min(tmp$score,na.rm = T))/(max(tmp$score,na.rm = T)-min(tmp$score,na.rm = T)),digits = 3)
  }
  x_cut<-c(x_cut,paste0(algo_name,'>=',value))
  x<-c(x,algo_name)
  
  if(!is.null(scores)){
    scores<-merge(scores,tmp[,c('name','score')],by = 'name',all=T)
    colnames(scores)<-c('name',unlist(fs_results[,'algorithm'])[1:(ncol(scores)-1)])
  }else{
    scores<-tmp[order(tmp$name),c('name','score')]
    colnames(scores)[2]<-algo_name
  }
  if(length(unique(scores$name))!=length(scores$name)) cat('\n i:',i,' duplicated name \n')
}

#Normalize scores
norm_scores<-scores
norm_scores[,2:(nrow(fs_results)+1)]<-apply(norm_scores[,2:(nrow(fs_results)+1),drop=F],2,function(c) (c-min(c,na.rm = T))/(max(c,na.rm = T)-min(c,na.rm = T)))
pvalue_pos<-which(unlist(fs_results[,'algorithm'])=='pvalue')
if(length(pvalue_pos)>0){#Invert Pvalue
  norm_scores[,pvalue_pos+1]<-(1-norm_scores[,pvalue_pos+1])
}
rownames(norm_scores)<-norm_scores[,1]
norm_scores[,1]<-NULL
p<-plot_ly(x=x_cut,y=rownames(norm_scores),z=round(as.matrix(norm_scores),digits = 3),type='heatmap')%>% layout(title = 'All variables', xaxis = list(title='Algorithm and cut value'),yaxis = list(title='Variable'), margin=list(l=150))
htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/fs_comparison_allscores_a',anticipation,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)


#Publicacion
# norm_scores<-norm_scores[!is.na(norm_scores$pvalue),]
# df<-expand.grid(algo=colnames(norm_scores),variable=rownames(norm_scores))
# df$value<-NA
# for(algo in unique(df$algo) ){
#   for(variable in unique(df$variable) ){
#     df$value[(df$algo==algo)&(df$variable==variable)]<-norm_scores[rownames(norm_scores)==variable,algo]
#   }  
# }
# p<-ggplot(data = df, aes(x = variable, y = algo))+geom_tile(aes(fill = value))+xlab('')+ylab('')+theme_bw()+ theme(legend.position="none",axis.text.x = element_text(angle = 90, hjust = 1),axis.text.y=element_text(size=12))
# ggsave(filename = 'test.pdf',plot = p,width = 15,height = 9)
#End publicacion

## Consensus
#Sum normalized scores
scores_names<-unique(scores$name)
scores_values<-lapply(scores_names,function(var){
  df<-data.frame(var=var,method="count",value=sum(as.numeric(norm_scores[rownames(norm_scores)==var,]),na.rm = T))
  df<-rbind(df,data.frame(var=var,method="mean",value=mean(as.numeric(norm_scores[rownames(norm_scores)==var,]),na.rm = T)))
  df<-rbind(df,data.frame(var=var,method="median",value=median(as.numeric(norm_scores[rownames(norm_scores)==var,]),na.rm = T)))
  return(df)
})
scores_values<-do.call("rbind",scores_values)

#Normalize scores per type
current<-scores_values$value[scores_values$method=='count']
scores_values$value[scores_values$method=='count']<-(current-min(current,na.rm = T))/(max(current,na.rm = T)-min(current,na.rm = T))
current<-scores_values$value[scores_values$method=='mean']
scores_values$value[scores_values$method=='mean']<-(current-min(current,na.rm = T))/(max(current,na.rm = T)-min(current,na.rm = T))
current<-scores_values$value[scores_values$method=='median']
scores_values$value[scores_values$method=='median']<-(current-min(current,na.rm = T))/(max(current,na.rm = T)-min(current,na.rm = T))

#Select by median
sel<-scores_values[scores_values$method=='median',]
sel<-sel[order(sel$value,decreasing = T),]
q<-0.25
v<-quantile(sel$value,probs=(1-q),na.rm=TRUE)
selected_columns_consensus<-as.character(sel$var[(sel$value>=v)&(!is.na(sel$value))])
## End consensus

#Only selected
#Compare selections
scores<-NULL
x_cut<-NULL
x<-NULL
for(i in 1:nrow(fs_results)){
  tmp<-fs_results[[i,'scores_variables']]
  algo_name<-fs_results[[i,'algorithm']]
  tmp_scores<-tmp$score[tmp$selected]
  x<-c(x,algo_name)
  
  if(!is.null(scores)){
    scores<-merge(scores,tmp[tmp$selected,c('name','score')],by = 'name',all=T)
    colnames(scores)<-c('name',unlist(fs_results[,'algorithm'])[1:(ncol(scores)-1)])
  }else{
    scores<-tmp[tmp$selected,c('name','score')]
    scores<-scores[order(scores$name),]
  }
}
#Normalize scores
norm_scores<-scores
norm_scores[,2:(nrow(fs_results)+1)]<-apply(norm_scores[,2:(nrow(fs_results)+1),drop=F],2,function(c) (c-min(c,na.rm = T))/(max(c,na.rm = T)-min(c,na.rm = T)))
pvalue_pos<-which(unlist(fs_results[,'algorithm'])=='pvalue')
if(length(pvalue_pos+1)>0){#Invert Pvalue
  norm_scores[,pvalue_pos+1]<-(1-norm_scores[,pvalue_pos+1])
}
rownames(norm_scores)<-norm_scores[,1]
norm_scores[,1]<-NULL
p<-plot_ly(x=x,y=rownames(norm_scores),z=round(as.matrix(norm_scores),digits = 3),type='heatmap') %>% layout(title = 'Selected variables', xaxis = list(title='Algorithm'),yaxis = list(title='Variable'), margin=list(l=150)) 
htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/fs_comparison_selectedscores_a',anticipation,'.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)

#Save until now to save memory space
save(list=c('fs_results','fs_params','selected_columns_consensus'),file=paste0(tmpfolder,'/after_feature.RData'),compress = 'xz')
rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)

################################### Balance dataset #########################################

if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/after_feature.RData') ,envir = tmp_env)
fs_results<-tmp_env$fs_results

algo<-which(unlist(fs_results[,'algorithm'])=='boruta')
fs_algo<-fs_results[[algo,'algorithm']]
selected_columns<-fs_results[[algo,'selected']]
# if('selected_columns_consensus' %in% names(tmp_env)){
#   selected_columns<-as.character(tmp_env$selected_columns_consensus)
#   algo<-'consensus'
# }else{
#   algo<-which(unlist(fs_results[,'algorithm'])=='cmim')
#   fs_algo<-fs_results[[algo,'algorithm']]
#   selected_columns<-fs_results[[algo,'selected']]
# }
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/prepare_train.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
selected_rows<-tmp_env$selected_rows
target_name<-if('target_name' %in% colnames(tmp_env)) tmp_env$target_name else 'pre_alarm'
anticipation<-tmp_env$anticipation
margin<-tmp_env$margin
seconds_to_aggregate<-tmp_env$seconds_to_aggregate
balance<-tmp_env$balance
balance_by<-tmp_env$balance_by
if(!is.null(tmp_env$include_variables)&&!is.na(tmp_env$include_variables)){
  fs_algo<-'forced_byinclude'
  selected_columns<-unlist(strsplit(tmp_env$include_variables,split = ','))
  selected_columns<-selected_columns[!selected_columns %in% tmp_env$exclude_columns]
}
rm(tmp_env)
gc(verbose = F)

#balance<-30
#balance_by<-'tomek_smote'
#balance_by<-'rbm'
#Src,libs
if(!exists("balance_dataset",inherits = F)) source('functions_common/balance_dataset.R')
rs<-balance_dataset(lstm=F,
                    wtdata=wtdata[,c(selected_columns,'ld_id','date_time',target_name)],
                    selected_rows=selected_rows,
                    target_name=target_name,
                    seconds_to_aggregate=seconds_to_aggregate,
                    balance=balance,
                    balance_by=balance_by,
                    select_good_same_bad_interval=T,
                    select_all_goods_after_bad=F)
if(rs$error) stop('error in preprocess_data')  
wtdata<-rs$data$wtdata
selected_rows<-rs$data$selected_rows
real_balance<-rs$data$real_balance

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
save(list=to_save,file=paste0(tmpfolder,'/balanced_data.RData'),compress = 'xz')
rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)

################################### PCA ####################################################
# if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs_moncaymulti'
# before_vars<-ls()
# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/after_feature.RData') ,envir = tmp_env)
# fs_results<-tmp_env$fs_results
# if('selected_columns_consensus' %in% names(tmp_env)){
#   selected_columns<-as.character(tmp_env$selected_columns_consensus)
# }else{
#   algo<-which(unlist(fs_results[,'algorithm'])=='cmim')
#   fs_algo<-fs_results[[algo,'algorithm']]
#   selected_columns<-fs_results[[algo,'selected']]
# }
# rm(tmp_env)
# 
# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/balanced_data.RData') ,envir = tmp_env)
# target_name<-if('target_name' %in% colnames(tmp_env)) tmp_env$target_name else 'pre_alarm'
# train<-tmp_env$wtdata
# selected_rows<-tmp_env$selected_rows
# balance<-tmp_env$balance
# real_balance<-round(tmp_env$real_balance,digits = 0)
# margin<-tmp_env$margin
# anticipation<-tmp_env$anticipation
# rm(tmp_env)
# 
# pr<-prcomp(train[selected_rows,selected_columns],center = T,scale. = T)
# train[selected_rows,selected_columns]<-pr$x
# var<-pr$sdev^2
# pve<-var/sum(var)
# first_pc<-which(cumsum(pve)>0.7)[1]
# loadings <- eigen(cov(train[selected_rows,selected_columns]))$vectors
# explvar <- loadings^2
# var<NULL
# for(p in 1:first_pc){
#   var<-c(var,which(explvar[,p]>0.05))
# }
# selected_columns[unique(var)]
# to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
# to_save<-to_save[!(to_save %in% before_vars)]
# save(list=to_save,file=paste0(tmpfolder,'/pca_data.RData'),compress = 'xz')
# rm(list=ls()[!(ls() %in% before_vars)])
# gc(verbose = F)

################################### SOM ####################################################

# before_vars<-ls()
# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/after_feature.RData') ,envir = tmp_env)
# fs_results<-tmp_env$fs_results
# if('selected_columns_consensus' %in% names(tmp_env)){
#   selected_columns<-as.character(tmp_env$selected_columns_consensus)
# }else{
#   algo<-which(unlist(fs_results[,'algorithm'])=='cmim')
#   fs_algo<-fs_results[[algo,'algorithm']]
#   selected_columns<-fs_results[[algo,'selected']]
# }
# rm(tmp_env)
# 
# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/balanced_data.RData') ,envir = tmp_env)
# target_name<-if('target_name' %in% colnames(tmp_env)) tmp_env$target_name else 'pre_alarm'
# train<-tmp_env$wtdata
# selected_rows<-tmp_env$selected_rows
# balance<-tmp_env$balance
# real_balance<-round(tmp_env$real_balance,digits = 0)
# margin<-tmp_env$margin
# anticipation<-tmp_env$anticipation
# normalize<-!((!is.null(tmp_env$normalization)&&tmp_env$normalization)||(!is.null(tmp_env$standarization)&&tmp_env$standarization))
# normalization<-tmp_env$normalization
# standarization<-tmp_env$standarization
# rm(tmp_env)
# 
# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/prepare_train.RData') ,envir = tmp_env)
# stat<-tmp_env$stat
# rm(tmp_env)
# 
# 
# dim<-round(sqrt(5*sqrt(length(selected_rows))),0)
# som_grid <- kohonen::somgrid(xdim = dim, ydim=dim, topo="rectangular")
# model <- kohonen::som(as.matrix(train[selected_rows,selected_columns]), grid=som_grid, alpha=c(0.05,0.01),rlen=10, keep.data = T)
# wtdata_bmu=model$unit.classif
# som_distances=model$distances
# som_codes=model$codes[[1]]
# som_pts<-model$grid$pts
# clusters<-hclust(dist(som_codes))
# som_cluster <- cutree(clusters, 5)
# wtdata_cluster<-som_cluster[wtdata_bmu]
# mean_sc<-mean(wtdata_cluster,na.rm = T)
# sdv_sc<-sd(wtdata_cluster)
# train$somcluster<-NA
# train$somcluster[selected_rows]<-(wtdata_cluster-mean_sc)/sdv_sc

#selected_columns<-c('VelViento_avg','TempAmb_max','TempCojLOA_avg')
#train[,colnames(train) %in% selected_columns]<-sapply(selected_columns, function(n) {((train[,n]*stat$sdv[stat$name==n])+stat$mean[stat$name==n])})
#rs<-plot_histogram_som_clusters(wtdata=train,som_cluster=som_cluster,wtdata_bmu=wtdata_bmu,som_selected_rows=selected_rows,som_selected_columns=selected_columns,date_time_name='date_time',grid_format=T)
#ggsave(filename =paste0('histogram_clusters.png'),width = 30,height = 40,plot = rs$data$plot)

# 
# to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
# to_save<-to_save[!(to_save %in% before_vars)]
# save(list=to_save,file=paste0(tmpfolder,'/som_data.RData'),compress = 'xz')
# rm(list=ls()[!(ls() %in% before_vars)])
# gc(verbose = F)


################################### CLASS benchmark #########################################
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs_moncaymulti'
before_vars<-ls()
use_gpu<-F
if((Sys.info()["nodename"] %in% c("PORTATILALE"))) use_gpu<-T
if(!exists("preprocess_data",inherits = F)) source('functions_deep/preprocess_data.R')
if(!exists("deep_ann",inherits = F)) source('functions_deep/deep_ann.R')
if(!exists("model_indicators")) source('functions_deep/model_indicators.R')
if(!exists("confusion_matrix")) source('functions_deep/confusion_matrix.R')
if(!exists("check_acciona_rules")) source('functions_common/check_acciona_rules.R')
if(!exists("check_acciona_rules_cm3")) source('functions_common/check_acciona_rules_cm3.R')

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/prepare_train.RData') ,envir = tmp_env)
train_machines<-tmp_env$train_machines
cut_in<-tmp_env$cut_in
wind_variable_name<-tmp_env$wind_variable_name
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/balanced_data.RData') ,envir = tmp_env)
target_name<-if('target_name' %in% colnames(tmp_env)) tmp_env$target_name else 'pre_alarm'
train<-tmp_env$wtdata
train_selected_rows<-tmp_env$selected_rows
tmp_sel<-((1:nrow(train) %in% train_selected_rows))
ld_id_selected_rows<-which(train$ld_id %in% unique(train$ld_id[train_selected_rows]))
tmp_sel<-tmp_sel[ld_id_selected_rows]
train<-train[ld_id_selected_rows,]
train_selected_rows<-which(tmp_sel)
balance<-tmp_env$balance
real_balance<-round(tmp_env$real_balance,digits = 0)
margin<-tmp_env$margin
anticipation<-tmp_env$anticipation
selected_columns<-tmp_env$selected_columns
fs_algo<-tmp_env$fs_algo
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
use_health<-tmp_env$use_health
use_lstm<-tmp_env$use_lstm
use_ots<-tmp_env$use_ots
use_alarms<-tmp_env$use_alarms
anticipation<-if('anticipation' %in% colnames(tmp_env)||is.null(tmp_env$anticipation)) 15 else tmp_env$anticipation
margin<-if('margin' %in% colnames(tmp_env)||is.null(tmp_env$margin)) 30 else tmp_env$margin
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
rm(tmp_env)
gc(verbose = F)

# tmp_env<-new.env()
# load(file =paste0(tmpfolder,'/pca_data.RData') ,envir = tmp_env)
# pr<-tmp_env$pr
# train<-tmp_env$train
# pca<-tmp_env$pca
# rm(tmp_env)

#tmp_env<-new.env() 
#load(file =paste0(tmpfolder,'/som_data.RData') ,envir = tmp_env)
#train<-tmp_env$train
#wtdata_cluster<-tmp_env$wtdata_cluster
#som_cluster<-tmp_env$som_cluster
#som_model<-tmp_env$model
#mean_sc<-tmp_env$mean_sc
#sdv_sc<-tmp_env$sdv_sc
#rm(tmp_env)

#Prepare traintest data
rs<-preprocess_data(wtdata=wtdata,selected_turbines=train_machines,
                    selected_columns=selected_columns,
                    target_name=target_name,use_alarms=use_alarms,
                    use_ots=use_ots,use_health=use_health,anticipation=anticipation,
                    margin=margin,seconds_to_aggregate=seconds_to_aggregate,
                    registers_before_pre_alarm=T,cut_in=NULL,windspd_name=NULL,
                    marging_after_pre_alarm=NULL,seconds_ot_marging=NULL)
if(rs$error) stop(rs$msg)
train_test<-rs$data$wtdata
train_test_selected_rows<-rs$data$selected_rows

### PCA ###
if(exists('pca')&&pca){
  train_test[train_test_selected_rows,selected_columns]<-scale(train_test[train_test_selected_rows,selected_columns],pr$center,pr$scale) %*% pr$rotation
}

if(exists('som_model')){
  rs<-kohonen::map(som_model,train_test[train_test_selected_rows,selected_columns])
  wtdata_cluster<-som_cluster[rs$unit.classif]
  wtdata_cluster<-(wtdata_cluster-mean_sc)/sdv_sc
  train_test$somcluster<-NA
  train_test$somcluster[train_test_selected_rows]<-wtdata_cluster
}

rs<-preprocess_data(wtdata=wtdata,selected_turbines=unique(wtdata$ld_id[!(wtdata$ld_id %in% abs(train_machines))]),
                    selected_columns=selected_columns,
                    target_name=target_name,use_alarms=use_alarms,
                    use_ots=T,use_health=F,anticipation=anticipation,
                    margin=margin,seconds_to_aggregate=seconds_to_aggregate,
                    registers_before_pre_alarm=T,cut_in=NULL,windspd_name=NULL,
                    marging_after_pre_alarm=NULL,seconds_ot_marging=NULL)
if(rs$error) stop(rs$msg)
test<-rs$data$wtdata
test_selected_rows<-rs$data$selected_rows

### PCA ###
if(exists('pca')&&pca){
  test[test_selected_rows,selected_columns]<-scale(test[test_selected_rows,selected_columns],pr$center,pr$scale) %*% pr$rotation
}

if(exists('som_model')){
  rs<-kohonen::map(som_model,test[test_selected_rows,selected_columns])
  wtdata_cluster<-som_cluster[rs$unit.classif]
  test$somcluster<-NA
  wtdata_cluster<-(wtdata_cluster-mean_sc)/sdv_sc
  test$somcluster[test_selected_rows]<-wtdata_cluster
  selected_columns<-c(selected_columns,'somcluster')
}

class_results<-list()
cm_bench_train<-list()
cm_bench_test<-list()

target_ratio<-floor((length(train_selected_rows)/sum(train$pre_alarm[train_selected_rows]==T)))
#target_ratio<-NULL

#batch_size<-max(floor(nrow(train)*0.01),32)
batch_size<-round(length(train_selected_rows)/2,0)
epochs<-5000
lr = 0.001

wtdata_train<-NULL
wtdata_test<-NULL
class_results<-NULL
i<-1

#X_val<-NULL
#y_val<-NULL
selected_rows_val<-NULL

#To validate with test lds (for check the real generalization power uncomment and configure following lines)
#moncay
#validation_lds<-which(test$ld_id %in% c(139,152,166,143,148,157))
#Izco
#validation_lds<-which(test$ld_id %in% #c(206,209,210,216,193,195)) 

#X_val<-test[validation_lds,]
#y_val<-test$pre_alarm[validation_lds]
#selected_rows_val<-test_selected_rows[test_selected_rows %in% validation_lds]

if(!use_lstm){
  if(use_gpu){
    epochs=60000
    architecture_bench<-list(lr = lr,architecture=c(5,5,5,5,1),activation='relu',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio,recurrent_dropout=NULL,timesteps=NULL)
    architecture_bench<-rbind(architecture_bench,list(lr = lr,architecture=c(5,5,5,5,1),activation='relu',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio/2,recurrent_dropout=NULL,timesteps=NULL))
    
    #architecture_bench<-list(lr = lr,architecture=c(50,50,50,1),activation='tanh',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio,recurrent_dropout=NULL,timesteps=NULL)
    #architecture_bench<-rbind(architecture_bench,list(lr = lr,architecture=c(50,50,50,1),activation='sigmoid',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio,recurrent_dropout=NULL,timesteps=NULL))
  }
  architecture_bench<-list(lr = lr,architecture=c(20,20,20,1),activation='sigmoid',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio,recurrent_dropout=NULL,timesteps=NULL)
  architecture_bench<-rbind(architecture_bench,list(lr = lr,architecture=c(20,20,20,1),activation='tanh',epochs=epochs,batch_size=batch_size,dropout=0.2,target_ratio=target_ratio,recurrent_dropout=NULL,timesteps=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='relu',epochs=1600,dropout=0.2,target_ratio=1))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='sigmoid',epochs=1600,batch_size=32,dropout=0.7,timesteps=NULL,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='sigmoid',epochs=1600,batch_size=32,dropout=0.7,timesteps=NULL,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='sigmoid',epochs=1600,dropout=0.5,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='relu',epochs=1600,batch_size=32,dropout=0.7,timesteps=NULL,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='relu',epochs=1600,batch_size=32,dropout=0.7,timesteps=NULL,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(10,8,1),activation='relu',epochs=1600,dropout=0.5,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='sigmoid',epochs=1600,dropout=0.2,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='sigmoid',epochs=1600,dropout=0.2,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='relu',epochs=1600,dropout=0.2,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='relu',epochs=1600,dropout=0.2,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='sigmoid',epochs=1600,dropout=0.5,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='sigmoid',epochs=1600,dropout=0.5,target_ratio=target_ratio))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='relu',epochs=1600,dropout=0.5,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(20,10,8,1),activation='relu',epochs=1600,dropout=0.5,target_ratio=target_ratio))
  architecture_bench<-rbind(architecture_bench,list(architecture=c(50,30,20,10,8,1),activation='relu',epochs=30000,batch_size=32,dropout=0.5,target_ratio=NULL,recurrent_dropout=NULL,timesteps=NULL))
  
  #architecture_bench<-list(architecture=c(50,30,20,10,8,1),activation='sigmoid',epochs=1600,batch_size=3000,dropout=0.5,target_ratio=5)
  
  architecture_bench<-rbind(architecture_bench,list(architecture=c(50,30,20,10,8,1),activation='sigmoid',epochs=30000,batch_size=32,dropout=0.5,target_ratio=NULL,recurrent_dropout=NULL,timesteps=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(50,30,20,10,8,1),activation='relu',epochs=35000,batch_size=length(train_selected_rows),dropout=0.7,target_ratio=NULL))
  #architecture_bench<-rbind(architecture_bench,list(architecture=c(50,30,20,10,8,1),activation='relu',epochs=35000,batch_size=length(train_selected_rows),dropout=0.7,target_ratio=target_ratio))
  
}else{ #LSTM
  architecture_bench<-list(lr=lr,architecture=c(50,50,50,1),activation='tanh',epochs=epochs,batch_size=batch_size,dropout=0.2,recurrent_dropout=0.2,timesteps=90,target_ratio=target_ratio)
  architecture_bench<-rbind(architecture_bench,list(lr=lr,architecture=c(50,50,50,1),activation='tanh',epochs=epochs,batch_size=batch_size,dropout=0.2,recurrent_dropout=0.2,timesteps=90,target_ratio=target_ratio/2))
}

for(i in 1:nrow(architecture_bench)){
  rs<-deep_ann(lstm=use_lstm,
               lr=architecture_bench[[i,'lr']],
               batch_size=architecture_bench[[i,'batch_size']],
               X=train,
               selected_rows=train_selected_rows,
               selected_columns=selected_columns,
               y=train$pre_alarm,
               X_val=X_val,
               y_val=y_val,
               selected_rows_val=selected_rows_val,
               epochs=architecture_bench[[i,'epochs']],
               architecture=architecture_bench[[i,'architecture']],
               activation=architecture_bench[[i,'activation']],
               dropout=architecture_bench[[i,'dropout']],
               input_dropout=0.2,
               recurrent_dropout=architecture_bench[[i,'recurrent_dropout']],
               target_ratio=architecture_bench[[i,'target_ratio']],
               timesteps=architecture_bench[[i,'timesteps']],
               view_metrics=F,view_progress_bar=F,use_gpu=use_gpu,inpute=T,testing=F,tmpfolder=tmpfolder,verbose=T)
  if(rs$error) stop('error:',rs$msg)
  model<-rs$data
  target_ratio<-model$target_ratio
  #Predict all the data of train
  rs<-model$predict(model=model,X=train_test,selected_rows=train_test_selected_rows,normalize = F,inpute=F,verbose=T)
  pred_train<-rs$data$yhat
  if(is.list(pred_train)) pred_train<-unlist(pred_train)
  pred_train=data.frame(algorithm=fs_algo,ld_id=train_test$ld_id[train_test_selected_rows],date_time=train_test$date_time[train_test_selected_rows],yhat=pred_train)
  rm(rs)
  gc(verbose = F)
  #Predict test
  rs<-model$predict(model=model,X=test,selected_rows = test_selected_rows,normalize = F,inpute=T,verbose=T)
  pred_test<-rs$data$yhat
  if(is.list(pred_test)) pred_test<-unlist(pred_test)
  pred_test=data.frame(algorithm=fs_algo,ld_id=test$ld_id[test_selected_rows],date_time=test$date_time[test_selected_rows],yhat=pred_test)
  rm(rs)
  gc(verbose = F)
  
  #Save in results
  class_results<-rbind(class_results,list(algorithm=fs_algo,model_type=ifelse(model$lstm,'lstm','ann'),model=model,target_ratio=model$target_ratio,architecture=model$architecture,activation=model$activation,dropout=model$dropout,pred_train=pred_train,pred_test=pred_test,history=model$history,stat=model$stat,zero_sdv_columns_names=model$zero_sdv_columns_names))
  rm(list=c('pred_train','pred_test'))
  gc(verbose = F)
  #Save in file half iteration 
  #save(list=c('class_results','architecture_bench','train_test','train_test_selected_rows','test','test_selected_rows'),file=paste0(tmpfolder,'/after_classifier.RData'),compress = 'xz')
}

save(list=c('class_results','architecture_bench','train_test','train_test_selected_rows','test','test_selected_rows'),file=paste0(tmpfolder,'/after_classifier.RData'),compress = 'xz')
rm(list=ls()[!(ls() %in% before_vars)])
gc(verbose = F)

################# Evaluation ###############
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs'
before_vars<-ls()

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/after_classifier.RData') ,envir = tmp_env)
train_test_selected_rows<-tmp_env$train_test_selected_rows
train_test<-tmp_env$train_test
test_selected_rows<-tmp_env$test_selected_rows
test<-tmp_env$test
architecture_bench<-tmp_env$architecture_bench
class_results<-tmp_env$class_results
rm(tmp_env)

tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
use_ots<-tmp_env$use_ots
wp_id<-tmp_env$wt_query$wp_id[1]
db_config<-tmp_env$db_config
use_alarms<-tmp_env$use_alarms
wtdata<-tmp_env$wtdata
old_tmpfolder<-tmpfolder
if(!is.null(tmp_env$wt_query$additional_model_params)) eval(parse(text = tmp_env$wt_query$additional_model_params[1]))
tmpfolder<-old_tmpfolder
anticipation<-tmp_env$anticipation
margin<-tmp_env$margin
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
rm(tmp_env)

moving_window<-ceiling(2*24*60*60/seconds_to_aggregate) 

if(!exists("anticipation")) anticipation<-15
if(!exists("model_indicators")) source('functions_deep/model_indicators.R')
if(!exists("confusion_matrix")) source('functions_deep/confusion_matrix.R')
if(!exists("check_acciona_rules")) source('functions_common/check_acciona_rules.R')
if(!exists("check_acciona_rules_cm3")) source('functions_common/check_acciona_rules_cm3.R')

library(htmlwidgets)
library(htmltools)
library(plotly)
library(DT)
library(zoo)

#wtdata$pre_alarm<-pre_alarm$pre_alarm

#Train
rs<-model_indicators(fun_cm=check_acciona_rules,
                     wtdata=train_test[train_test_selected_rows,],
                     class_results=class_results,
                     use_ots=use_ots,
                     use_alarms=use_alarms,
                     type='pred_train',
                     generate_cm_table=T,
                     generate_plots=T,
                     plot_mvavg_only=F,
                     plot_pre_alarm=F,
                     threshold=0.5,
                     anticipation=anticipation,
                     moving_window=moving_window,
                     margin=margin)

#Test
rs<-model_indicators(fun_cm=check_acciona_rules,
                     wtdata=test[test_selected_rows,],
                     class_results=class_results,
                     use_ots=use_ots,
                     use_alarms=use_alarms,
                     type='pred_test',
                     generate_cm_table=T,
                     generate_plots=T,
                     plot_mvavg_only=F,
                     plot_pre_alarm=F,
                     threshold=0.5,
                     anticipation=anticipation,
                     moving_window=moving_window,
                     margin=margin)
