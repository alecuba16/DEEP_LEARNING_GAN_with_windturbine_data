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
if(!is.null(tmp_env$wt_query$train_machines[1]) && grepl(pattern = '[-]?[0-9]+,',x = tmp_env$wt_query$train_machines[1])){
  train_machines<-as.numeric(unlist(strsplit(tmp_env$wt_query$train_machines[1],split = ',')))
}else{
  train_machines<-tmp_env$wt_query$train_machines[1]
}
balance<-if(exists('balance')) tmp_env$balance else tmp_env$balance_per
balance_by<-tmp_env$balance_by
rm(tmp_env)
gc(verbose = F)

#train_machines<-50 #50% for train 50% for test.
train_machines<-abs(train_machines)

if(!exists('preprocess_data')) source('preprocess_data.R')

rs<-preprocess_data(wtdata=wtdata,selected_turbines=unique(wtdata$ld_id),
                    target_name='pre_alarm',use_alarms=use_alarms,
                    use_ots=use_ots,use_health=use_health,anticipation=anticipation,
                    margin=margin,seconds_to_aggregate=seconds_to_aggregate,
                    marging_after_pre_alarm=60*60*24*7,
                    seconds_ot_marging=60*60*24,
                    registers_before_pre_alarm=F,
                    na_columns_per_exclude=10)
if(rs$error) stop('error in preprocess_data')  
wtdata<-rs$data$wtdata
selected_rows<-rs$data$selected_rows
selected_turbines<-rs$data$selected_turbines
zero_sdv_columns_names<-rs$data$zero_sdv_columns_names
na_columns_names<-rs$data$na_columns_names
selected_columns<-rs$data$selected_columns
normalized<-rs$data$normalized
stat<-rs$data$stat
exclude_columns<-unique(c(colnames(wtdata)[!(colnames(wtdata) %in% selected_columns)],'date_time','pre_alarm_0_anti',target_name,'ld_id','health_status','alarm','alarm_block_code','alarm_all','alarm_all_block_code','ot','ot_all','ot_block_code','ot_all_block_code','alarm_block_code','alarm_all_block_code'))

#Src,libs
source('functions_common/feature_selection.R', echo=TRUE)

fs_params = list()
#fs_params = rbind(fs_params,list(algorithm='pvalue',na_columns_per_exclude=10,complete_cases=T,params=data.frame(method="simulation",nsim=1000,pvalue=0.05,stringsAsFactors = F),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='caretrfe',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
#fs_params = rbind(fs_params,list(algorithm='boruta',na_columns_per_exclude=10,complete_cases=T,params=data.frame(q=0.25,maxRuns=50,field='meanImp'),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='cmim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='disr',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='jmi',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='jmim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='mim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='mrmr',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_params = rbind(fs_params,list(algorithm='njmim',na_columns_per_exclude=20,complete_cases=T,params=data.frame(q=0.25),cor_threshold=0.95))
fs_results<-list()

target_name<-'pre_alarm'
normalize<-!normalized
#Only turbines with failures
tmp_selected<-selected_rows[selected_rows %in% which(wtdata$pre_alarm==T)]
lds<-unique(wtdata$ld_id[tmp_selected])

for(i in 1:nrow(fs_params)){
  for(ld in lds){
    tmp_selected_rows<-selected_rows[selected_rows %in% which(wtdata$ld_id==ld)]
    cat(paste0('Algoritm ',i,' of ',nrow(fs_params),' :',fs_params[i,]$algorithm,' for turbine ',ld,'\n'))
    #Remove all data with ot_all=1 for train
    rs<-feature_selection(wtdata=wtdata[tmp_selected_rows,],
                          exclude_columns=exclude_columns,
                          target_name=target_name,
                          algorithm=fs_params[i,]$algorithm,
                          date_time_name='date_time',
                          cor_threshold=fs_params[i,]$cor_threshold,
                          na_columns_per_exclude=fs_params[i,]$na_columns_per_exclude,
                          complete_cases=fs_params[i,]$complete_cases,
                          params=fs_params[i,]$params,
                          discretize=F,
                          parallel=T,
                          normalize=normalize,
                          logfile='cluster.log')
    if(rs$error) stop(paste0("\non call feature_selection\n\t",rs$msg))
    tmp<-rs$data
    tmp$wtdata<-NULL #Free space
    tmp$algorithm<-fs_params[[i,'algorithm']]
    tmp$params<-fs_params[[i,'params']]
    tmp$complete_cases<-fs_params[[i,'complete_cases']]
    tmp$na_columns_per_exclude<-fs_params[[i,'na_columns_per_exclude']]
    tmp$cor_threshold<-fs_params[[i,'cor_threshold']]
    tmp$scores_variables$ld_id<-ld
    fs_results<-rbind(fs_results,tmp)
    rm(rs)
    gc(verbose = F)
  }
}

library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#Only selected
#Compare selections
scores<-NULL
x_cut<-NULL
x<-NULL
for(i in 1:nrow(fs_results)){
  tmp<-fs_results[[i,'scores_variables']]
  algo_name<-fs_results[[i,'algorithm']]
  tmp_scores<-tmp$score[tmp$selected]
  #normalize scores
  normalized_score<-(tmp_scores-min(tmp_scores,na.rm = T))/(max(tmp_scores,na.rm = T)-min(tmp_scores,na.rm = T))
  scores<-rbind(scores,data.frame(algo=algo_name,scores=normalized_score,name=tmp$name[tmp$selected],ld_id=tmp$ld_id[tmp$selected]))
}

#df <- melt(scores, id.vars=c('name','algo'))
scores$ld_id<-as.factor(scores$ld_id)
p<-ggplot(scores, aes(x=name,y=scores,fill=ld_id)) + geom_bar(stat = "identity")+ facet_grid(algo ~ .)+theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave('fs_per_turbine.png',plot=p, width = 20, height = 12)
#gplot(scores, aes(ld_id, names)) + geom_bar(aes(fill = scores), position = "dodge", stat="identity")

#Sum normalized scores
scores_names<-unique(scores$name)
scores_values<-lapply(scores_names,function(var){
  df<-data.frame(var=paste0('c_',var),method="count",value=sum(scores$scores[scores$name==var],na.rm = T))
  df<-rbind(df,data.frame(var=paste0('a_',var),method="mean",value=mean(scores$scores[scores$name==var],na.rm = T)))
  df<-rbind(df,data.frame(var=paste0('m_',var),method="median",value=median(scores$scores[scores$name==var],na.rm = T)))
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

#Remove 0
scores_values<-scores_values[scores_values$value!=0,]

scores_values<-transform(scores_values,var=reorder(var,-(value)))
#Split mean
p<-ggplot(scores_values,aes(x=var,y=value))+ geom_bar(stat = "identity")+ggtitle("Sorted by scores")+theme(axis.text.y = element_text(size = 5))+facet_wrap(~method, ncol = 1,scales = 'free_y')+ coord_flip()
#scores_sum<-data.frame(name=scores_names,sum=scores_values)
#scores_sum <- transform(scores_sum, name = factor(name, levels = scores_sum$name[order(scores_sum$sum,decreasing = F)]))
ggsave('fs_count_sorted.png',plot=p, width = 20, height = 25)
