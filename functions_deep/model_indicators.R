model_indicators<-function(wtdata,class_results,fun_cm=NULL,use_ots,use_alarms,type='',generate_cm_table=T,generate_plots=T,show_ot_all=F,show_alarm_all=F,threshold=0.7,plot_mvavg_only=F,plot_pre_alarm=T,anticipation=15,margin=15,plot_daily=T,date_time_name='date_time',moving_window=ceiling(2*24*60*60/86400)){
  
  if(is.null(fun_cm)) return(list(error=T,data=NULL,msg="fun_cm function not set"))
  
  library(zoo)
  library(RColorBrewer)
  library(dplyr)
  cm<-data.frame(ld=as.numeric(),algo=as.character(),acc=as.numeric(),pres=as.numeric(),kappa=as.numeric(),tp=as.numeric(),fp=as.numeric(),tn=as.numeric(),fn=as.numeric(),stringsAsFactors = F)
  for(ld in unique(wtdata$ld_id)){
    current_ld_rows<-which(wtdata$ld_id==ld)
    if(length(current_ld_rows)>0){
      current_pre_alarm<-wtdata$pre_alarm[current_ld_rows] #pre alarm
      target<-current_pre_alarm
      #if(use_ots&&'ot' %in% names(wtdata)) target<-wtdata$ot[current_ld_rows]
      #if(use_alarms&&'alarm' %in% names(wtdata)) target<-target|(wtdata$alarm[current_ld_rows]) #real alarm
      dt_tmp<-wtdata$date_time[current_ld_rows]
      for(j in 1:nrow(class_results)){
        fs_algo<-paste0(class_results[[j,'model_type']],'_',paste0(class_results[[j,'architecture']],collapse = '_'),'_',class_results[[j,'activation']],'t',class_results[[j,'target_ratio']],'d',class_results[[j,'dropout']])
        pred<-class_results[[j,type]]
        yhat<-pred$yhat[pred$ld_id==ld]
        if('ot_all' %in% names(wtdata)) yhat[(wtdata$ot_all[current_ld_rows]==1)&(wtdata$ot[current_ld_rows]==0)]<-0 #Days with other ot probability dropped to 0 because is affected by the work.
        yhat_bin<-(yhat>threshold)
        tmp_cm <- fun_cm(date_time = dt_tmp,pre_alarm = yhat_bin, alarm = target, anticipation = anticipation, margin = margin,critic_fault=F)
        if(tmp_cm$error) return(list(error=T,data=NULL,msg=paste0("\n",iam," on call cm:",tmp_cm$msg)))
        tp<-sum(tmp_cm$data$conf_matrix=='tp',na.rm = T)
        fp<-sum(tmp_cm$data$conf_matrix=='fp',na.rm = T)
        tn<-sum(tmp_cm$data$conf_matrix=='tn',na.rm = T)
        fn<-sum(tmp_cm$data$conf_matrix=='fn',na.rm = T)
        total<-sum(tp+tn+fp+fn)
        acc<-(tp+tn)/total
        pres<-tp/(tp+fp)
        ma<-((tp+fp)*(tp+fn))/total
        mb<-((tn+fp)*(tn+fn))/total
        pe<-(ma + mb)/total
        k<-(acc-pe)/(1-pe)
        cm<-rbind(cm,data.frame(ld=ld,algo=fs_algo,acc=acc,pres=pres,kappa=k,tp=tp,fp=fp,tn=tn,fn=fn))
      }
    }
  }
  #cm$ld<-as.factor(cm$ld)
  kappa_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$kappa[cm$algo==a],na.rm=T)),3)
  kappa_median<-round(sapply(levels(cm$algo),function(a) median(cm$kappa[cm$algo==a],na.rm=T)),3)
  tp_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$tp[cm$algo==a],na.rm=T)),3)
  tp_median<-round(sapply(levels(cm$algo),function(a) median(cm$tp[cm$algo==a],na.rm=T)),3)
  fp_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$fp[cm$algo==a],na.rm=T)),3)
  fp_median<-round(sapply(levels(cm$algo),function(a) median(cm$fp[cm$algo==a],na.rm=T)),3)
  tn_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$tn[cm$algo==a],na.rm=T)),3)
  tn_median<-round(sapply(levels(cm$algo),function(a) median(cm$tn[cm$algo==a],na.rm=T)),3)
  fn_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$fn[cm$algo==a],na.rm=T)),3)
  fn_median<-round(sapply(levels(cm$algo),function(a) median(cm$fn[cm$algo==a],na.rm=T)),3)
  pres_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$pres[cm$algo==a],na.rm=T)),3)
  pres_median<-round(sapply(levels(cm$algo),function(a) median(cm$pres[cm$algo==a],na.rm=T)),3)
  acc_mean<-round(sapply(levels(cm$algo),function(a) mean(cm$acc[cm$algo==a],na.rm=T)),3)
  acc_median<-round(sapply(levels(cm$algo),function(a) median(cm$acc[cm$algo==a],na.rm=T)),3)
  indicators_summary<-data.frame(algo=levels(cm$algo),kappa_mean=kappa_mean,kappa_median=kappa_median,tp_mean=tp_mean,tp_median=tp_median,fp_mean=fp_mean,fp_median=fp_median,tn_mean=tn_mean,tn_median=tn_median,fn_mean=fn_mean,fn_median=fn_median,pres_mean=pres_mean,pres_median=pres_median,acc_mean=acc_mean,acc_median=acc_median)
  
  if(generate_cm_table){
    html_tags <- vector(mode = "list", length = 4)
    html_tags[[1]]<-htmltools::h1(paste0("CM results for a ",type," dataset"))
    html_tags[[2]]<-DT::datatable(cm,caption = "Confusion Matrix by method and ld_id")
    html_tags[[3]]<-htmltools::br()
    html_tags[[4]]<-DT::datatable(indicators_summary,caption = "Confusion Matrix by method")
    save_html(tagList(html_tags), paste0(getwd(),'/',tmpfolder,'/',type,'_cm.html'), background = "white", libdir = "lib")
    save(list=c('cm','indicators_summary'),file=paste0(tmpfolder,'/',type,'_cm.RData'),compress = 'xz')
  }
  
  if(generate_plots){
    if(plot_daily){
      wtdata[,date_time_name]<-as.POSIXct((as.numeric(wtdata[,date_time_name])%/%86400)*86400,origin='1970-01-01',tz = "UTC")
      alarm_ot_columns<-c("pre_alarm","alarm","alarm_all","ot","ot_all");
      if(any(alarm_ot_columns %in% names(wtdata))){
        alarm_ot_columns<-c('ld_id',date_time_name,alarm_ot_columns);
        alarm_ot_columns<-alarm_ot_columns[alarm_ot_columns %in% names(wtdata)];
        grouped_alarm_ot<-wtdata[,  (names(wtdata) %in% alarm_ot_columns)] %>% group_by(date_time,ld_id) %>% summarise_all(funs(max(., na.rm = T)))
        wtdata_out_day<-data.frame(grouped_alarm_ot)
      }
      alarm_ot_desc_columns<-c("alarm_block_code","alarm_all_block_code","ot_block_code","ot_all_block_code");
      if(any(alarm_ot_desc_columns %in% names(wtdata))){
        alarm_ot_desc_columns<-c('ld_id',date_time_name,alarm_ot_desc_columns);
        alarm_ot_desc_columns<-alarm_ot_desc_columns[alarm_ot_desc_columns %in% names(wtdata)];
        grouped_alarm_ot_desc<-wtdata[, (names(wtdata) %in% alarm_ot_desc_columns)] %>% group_by(date_time,ld_id) %>% summarise_all(funs(paste(unique(unlist(strsplit(.,','))),collapse = ',')))
        wtdata<-cbind(wtdata_out_day,grouped_alarm_ot_desc[,!(names(grouped_alarm_ot_desc) %in% c(date_time_name,'ld_id'))])
      }else{
        wtdata<-wtdata_out_day
      }
    }
    
    for(ld in unique(wtdata$ld_id)){
      df<-NULL
      current_ld_rows<-which(wtdata$ld_id==ld)
      #Selected
      tmp_df<-wtdata[current_ld_rows,]
      if(show_ot_all&&('ot_all' %in% names(tmp_df))) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$ot_all,fs_algo='ot_all',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:ot_all',stringsAsFactors = F),ot_other=0))
      if('ot' %in% names(tmp_df)&&any(as.logical(tmp_df$ot))){
        if('ot_block_code' %in% names(tmp_df))
          df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$ot,fs_algo='ot',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>ots:',gsub(pattern = ',',replacement = '<br>',x = tmp_df$ot_block_code),stringsAsFactors = F),ot_other=0))
        else
          df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$ot,fs_algo='ot',text=paste('date:',as.POSIXct(tmp_df$date_time),stringsAsFactors = F),ot_other=0))
      }
      
      if(('ot' %in% names(tmp_df)&&plot_pre_alarm)||!('ot' %in% names(tmp_df))) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$pre_alarm,fs_algo=rep('pre_alarm',length(tmp_df$pre_alarm)),text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:pre_alarm'),ot_other=0,stringsAsFactors = F))
      
      if(show_ot_all&&('alarm_all' %in% names(tmp_df))) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=tmp_df$alarm_all,fs_algo='alarm_all',text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:alarm_all'),ot_other=0,stringsAsFactors = F))
      for(j in 1:nrow(class_results)){
        fs_algo<-paste0(class_results[[j,'model_type']],'_',paste0(class_results[[j,'architecture']],collapse = '_'),'_',class_results[[j,'activation']],'t',class_results[[j,'target_ratio']],'d',class_results[[j,'dropout']])
        pred<-class_results[[j,type]]
        current_pred<-pred[pred$ld_id==ld,]
        if(plot_daily){
          current_pred$date_time<-as.POSIXct((as.numeric(current_pred$date_time)%/%86400)*86400,origin='1970-01-01',tz = "UTC")
          current_pred<-current_pred[,c('yhat','date_time')] %>% group_by(date_time) %>% summarise_all(funs(max(., na.rm = T)))
        }
        yhat<-current_pred$yhat
        
        if(!plot_mvavg_only) df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat,fs_algo=fs_algo,text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat,4),'<br>pre_alarm:',(tmp_df$pre_alarm>0),'<br>ot_other:',ifelse(is.na(tmp_df$ot_all>0),0,tmp_df$ot_all>0)),ot_other=ifelse(is.na((tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),0,(tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),stringsAsFactors = F))
        #Moving avg
        yhat_mvavg<-zoo::rollapply(yhat,moving_window, mean,align = "right", na.rm = TRUE)
        yhat_mvavg<-c(rep(NA,moving_window-1),yhat_mvavg)
        if('ot_all' %in% names(tmp_df)){
          #df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat[current_ld_rows],fs_algo=fs_algo,text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat[current_ld_rows],4),'<br>pre_alarm:',(tmp_df$pre_alarm>0),'<br>ot_other:',ifelse(is.na(tmp_df$ot_all>0),0,tmp_df$ot_all>0)),ot_other=ifelse(is.na((tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),0,(tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),stringsAsFactors = F)) 
          df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat_mvavg,fs_algo=paste0(fs_algo,'_yhat_mvavg_',moving_window),text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat_mvavg,4),'<br>pre_alarm:',tmp_df$pre_alarm>0,'<br>ot_other:',ifelse(is.na(tmp_df$ot_all>0),0,tmp_df$ot_all>0)),ot_other=ifelse(is.na((tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),0,(tmp_df$pre_alarm==0)&(tmp_df$ot_all>0)),stringsAsFactors = F))
        }else{
          #df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat[current_ld_rows],fs_algo=fs_algo,text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat[current_ld_rows],4),'<br>pre_alarm:',(tmp_df$pre_alarm>0)),ot_other=0,stringsAsFactors = F))
          df<-rbind(df,data.frame(ld_id=ld,date_time=tmp_df$date_time,yhat=yhat_mvavg,fs_algo=paste0(fs_algo,'_yhat_mvavg_',moving_window),text=paste('date:',as.POSIXct(tmp_df$date_time),'<br>fs_algo:',fs_algo,'<br>prob:',round(yhat_mvavg,4),'<br>pre_alarm:',(tmp_df$pre_alarm>0)),ot_other=0,stringsAsFactors = F))
        }
      }
      
      n<-length(unique(df$fs_algo))
      if('ot' %in% unique(df$fs_algo)){
        if(n>9){
          my_color_palete = rainbow(n)
        }else{
          my_color_palete = brewer.pal(n=n, "Set1")
        }
        pos_ot<-which(unique(df$fs_algo)=='ot')
        if(pos_ot>1&&pos_ot<n){
          my_color_palete<-c(my_color_palete[2:pos_ot],my_color_palete[1],my_color_palete[(pos_ot+1):n])
        }else if(pos_ot==n){
          my_color_palete<-c(my_color_palete[2:(n-1)],my_color_palete[1])
        }
      }else{
        n<-n+1
        if(n>9){
          my_color_palete = rainbow(n)[2:n]
        }else{
          my_color_palete = brewer.pal(n=n , "Set1")[2:n]
        }
      }
      
      if(any(levels(df$fs_algo)=='ot')){
        ot_pos<-which(levels(df$fs_algo)=='ot')
        reorder<-1:length(levels(df$fs_algo))
        reorder<-reorder[reorder!=ot_pos]
        df$fs_algo = factor(df$fs_algo,levels(df$fs_algo)[c(reorder,ot_pos)])
        my_color_palete<-my_color_palete[c(reorder,ot_pos)]
      }
      #df$fs_algo<-as.factor(df$fs_algo)  
      #Only shows points with pre_alarms or >0.001.
      #df<-df[is.na(df$ot_other)|(!df$ot_other&(df$yhat>0.001)),]
      
      p<-ggplot()
      p<-p + geom_point(data=df[(df$ld_id==df)&(df$fs_algo!='ot'),],aes(x=date_time,y=yhat,color=fs_algo,text=text),alpha=0.6)
      p<-p + geom_point(data=df[(df$ld_id==df)&(df$fs_algo=='ot')&(df$yhat>0),],aes(x=date_time,y=yhat,color=fs_algo,text=text),alpha=0.6,size=4)
      p<-p + theme_light()+ylim(0, 1)+scale_colour_manual(values=my_color_palete)
      p<-p + ggtitle(paste0("Results of the turbine ",ld," from ",type," dataset"))
      ggsave(paste0(getwd(),'/',tmpfolder,'/',type,'_',ld,'_fs_class_results.png'),plot=p, width = 15, height = 9)
      
      #Publication
      p<-ggplot()
      p<-p + geom_point(data=df[(df$ld_id==df)&(df$fs_algo!='ot'),],aes(x=date_time,y=yhat,color=fs_algo,text=text),alpha=0.6)
      p<-p + geom_point(data=df[(df$ld_id==df)&(df$fs_algo=='ot')&(df$yhat>0),],aes(x=date_time,y=yhat,color=fs_algo,text=text),alpha=0.6,size=4)
      p<-p + theme_light()+ylim(0, 1)+scale_colour_manual(values=my_color_palete)
      p<-p + theme(legend.position="none")+theme(axis.title.x=element_blank(),axis.title.y=element_blank())
      ggsave(paste0(getwd(),'/',tmpfolder,'/',type,'_',ld,'_fs_class_results.pdf'),plot=p, width = 15, height = 9)
      
      p<-ggplotly(p,tooltip=c('text'))
      #p<- p+geom_point_interactive(aes(tooltip = text, data_id = text), size = 2) 
      #p<-ggiraph(ggobj =p)
      #htmlwidgets::saveWidget(p,file = paste0(tmpfolder,'/fs_class_results_',ld,'.html'))
      htmlwidgets::saveWidget(p,file = paste0(getwd(),'/',tmpfolder,'/',type,'_',ld,'_fs_class_results.html'),libdir = paste0(getwd(),'/',tmpfolder,'/lib'),selfcontained = F)
    }
  }
  return(list(error=F,data=list(cm=cm,indicators_summary=indicators_summary,prob_plot=df),msg='ok'))
}
