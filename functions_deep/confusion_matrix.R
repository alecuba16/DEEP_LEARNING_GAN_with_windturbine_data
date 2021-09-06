confusion_matrix <- function(date_time = NULL, pre_alarm=NULL,real_alarm=NULL,anticipation = 14, margin = 14,critic_fault=F) {
  iam=match.call()[[1]]
  
  
  
  #Fix date to posix (requirement for seq.POSIXt)
  date_time<-as.POSIXct(date_time,tz='UTC',origin='1970-01-01')
  
  #Create a temporary binary real_pre_alarm by default false
  real_pre_alarm<-rep(0,length(pre_alarm))
  
  if(any(alarm==1)){
    #Get the real_prealarm vector of each alarm and the dates between alarm-horitzon -> alarm
    real_pre_alarm_dates<-as.POSIXct(unlist(lapply(date_time[alarm==1],function(dt) seq.POSIXt(dt-as.difftime(anticipation+margin, units="days"),dt-as.difftime(anticipation, units="days"),by='day'))),origin='1970-01-01',tz = "UTC")
    #Asign true only to pre_real_alarm_dates.
    real_pre_alarm[date_time %in% real_pre_alarm_dates]<-1
  }
  
  for(i in which(alarm==1)){
    a_dt<-seq.POSIXt(date_time[i]-as.difftime(anticipation+margin, units="days"),date_time[i]-as.difftime(anticipation, units="days"),by='day')
    if(sum(date_time %in% a_dt)>0&&any(pre_alarm[date_time %in% a_dt]==1,na.rm = T)){#Mark next 0 as 1
      p_dt<-date_time[pre_alarm[date_time %in% a_dt]==1]
      p_dt<-min(p_dt,na.rm = T)
      pre_alarm[date_time>=p_dt & date_time<=date_time[i]]<-1
    }
  }
  
  
  #by default tn
  cm<-rep('tn',length(date_time))
  #by default 1 are fp
  cm[pre_alarm==1]<-'fp'
  
  cm[(real_pre_alarm==1)&(pre_alarm==0)]<-'fn'
  cm[(real_pre_alarm==1)&(pre_alarm==1)]<-'tp'

  #cm<-data.frame(tp=sum(cm=='tp'),fp=sum(cm=='fp'),tn=sum(cm=='tn'),fn=sum(cm=='fn'))
  
  return(list(error=FALSE,data=list(conf_matrix=cm),msg="Ok"))
}