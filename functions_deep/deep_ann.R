comb <- function(results, x) {
  results[x$rmin:x$rmax,1:(x$timesteps*x$num_registers_day),] <- x$result
  results
}

prepare_lstm<-function(wtdata=NULL,selected_rows=NULL,selected_columns=NULL,exclude_columns=c(date_time_name,'ld_id'),timesteps=NULL,filter_bad_in_good=F,date_time_name='date_time',logfile=NULL,verbose=F){
  #Only use selected columns,ld_id and date_time
  wtdata<-wtdata[,colnames(wtdata) %in% c(selected_columns,'ld_id',date_time_name)]
  wtdata$row_id<-1:nrow(wtdata)
  
  first_ld_dt<-wtdata[(wtdata$ld_id==unique(wtdata$ld_id)[1]),date_time_name]
  first_ld_dt<-sort(first_ld_dt)
  seconds_to_aggregate<-as.numeric(median(diff(first_ld_dt),na.rm=T), units = "secs")
  rm(first_ld_dt)
  gc(verbose = F)
  timesteps_rows<-(timesteps*24*60*60)/seconds_to_aggregate
  lds_ids<-unique(wtdata$ld_id)
  wtdata_lstm<-NULL
  row_ids<-NULL
  for(ld in lds_ids){
    if(verbose) cat(paste0("\nTurbine (",which(ld==lds_ids),"/",length(lds_ids),')'))
    tmp<-wtdata[wtdata$ld_id==ld,]
    wtdata<-wtdata[wtdata$ld_id!=ld,]
    gc(verbose = F)
    #Reconstruct fixed date_time
    gen_dt<-seq.POSIXt(min(tmp[,date_time_name],na.rm = T),by =paste0(seconds_to_aggregate,' sec'),to = max(tmp[,date_time_name],na.rm = T))
    gen_dt<-data.frame(date_time=gen_dt)
    colnames(gen_dt)[1]<-date_time_name
    tmp<-merge(gen_dt,tmp,by=date_time_name,all = T)
    rm(gen_dt)
    gc(verbose = F)
    if(paste0(date_time_name,'.x') %in% colnames(tmp)){
      colnames(tmp)[colnames(tmp)==paste0(date_time_name,'.y')]<-date_time_name
      colnames(tmp)[colnames(tmp)==paste0(date_time_name,'.x')]<-paste0(date_time_name,'_fake')
    }
    
    #Order columns as expected by selected_columns
    tmp<-tmp[,c(selected_columns,date_time_name,'row_id')]
    #Sort by dt
    tmp<-tmp[order(tmp[,date_time_name]),]
    
    tmp_array<-array(NA,dim=list(nrow(tmp),timesteps_rows,length(selected_columns)),dimnames = list(1:nrow(tmp),1:timesteps_rows,selected_columns))
    for(i in 1:dim(tmp_array)[1]){
      if(i<timesteps){
        selected_o<-max(i-timesteps-1,1):i
        selected_d<-(timesteps-i+1):timesteps
      }else{
        selected_o<-(i-timesteps+1):i
        selected_d<-1:timesteps
      }
      bads<-tmp[selected_o,'pre_alarm']==1
      tmp_array[i,selected_d,]<-as.matrix(tmp[selected_o,selected_columns])
      if(filter_bad_in_good&&any(bads)) tmp_array[i,bads,]<-rep(NA,length(selected_columns))
    }
    
    #
    row_id<-tmp$row_id[!is.na(tmp$row_id)]
    
    rm(tmp)
    gc(verbose = F)
    #bk order_id
    tmp_array<-tmp_array[!is.na(row_id),,]
    row_ids<-c(row_ids,row_id)
    
    #Combine
    wtdata_lstm<-abind::abind(wtdata_lstm,tmp_array,along = 1)
    rm(tmp_array)
    gc(verbose = F)
  }
  rm(wtdata)
  gc(verbose = F)
  #reorder output as input wtdata
  wtdata_lstm<-wtdata_lstm[order(row_ids),,]
  
  # #Remove date_time 
  # if(paste0(date_time_name,'_fake') %in% colnames(tmp)){
  #   selected<-which(!is.na(tmp[,date_time_name]))
  #   if(length(selected)>0)
  #   wtdata_lstm<-wtdata_lstm[selected,,]
  # }
  #wtdata_lstm<-wtdata_lstm[selected_rows,,]
  return(list(error=F,data=list(X=wtdata_lstm)))
}

kappa<-function(y_true, y_pred){
  K <- backend()
  y_pred_pos = K$round(K$clip(y_pred, 0, 1))
  y_pred_neg = 1 - y_pred_pos
  y_pos = K$round(K$clip(y_true, 0, 1))
  y_neg = 1 - y_pos
  tp = K$sum(y_pos * y_pred_pos)
  tn = K$sum(y_neg * y_pred_neg)
  fp = K$sum(y_neg * y_pred_pos)
  fn = K$sum(y_pos * y_pred_neg)
  total =tp+tn+fp+fn
  acc<-(tp+tn)/total
  ma<-((tp+fp)*(tp+fn))/total
  mb<-((tn+fp)*(tn+fn))/total
  pe<-(ma + mb)/total
  return((acc-pe)/(1-pe+K$epsilon()))
  #return(total)
}

precision<-function(y_true, y_pred){
  K <- backend()
  true_positives = K$sum(K$round(K$clip(y_true * y_pred, 0, 1)))
  predicted_positives = K$sum(K$round(K$clip(y_pred, 0, 1)))
  precision = true_positives / (predicted_positives + K$epsilon())
  return(precision)
}

specificity<-function(y_true, y_pred){
  K <- backend()
  true_negatives = K$sum(K$round(K$clip((1-y_true) * (1-y_pred), 0, 1)))
  possible_negatives = K$sum(K$round(K$clip(1-y_true, 0, 1)))
  return(true_negatives / (possible_negatives + K$epsilon()))
}

sensitivity<-function(y_true, y_pred){
  K <- backend()
  true_positives = K$sum(K$round(K$clip(y_true * y_pred, 0, 1)))
  possible_positives = K$sum(K$round(K$clip(y_true, 0, 1)))
  return(true_positives / (possible_positives + K$epsilon()))
}

leaky_relu<-function(x, alpha){
  #if(alpha<1)
  return(tf$maximum(x, alpha * x))
  #else
  #  return(tf$nn$relu(x)-alpha*tf$nn$relu(-x))
}

# normalize_range<-function(X,selected_rows=NULL,selected_columns=NULL,min=0,max=1,lstm=F){
#   X[,selected_columns]<-(apply(X[,selected_columns],2,function(col){
#     # Normalize to [0, 1]:
#     if(lstm){
#       m<-min(col,na.rm = T)
#       range<- max(col,na.rm = T)-m
#     }else{
#       m<-min(col[selected_rows],na.rm = T)
#       range<- max(col[selected_rows],na.rm = T)-m
#     }
#     col<-(col - m)/range
#     #Then scale to [x,y]:
#     range2<-max-min
#     normalized<-(col*range2)+min;
#     return(normalized)
#   }))
#   return(X)
# }

normalize_range<-function(X,selected_rows=NULL,selected_columns=NULL,min=0,max=1,lstm=F,min_max_train=NULL){
  if(is.null(min_max_train)){
    min_max_train<-lapply(which(colnames(X) %in% selected_columns),function(col){
      # Normalize to [0, 1]:
      if(lstm) selected_rows<-1:dim(X)[1]
      min_tmp<-min(X[selected_rows,col],na.rm = T)
      max_tmp<-max(X[selected_rows,col],na.rm = T)
      return(data.frame(name=colnames(X)[col],min=min_tmp,max=max_tmp))
    })
    min_max_train<-do.call("rbind",min_max_train)
  }
  for(name in min_max_train$name){
    range<-min_max_train$max[min_max_train$name==name]-min_max_train$min[min_max_train$name==name]
    X[,name]<-(X[,name] - min_max_train$min[min_max_train$name==name])/range
    #Then scale to [x,y]:
    range2<-max-min
    X[,name]<-(X[,name]*range2)+min
  }
  return(list(error=F,data=list(X=X,min_max_train=min_max_train),msg='ok'))
}

inpute_lstm_median<-function(X=NULL,selected_rows=NULL,inpute=F,verbose=F){
  to_remove<-NULL
  for(row in 1:dim(X)[1]){
    if(verbose&&(row %% 100)==0) cat(paste0('\n',round((row*100)/(dim(X)[1]),1)),'%')
    if(any(complete.cases(X[row,,]))&&!all(complete.cases(X[row,,]))&&inpute){
      for(col in 1:dim(X[row,,])[2]){
        X[row,is.na(X[row,,col]),col]<-median(X[row,,col],na.rm = T)
      }
    }else{
      to_remove<-c(to_remove,row)
    }
  }
  if(!is.null(to_remove)) selected_rows<-selected_rows[!(selected_rows %in% to_remove)]
  return(list(error=F,data=list(X=X,selected_rows=selected_rows),msg='ok'))
}

predict_ann<-function(model=NULL,X=NULL,selected_rows=NULL,normalize=F,inpute=F,date_time_name='date_time',verbose=F){
  iam=match.call()[[1]]
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  sourcesandlib<-c('tensorflow','keras','caret','dplyr')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(all(is.null(X))||all(is.na(X))) return(list(error=T,data=NULL,msg='X null'))
  if(all(is.null(model))||all(is.na(model))) return(list(error=T,data=NULL,msg='model null'))
  if(nrow(X)<max(selected_rows)) return(list(error=T,data=NULL,msg='X have to be the original dataframe referenced by selected_rows'))
  
  #Load model
  #model_class<-keras::load_model_hdf5(filepath = model$save_model_path,custom_objects = list(precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa),compile = T)
  if(verbose) cat("\nLoading model...")
  model_obj<-keras::unserialize_model(model=model$model_serialized,custom_objects =list(precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa) ,compile=T)
  timesteps<-NULL
  stateful<-F
  
  lstm<-model$lstm
  if(lstm&&!('ld_id' %in% colnames(X))) return(list(error=T,data=NULL,msg='missing ld_id from X colnames, is required for lstm'))
  if(lstm&&!(date_time_name %in% colnames(X))) return(list(error=T,data=NULL,msg=paste0('missing ',date_time_name,' from X colnames, is required for lstm')))
  timesteps<-model$timesteps
  if((stateful %in% colnames(model))&&!is.null(model$stateful)&&model$stateful) stateful<-T
  
  selected_rows<-selected_rows[order(selected_rows)]
  
  #Check if same columns
  if(!all(model$model_columns %in% colnames(X))) return(list(error=T,data=NULL,msg=paste0('missing these variables columns on X:',paste0(model$model_columns[!(model$model_columns %in% colnames(X))],collapse = ','))))
  
  selected_columns<-model$model_columns
  
  #Normalize range
  range<-c(0,1)
  min_max_train<-model$min_max_train
  if(model$activation=='tanh') range<-c(-1,1)
  if(model$activation=='sigmoid') range<-c(0,1)
  rs<-normalize_range(X,selected_rows=selected_rows,selected_columns=selected_columns,min=range[1],max=range[2],lstm=lstm,min_max_train=min_max_train)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  X<-rs$data$X
  
  yhat_out<-rep(0,length(selected_rows))
  if(lstm){
    if(verbose) cat("\nGenerating LSTM data format...")
    rs<-prepare_lstm(wtdata = X,selected_rows = selected_rows,selected_columns=selected_columns,timesteps = timesteps,filter_bad_in_good = F,date_time_name=date_time_name,verbose=verbose)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    X<-rs$data$X
    rs<-inpute_lstm_median(X=X,selected_rows=selected_rows,inpute=inpute,verbose=T)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    X<-rs$data$X
    selected_rows_new<-rs$data$selected_rows
    X<-X[selected_rows_new,,selected_columns]
  }else{
    selected_rows_new<-selected_rows
    X<-X[selected_rows_new,selected_columns]
    if(!is.matrix(X)) X<-as.matrix(X)
  }
  
  if((lstm&&!all(selected_columns %in% dimnames(X)[[3]]))||(!lstm&&!all(selected_columns %in% colnames(X)))) return(list(error=T,data=NULL,msg="Missing columns because is NA on test dataset"))
  #selected_rows_new<-rs$data$selected_rows
  #na_columns_names<-rs$data$na_columns_names
  #stat<-rs$data$stat
  #zero_sdv_columns_names<-rs$data$zero_sdv_columns_names
  if(verbose) cat("\nPredicting...")
  #Evaluate
  if(lstm&&stateful)
    yhat<-model_obj$predict(x = X,batch_size = length(selected_rows_new))
  else
    yhat<-model_obj$predict(x=X)
  #pos<-which(selected_rows_new %in% selected_rows)
  yhat_out[selected_rows %in% selected_rows_new]<-yhat
  return(list(error=F,data=list(yhat=yhat_out,selected_rows=selected_rows_new),msg='ok'))
}

deep_ann<-function(X=NULL,y=NULL,selected_rows=NULL,X_val=NULL,y_val=NULL,selected_rows_val=NULL,val_size=0.1,selected_columns=NULL,lstm=F,lr=0.001,epochs=500,batch_size=length(y),architecture=c(50,20,10,1),activation='relu',dropout=0,input_dropout=0,recurrent_dropout=0,target_ratio=NULL,timesteps=15,view_metrics=F,view_progress_bar=F,use_gpu=F,save_model_path=NULL,log_device_placement=F,date_time_name='date_time',inpute=T,normalize=F,logfile=NULL,stateful=F,testing=F,tmpfolder=NULL,verbose=F){
  iam=match.call()[[1]]
  #Dependencias
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('abind','tensorflow','keras','caret','dplyr','parallel','digest')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  if(all(is.null(X))||all(is.na(X))) return(list(error=T,data=NULL,msg='X null'))
  if(all(is.null(y))||all(is.na(y))) return(list(error=T,data=NULL,msg='y null'))
  if(nrow(X)<max(selected_rows)) return(list(error=T,data=NULL,msg='X have to be the original dataframe referenced by selected_rows'))
  if(nrow(X)!=length(y)) return(list(error=T,data=NULL,msg='X and target length Y are different in length'))
  if(lstm&&!('ld_id' %in% colnames(X))) return(list(error=T,data=NULL,msg='missing ld_id from X colnames, is required for lstm'))
  if(lstm&&!(date_time_name %in% colnames(X))) return(list(error=T,data=NULL,msg=paste0('missing ',date_time_name,' from X colnames, is required for lstm')))
  
  #Change target variable to numeric 0/1
  if(is.logical(y)) y<-as.numeric(y)
  if(!(is.logical(y)||is.numeric(y))||(is.numeric(y)&&((length(unique(y))!=2)||!all(c(0,1) %in% unique(y))))) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable y must be or logical or numeric with 0/1")))
  
  selected_rows<-selected_rows[order(selected_rows)]
  #Sort columns by selected
  selected_columns<-selected_columns[order(selected_columns)]
  
  #Normalize range
  range<-c(0,1)
  if(activation=='tanh') range<-c(-1,1)
  if(activation=='sigmoid') range<-c(0,1)
  rs<-normalize_range(X,selected_rows=selected_rows,selected_columns=selected_columns,min=range[1],max=range[2],lstm=lstm)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  X<-rs$data$X
  min_max_train<-rs$data$min_max_train
  
  if(!is.null(X_val)&&!is.null(y_val)){
    #Normalize range
    rs<-normalize_range(X_val,selected_rows=selected_rows_va,selected_columns=selected_columns,min=range[1],max=range[2],lstm=lstm,min_max_train=min_max_train)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    X_val<-rs$data$X
    if(is.logical(y_val)) y_val<-as.numeric(y_val)
  }

  if(lstm){
    rs<-prepare_lstm(wtdata = X,selected_rows = selected_rows,selected_columns=selected_columns,timesteps = timesteps,filter_bad_in_good = T,date_time_name=date_time_name,verbose=verbose)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    rm(X)
    gc(verbose = F)
    X<-rs$data$X
    rm(rs)
    gc(verbose = F)
    rs<-inpute_lstm_median(X=X,selected_rows=selected_rows,inpute=inpute,verbose=T)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    rm(X)
    gc(verbose = F)
    X<-rs$data$X
    selected_rows<-rs$data$selected_rows
    rm(rs)
    gc(verbose = F)
    X_train<-X[selected_rows,,selected_columns]
    y_train<-y[selected_rows]
    num_rows<-length(y_train)
    #Val
    if(!is.null(X_val)&&!is.null(y_val)){
      rs<-prepare_lstm(wtdata = X_val,selected_rows = selected_rows_val,selected_columns=selected_columns,timesteps = timesteps,filter_bad_in_good = T,date_time_name=date_time_name,verbose=verbose)
      if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
      rm(X_val)
      gc(verbose = F)
      X_val<-rs$data$X
      rm(rs)
      gc(verbose = F)
      rs<-inpute_lstm_median(X=X_val,selected_rows=selected_rows_val,inpute=T,verbose=T)
      if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
      rm(X_val)
      gc(verbose = F)
      X_val<-rs$data$X
      selected_rows_val<-rs$data$selected_rows
      rm(rs)
      gc(verbose = F)
      #10% of train selected for validation
      validation_length<-round(min(length(selected_rows_val),val_size*length(selected_rows)),0)
      validation_alarm_cases<-which(y_val==1)
      validation_ok_cases<-which(y_val==0)
      if(any(validation_alarm_cases %in% selected_rows_val)){
        validation_alarm_cases<-validation_alarm_cases[validation_alarm_cases %in% selected_rows_val]
        validation_ok_cases<-validation_ok_cases[validation_ok_cases %in% selected_rows_val]
        selected_rows_val<-sample(validation_alarm_cases,round(min(validation_length/2,length(validation_alarm_cases)),0))
        selected_rows_val<-c(selected_rows_val,sample(validation_ok_cases,validation_length-length(selected_rows_val)))
      }else{
        selected_rows_val<-sample(validation_ok_cases,validation_length)
      }
      X_val<-X_val[selected_rows_val,,selected_columns]
      y_val<-y_val[selected_rows_val]
    }else{
      #Sort random
      sorted_random<-sample(1:num_rows,size = num_rows,replace = F)
      X_train<-X_train[sorted_random,,]
      y_train<-y_train[sorted_random]
      validation_length<-floor(num_rows*val_size)
      validation_alarm_cases<-floor((sum(y_train)*val_size))
      validation_rows<-sample(which(y_train==1),validation_alarm_cases)
      validation_rows<-c(validation_rows,sample(which(y_train==0),validation_length-length(validation_rows)))
      X_val<-X_train[validation_rows,,]
      train_rows<-which(!((1:num_rows) %in% validation_rows))
      X_train<-X_train[train_rows,,]
      y_val<-y_train[validation_rows]
      y_train<-y_train[train_rows]
    }
  }else{
    X_train<-X[selected_rows,selected_columns]
    if(!is.matrix(X)) X_train<-as.matrix(X_train)
    y_train<-y[selected_rows]
    num_rows<-length(y_train)
    #Val
    if(!is.null(X_val)&&!is.null(y_val)){
      #10% of train selected for validation
      validation_length<-round(min(length(selected_rows_val),val_size*length(selected_rows)),0)
      validation_alarm_cases<-which(y_val==1)
      validation_ok_cases<-which(y_val==0)
      validation_alarm_cases<-validation_alarm_cases[validation_alarm_cases %in% selected_rows_val]
      validation_ok_cases<-validation_ok_cases[validation_ok_cases %in% selected_rows_val]
      selected_rows_val<-sample(validation_alarm_cases,round(min(validation_length/2,length(validation_alarm_cases)),0))
      selected_rows_val<-c(selected_rows_val,sample(validation_ok_cases,validation_length-length(selected_rows_val)))
      X_val<-as.matrix(X_val[selected_rows_val,selected_columns])
      y_val<-y_val[selected_rows_val]
    }else{
      #Sort random
      sorted_random<-sample(1:num_rows,size = num_rows,replace = F)
      X_train<-X_train[sorted_random,]
      y_train<-y_train[sorted_random]
      validation_length<-floor(num_rows*val_size)
      validation_alarm_cases<-floor((sum(y_train)*val_size))
      validation_rows<-sample(which(y_train==1),validation_alarm_cases)
      validation_rows<-c(validation_rows,sample(which(y_train==0),validation_length-length(validation_rows)))
      X_val<-X_train[validation_rows,]
      train_rows<-which(!((1:num_rows) %in% validation_rows))
      X_train<-X_train[train_rows,]
      y_val<-y_train[validation_rows]
      y_train<-y_train[train_rows]
    }
  }
  if(is.null(target_ratio)||is.na(target_ratio)) target_ratio<-floor((length(y_train)/sum(y_train)))
  
  #Free memory
  rm(selected_rows)
  gc(verbose = F)
  
  #Recalculate batchsize if lstm
  if(lstm){
    n_rows<-dim(X_train)[1]
    if(stateful&&((n_rows%%batch_size)!=0)){
      to_copy<-batch_size-(n_rows%%batch_size)
      to_copy<-sample(1:n_rows,size = to_copy,replace = F)
      X_train<-abind(X_train,X_train[to_copy,,],along = 1)
      y_train<-c(y_train,y_train[to_copy])
    }else{
      if(batch_size>=n_rows) batch_size<-n_rows
    }
  }
  
  #X<-as.matrix(X)
  
  #Sample weight
  #sample_weights<-rep(1,length(y))
  #sample_weights[y==0]<-(1-((length(y)-alarm_cases)/length(y)))
  #sample_weights[y==1]<-target_ratio
  #sample_weights[y==1]<-(1-(alarm_cases/length(y)))
  
  #Check if gpu available
  use_device<-'/cpu:0'
  if(use_gpu){
    dl<-tensorflow::import('tensorflow.python.client.device_lib')
    dl<-dl$list_local_devices()
    dl<-sapply(1:length(dl),function(d) dl[[d]]$name)
    found_gpus<-grep(pattern = '/device:GPU|/gpu:0',x = dl,value = T)
    gpu_available<-length(found_gpus)>0
    if(use_gpu&&gpu_available) use_device<-found_gpus[1]
  }
  #if(testing&&lstm&&!is.null(tmpfolder)) save(list=ls(),file=paste0(tmpfolder,'/tmplstm.RData'),compress='xz')
  
  #dANN Classifier
  K <- keras::backend()
  
  if(lstm){
    if(stateful){
      inputs<-keras::layer_input(batch_shape = c(batch_size,dim(X_train)[2],dim(X_train)[3]))
    }else{
      inputs<-keras::layer_input(shape = c(dim(X_train)[2],dim(X_train)[3]))
    }
  }else{
    inputs<-keras::layer_input(shape = as.integer(dim(X_train)[2]))
  }
  
  model<- inputs
  #model<- inputs %>% keras::layer_dropout(rate = ifelse(is.null(input_dropout),0,input_dropout))
  
  for(i in 1:length(architecture)){
    if(i<length(architecture)){
      if(lstm){
        name<-paste0('l',i,'_lstm',architecture[i],'_activation_',activation,ifelse(!is.null(dropout),paste0('_d',dropout),''))
        model<-keras::layer_lstm(name = name ,object = model,units = architecture[i],activation = activation, return_sequences=ifelse(i<length(architecture)-1,T,F),stateful=stateful, dropout=dropout, recurrent_dropout=recurrent_dropout,unroll = T)
      }else{
        name<-paste0('l',i,'_dense',architecture[i])
        model<-keras::layer_dense(object = model,name = name ,units = as.integer(architecture[i]))
        name<-paste0('l',i,'_activation_',activation)
        model<-switch(activation,
                      'sigmoid'=keras::layer_activation(name = name,object = model,activation = 'sigmoid'),
                      'relu'=keras::layer_activation(name = name,object =model,activation = 'relu'),
                      'tanh'=keras::layer_activation(name = name,object =model,activation = 'tanh'),
                      'leaky_relu'=keras::layer_activation_leaky_relu(name = name,object = model))
        if(!is.null(dropout)) model<-keras::layer_dropout(name=paste0('l',i,'_dropout',dropout),object = model,rate = dropout)
      }
    }else{
      #model<-keras::layer_dense(object=model,units = 1,activation = 'sigmoid')
      model<-keras::layer_dense(name=paste0('output_sigmoid'),object=model,units = 1,activation = 'sigmoid')
    }
  }
  
  model<- keras_model(
    inputs = inputs,
    outputs = model
  )
  summary(model)
  
  model %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(lr=lr),
    #optimizer = keras::optimizer_sgd(lr = lr,nesterov = T,momentum=0.05),
    #optimizer = keras::optimizer_adam(lr = 0.0001),
    #optimizer = keras::optimizer_rmsprop(),
    #optimizer = keras::optimizer_rmsprop(lr = 0.00001),
    metrics = list('accuracy',precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
    #weighted_metrics = list('accuracy',precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
  )
  
  cat("\n # Batch size = ",batch_size," number of rows= ",num_rows,"\n")
  
  rm(X)
  gc(verbose = F)
  
  #save(list=c('Xtrain','y','train_rows','X_val','validation_rows'),file = 'tune_lstm.RData',compress='xz')
  with(tf$device(use_device), {
    #sess<-tf$Session(graph = tf$get_default_graph(),config=tf$ConfigProto(allow_soft_placement=T,intra_op_parallelism_threads=as.integer(parallel::detectCores()),inter_op_parallelism_threads=as.integer(parallel::detectCores()),log_device_placement=log_device_placement))
    #K$set_session(sess)
    history <- model %>% keras::fit(x = X_train,y=y_train, epochs = epochs, batch_size = batch_size,class_weight = list('0'=1,'1'=target_ratio),view_metrics = view_metrics,verbose = ifelse(view_progress_bar,1,2),shuffle = ifelse(stateful,F,T),validation_data = list(X_val,y_val)) #callbacks = c(callback_early_stopping(monitor = "val_loss", min_delta=0.01, patience = 50, mode = "auto")) #class_weight = list(0:1,1:500)
  })
  
  if(!is.null(save_model_path)) keras::save_model_hdf5(object=model,filepath=save_model_path,overwrite=T,include_optimizer=T)
  model_serialized<-keras::serialize_model(model=model,include_optimizer = T)
  
  model<-list(lstm=lstm,timesteps=timesteps,min_max_train=min_max_train,predict=predict_ann,architecture=architecture,activation=activation,dropout=dropout,target_ratio=target_ratio,model_columns=selected_columns,history=history,model_serialized=model_serialized,save_model_path=save_model_path)
  
  return(list(error=F,data=model,msg='ok'))
}
