accuracy<-function(y_true, y_pred){
  K <- backend()
  y_pred_pos = K$round(K$clip(y_pred, 0, 1))
  y_pred_neg = 1 - y_pred_pos
  y_pos = K$round(K$clip(y_true, 0, 1))
  y_neg = 1 - y_pos
  tp = K$sum(y_pos * y_pred_pos)
  tn = K$sum(y_neg * y_pred_neg)
  fp = K$sum(y_neg * y_pred_pos)
  fn = K$sum(y_pos * y_pred_neg)
  total =tp+tn+fp+fn+K$epsilon()
  return((tp+tn)/total)
  #return(total)
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

kappa_manual<-function(y_real, y_pred){
  kappa<-NA
  tp<-sum((y_real>0.5)&(y_pred>0.5))
  fp<-sum((y_real<=0.5)&(y_pred>0.5))
  tn<-sum((y_real<=0.5)&(y_pred<=0.5))
  fn<-sum((y_real>0.5)&(y_pred<=0.5))
  total<-sum(tp+tn+fp+fn)
  acc<-(tp+tn)/total
  pres<-tp/(tp+fp)
  ma<-((tp+fp)*(tp+fn))/total
  mb<-((tn+fp)*(tn+fn))/total
  pe<-(ma + mb)/total
  k<-(acc-pe)/(1-pe)
  if(!is.nan(k)) kappa<-k
  return(kappa)
}

acc_manual<-function(y_real, y_pred){
  acc<-NA
  tp<-sum((y_real>0.5)&(y_pred>0.5))
  fp<-sum((y_real<=0.5)&(y_pred>0.5))
  tn<-sum((y_real<=0.5)&(y_pred<=0.5))
  fn<-sum((y_real>0.5)&(y_pred<=0.5))
  total<-sum(tp+tn+fp+fn)
  if(total!=0) acc<-(tp+tn)/total
  return(acc)
}

precision<-function(y_true, y_pred){
  K <- backend()
  true_positives = K$sum(K$round(K$clip(y_true * y_pred, 0, 1)))
  predicted_positives = K$sum(K$round(K$clip(y_pred, 0, 1)))
  precision = true_positives / (predicted_positives + K$epsilon())
  return(precision)
}

precision_manual<-function(y_real, y_pred){
  pres<-NA
  tp<-sum((y_real>0.5)&(y_pred>0.5))
  pp<-sum(y_pred>0.5)
  if(pp!=0) pres <- tp/pp
  return(pres)
}

specificity<-function(y_true, y_pred){
  K <- backend()
  true_negatives = K$sum(K$round(K$clip((1-y_true) * (1-y_pred), 0, 1)))
  possible_negatives = K$sum(K$round(K$clip(1-y_true, 0, 1)))
  return(true_negatives / (possible_negatives + K$epsilon()))
}

specificity_manual<-function(y_real, y_pred){
  spec<-NA
  fp<-sum((y_real<=0.5)&(y_pred>0.5))
  tn<-sum((y_real<=0.5)&(y_pred<=0.5))
  den<-tn+fp
  if(den!=0) spec <- tn/den
  return(spec)
}

sensitivity<-function(y_true, y_pred){
  K <- backend()
  true_positives = K$sum(K$round(K$clip(y_true * y_pred, 0, 1)))
  possible_positives = K$sum(K$round(K$clip(y_true, 0, 1)))
  return(true_positives / (possible_positives + K$epsilon()))
}

sensitivity_manual<-function(y_real, y_pred){
  sen<-NA
  tp<-sum((y_real>0.5)&(y_pred>0.5))
  fn<-sum((y_real>0.5)&(y_pred<=0.5))
  den<-(fn+tp)
  if(den!=0) sen <- tp/den
  return(sen)
}

toggle_trainable<-function(network,state){
  for(l in length(network$layers)){
    network$layers[[l]]$trainable <- state
  }
}

normalize_range<-function(X,selected_rows=NULL,selected_columns=NULL,min=0,max=1,min_max_train=NULL){
  if(is.null(min_max_train)){
    min_max_train<-lapply(which(colnames(X) %in% selected_columns),function(col){
      # Normalize to [0, 1]:
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

predict_ann_adv<-function(model=NULL,X=NULL,selected_rows=NULL,normalize=F,inpute=F,date_time_name='date_time',verbose=F){
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
  model_obj<-keras::unserialize_model(model=model$discriminator_serialized,custom_objects =list(precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa) ,compile=T)
  
  selected_rows<-selected_rows[order(selected_rows)]
  
  #Check if same columns
  if(!all(model$model_columns %in% colnames(X))) return(list(error=T,data=NULL,msg=paste0('missing these variables columns on X:',paste0(model$model_columns[!(model$model_columns %in% colnames(X))],collapse = ','))))
  
  selected_columns<-model$model_columns
  
  #Normalize range
  range<-c(0,1)
  min_max_train<-model$min_max_train
  if(model$activation_discriminator=='tanh') range<-c(-1,1)
  if(model$activation_discriminator=='sigmoid') range<-c(0,1)
  rs<-normalize_range(X,selected_rows=selected_rows,selected_columns=selected_columns,min=range[1],max=range[2],min_max_train=min_max_train)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  X<-rs$data$X
  
  yhat_out<-rep(0,length(selected_rows))
  
  selected_rows_new<-selected_rows
  X<-X[selected_rows_new,selected_columns]
  if(!is.matrix(X)) X<-as.matrix(X)
  
  
  if(verbose) cat("\nPredicting...")
  #Evaluate
  yhat<-model_obj$predict(x=X)
  #pos<-which(selected_rows_new %in% selected_rows)
  yhat_out[selected_rows %in% selected_rows_new]<-yhat
  return(list(error=F,data=list(yhat=yhat_out,selected_rows=selected_rows_new),msg='ok'))
}

deep_ann_gan<-function(X=NULL,y=NULL,selected_rows=NULL,X_val=NULL,y_val=NULL,selected_rows_val=NULL,val_size=0.1,selected_columns=NULL,lr=0.001,epochs=500,kappa_validation_stop=0.8,batch_size=length(y),architecture_discriminator=c(50,20,10,1),architecture_generator=c(50,20,10,1),activation_discriminator='tanh',activation_generator='leaky_relu',dropout_discriminator=NULL,dropout_generator=NULL,target_ratio=NULL,view_metrics=F,view_progress_bar=F,use_gpu=F,save_model_path=NULL,log_device_placement=F,date_time_name='date_time',inpute=T,normalize=F,logfile=NULL,stateful=F,testing=F,tmpfolder=NULL,verbose=F){
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
  
  #Change target variable to numeric 0/1
  if(is.logical(y)) y<-as.numeric(y)
  if(!(is.logical(y)||is.numeric(y))||(is.numeric(y)&&((length(unique(y))!=2)||!all(c(0,1) %in% unique(y))))) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable y must be or logical or numeric with 0/1")))
  
  selected_rows<-selected_rows[order(selected_rows)]
  #Sort columns by selected
  selected_columns<-selected_columns[order(selected_columns)]
  
  #Normalize range
  range<-c(0,1)
  if(activation_discriminator=='tanh') range<-c(-1,1)
  if(activation_discriminator=='sigmoid') range<-c(0,1)
  rs<-normalize_range(X,selected_rows=selected_rows,selected_columns=selected_columns,min=range[1],max=range[2])
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  X<-rs$data$X
  min_max_train<-rs$data$min_max_train
  
  if(!is.null(X_val)&&!is.null(y_val)){
    #Normalize range
    rs<-normalize_range(X_val,selected_rows=selected_rows_va,selected_columns=selected_columns,min=range[1],max=range[2],min_max_train=min_max_train)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    X_val<-rs$data$X
    if(is.logical(y_val)) y_val<-as.numeric(y_val)
  }
  
  
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
  
  if(is.null(target_ratio)||is.na(target_ratio)) target_ratio<-floor((length(y_train)/sum(y_train)))
  
  #Free memory
  rm(selected_rows)
  gc(verbose = F)
  
  
  #if((activation!='sigmoid')&&(activation!='tanh')) activation_out<-'sigmoid'
  
  
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
  
  #TODO
  #activation<-'sigmoid'
  #activation<-'relu'
  #activation_out <- 'tanh'
  #batch_size<-sum(y_train==1)
  K <- keras::backend()
  
  #===Discriminator
  discriminator_inputs<-keras::layer_input(shape = as.integer(dim(X_train)[2]))
  discriminator<-discriminator_inputs
  for(i in 1:length(architecture_discriminator)){
    if(i<length(architecture_discriminator)){
      name<-paste0('l',i,'_dense',architecture_discriminator[i])
      discriminator<-keras::layer_dense(object = discriminator,name = name ,units = as.integer(architecture_discriminator[i]))
      discriminator<-keras::layer_batch_normalization(object = discriminator,name = paste0('l',i,'_batchnormalization'))
      name<-paste0('l',i,'_activation_',activation_discriminator)
      discriminator<-switch(activation_discriminator,
                            'sigmoid'=keras::layer_activation(name = name,object = discriminator,activation = 'sigmoid'),
                            'relu'=keras::layer_activation(name = name,object =discriminator,activation = 'relu'),
                            'tanh'=keras::layer_activation(name = name,object =discriminator,activation = 'tanh'),
                            'leaky_relu'=keras::layer_activation_leaky_relu(name = name,object = discriminator))
      if(!is.null(dropout_discriminator)) discriminator<-keras::layer_dropout(name=paste0('l',i,'_dropout',dropout_discriminator),object = discriminator,rate = dropout_discriminator)
    }else{
      #model<-keras::layer_dense(object=model,units = 1,activation = 'sigmoid')
      discriminator<-keras::layer_dense(name=paste0('output_sigmoid'),object=discriminator,units = 1,activation = 'sigmoid')
    }
  }
  discriminator<- keras_model(inputs = discriminator_inputs,outputs = discriminator)
  discriminator %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(lr=lr),
    metrics = list(accuracy=accuracy,precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
  )
  summary(discriminator)
  #----Discriminator
  
  # #Discriminator
  # discriminator<-keras::keras_model_sequential()
  # discriminator<-keras::layer_dense(name='discriminator1',object = discriminator ,units =10,input_shape=as.integer(dim(X_train)[2]),kernel_initializer='random_uniform')
  # #discriminator<-keras::layer_batch_normalization(object = discriminator,name = 'discriminator1_normalization',batch_size = batch_size)
  # discriminator<-keras::layer_activation(object=discriminator,activation = activation)
  # discriminator<-keras::layer_dropout(name='discriminator1_dropout',object = discriminator,rate = 0.2)
  # discriminator<-keras::layer_dense(name='discriminator2',object = discriminator ,units =10,kernel_initializer='random_uniform')
  # discriminator<-keras::layer_batch_normalization(object = discriminator,name = 'discriminator2_normalization',batch_size = batch_size)
  # discriminator<-keras::layer_activation(object=discriminator,activation = activation)
  # discriminator<-keras::layer_dropout(name='discriminator2_dropout',object = discriminator,rate = 0.2)
  # discriminator<-keras::layer_dense(name='discriminator_out',object = discriminator ,units =1,activation=activation_out,kernel_initializer='random_uniform')
  # summary(discriminator)
  # discriminator %>% keras::compile(
  #   loss = 'binary_crossentropy',
  #   optimizer = keras::optimizer_adam(),
  #   metrics = list('accuracy',precision=precision,specificity=specificity,sensitivity=sensitivity)
  # )
  # 
  
  
  #===Generator
  generator_inputs<-keras::layer_input(shape = as.integer(dim(X_train)[2]))
  generator<-generator_inputs
  for(i in 1:length(architecture_generator)){
    if(i<length(architecture_generator)){
      name<-paste0('l',i,'_dense',architecture_generator[i])
      generator<-keras::layer_dense(object = generator,name = name ,units = as.integer(architecture_generator[i]))
      generator<-keras::layer_batch_normalization(object = generator,name = paste0('l',i,'_batchnormalization'))
      name<-paste0('l',i,'_activation_',activation_generator)
      generator<-switch(activation_generator,
                        'sigmoid'=keras::layer_activation(name = name,object = generator,activation = 'sigmoid'),
                        'relu'=keras::layer_activation(name = name,object =generator,activation = 'relu'),
                        'tanh'=keras::layer_activation(name = name,object =generator,activation = 'tanh'),
                        'leaky_relu'=keras::layer_activation_leaky_relu(name = name,object = generator))
      if(!is.null(dropout_generator)) generator<-keras::layer_dropout(name=paste0('l',i,'_dropout',dropout_generator),object = generator,rate = dropout_generator)
    }else{
      generator<-keras::layer_dense(name=paste0('output_dense'),object=generator,units = as.integer(dim(X_train)[2]))
      name<-paste0('output_activation_',activation_generator)
      generator<-switch(activation_generator,
                        'sigmoid'=keras::layer_activation(name = name,object = generator,activation = 'sigmoid'),
                        'relu'=keras::layer_activation(name = name,object =generator,activation = 'relu'),
                        'tanh'=keras::layer_activation(name = name,object =generator,activation = 'tanh'),
                        'leaky_relu'=keras::layer_activation_leaky_relu(name = name,object = generator))
    }
  }
  generator<- keras_model(inputs = generator_inputs,outputs = generator)
  generator %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(lr=lr),
    metrics = list(accuracy=accuracy,precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
  )
  summary(generator)
  #----Generator
  
  # #Generator
  # generator<-keras::keras_model_sequential()
  # generator<-keras::layer_dense(object=generator,name='generator1' ,units =as.integer(dim(X_train)[2]),input_shape=as.integer(dim(X_train)[2]),kernel_initializer='random_uniform')
  # #generator<-keras::layer_batch_normalization(object = generator,name = 'generator1_normalization')
  # generator<-keras::layer_activation(object=generator,activation = activation,name='generator1_activation')
  # generator<-keras::layer_dense(name='generator2',object = generator ,units =as.integer(dim(X_train)[2]),kernel_initializer='random_uniform')
  # #generator<-keras::layer_batch_normalization(object = generator,name = 'generator2_normalization',batch_size = batch_size)
  # generator<-keras::layer_activation(object=generator,activation = activation,name='generator2_activation')
  # generator<-keras::layer_dense(name='generator_out',object = generator ,units=as.integer(dim(X_train)[2]),kernel_initializer='normal')
  # #generator<-keras::layer_batch_normalization(object = generator,name = 'generator_out_normalization',batch_size = batch_size)
  # generator<-keras::layer_activation(object=generator,activation = activation_out,name='generator_out_activation')
  # generator %>% keras::compile(
  #   loss = 'binary_crossentropy',
  #   optimizer = keras::optimizer_adam(),
  #   metrics = list('accuracy',precision=precision,specificity=specificity,sensitivity=sensitivity)
  # )
  # summary(generator)
  
  stopped_by<-'iterations'
  
  #Adversarial model
  adversarial<-keras::keras_model_sequential()
  adversarial$add(layer=generator)
  adversarial$add(layer=discriminator)
  adversarial %>% keras::compile(
    loss = 'binary_crossentropy',
    optimizer = keras::optimizer_adam(),
    metrics = list(accuracy=accuracy,precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa)
  )
  summary(adversarial)
  
  
  iterations<-epochs
  ok_cases<-which(y_train==0)
  alarm_cases<-which(y_train==1)
  n_alarm_cases<-length(alarm_cases)
  #batch_size<-20
  noise <- X_train[ok_cases,]
  batch_size<-dim(noise)[1]
  real_X<-X_train[alarm_cases,]
  for(i in 1:iterations){
    noise<-X_train[sample(x = ok_cases,size =floor(n_alarm_cases/2),replace = F),]
    tmp_real<-X_train[sample(x = alarm_cases,size =floor(n_alarm_cases/2),replace = F),]
    #real_y<- runif(n = n_alarm_cases,min = 0.7,max = 1)
    real_y<-rep(1,floor(n_alarm_cases/2))
    #fake_y<- runif(n = n_alarm_cases,min = 0,max = 0.3)
    fake_y<-rep(0,floor(n_alarm_cases/2))
    fake_X<-generator$predict_on_batch(noise)
    discriminator$trainable<-T
    toggle_trainable(discriminator,T)
    #Real train
    cat('\nTraining discriminator...')
    lr<-keras::fit(object=discriminator,x = rbind(tmp_real,fake_X), y=c(real_y,fake_y),epochs = 500,batch_size =batch_size,verbose = verbose,view_metrics = F
                   #class_weight = list('0':1,'1':0.5)
    )
    #pr<-discriminator$predict_on_batch(rbind(tmp_real,fake_X))
    lr<-c(lr$metrics$loss[length(lr$metrics$loss)],lr$metrics$accuracy[length(lr$metrics$accuracy)],lr$metrics$precision[length(lr$metrics$precision)],lr$metrics$specificity[length(lr$metrics$specificity)],lr$metrics$sensitivity[length(lr$metrics$sensitivity)],lr$metrics$kappa[length(lr$metrics$kappa)])
    discriminator$trainable<-F
    toggle_trainable(discriminator,F)
    
    #la<-adversarial$train_on_batch(noise, real_y)
    if(!verbose)
      cat('adversarial...')
    else
      cat('\nTraining adversarial...')
    la<-adversarial %>% keras::fit(x = noise,y = real_y,epochs = 100,batch_size =batch_size,verbose = verbose,view_metrics = F)
    #pa<-adversarial$predict_on_batch(noise)
    #la<-c(min(la$metrics$loss),acc_manual(fake_y,pa),precision_manual(fake_y,pa),specificity_manual(fake_y,pa),sensitivity_manual(fake_y,pa),kappa_manual(fake_y,pa))
    la<-c(la$metrics$loss[length(la$metrics$loss)],la$metrics$accuracy[length(la$metrics$accuracy)],la$metrics$precision[length(la$metrics$precision)],la$metrics$specificity[length(la$metrics$specificity)],la$metrics$sensitivity[length(la$metrics$sensitivity)],la$metrics$kappa[length(la$metrics$kappa)])
    
    #Validation
    pv<-discriminator$predict(x = X_val)
    k<-kappa_manual(y_val,pv)
    val_met<-c(acc_manual(y_val,pv),precision_manual(y_val,pv),specificity_manual(y_val,pv),sensitivity_manual(y_val,pv),k)
    cat('\n',sprintf("%2.1f",round(i*100/iterations,1)),'%:')
    
    msg<-paste0('[Dis loss:',sprintf("%.3f", round(lr[[1]],3)),
                ' acc:',sprintf("%.3f", round(lr[[2]],3)),
                ' pres:',sprintf("%.3f", round(lr[[3]],3)),
                ' spec:',sprintf("%.3f", round(lr[[4]],3)),
                ' sen:',sprintf("%.3f", round(lr[[5]],3)),
                ' kap:',sprintf("%.3f", round(lr[[6]],3)),'] ')
    msg<-paste0(msg,': [Adv (1 fake), loss:',sprintf("%.3f", round(la[[1]],3)),
                ' acc:',sprintf("%.3f", round(la[[2]],3)),
                ' pres:',sprintf("%.3f", round(la[[3]],3)),
                ' spec:',sprintf("%.3f", round(la[[4]],3)),
                ' sen:',sprintf("%.3f", round(la[[5]],3)),
                ' kap:',sprintf("%.3f", round(la[[6]],3)),'] ')
    msg<-paste0(msg,': [VAL acc:',sprintf("%.3f", round(val_met[1],3)),
                ' pres:',sprintf("%.3f", round(val_met[2],3)),
                ' spec:',sprintf("%.3f", round(val_met[3],3)),
                ' sen:',sprintf("%.3f", round(val_met[4],3)),
                ' kap:',sprintf(" %.3f", round(val_met[5],3)),'] ')
    cat(msg)
    if(k>=kappa_validation_stop){
      stopped_by<-'kappa'
      break #stop with validation kappa of 0.8
    }
  }
  if(stopped_by=='kappa')
    cat('\n Stopped with Kappa>=',kappa_validation_stop,' on validation')
  else
    cat('\n Stopped by max iterations (',epochs,')')

  if(!is.null(save_model_path)) keras::save_model_hdf5(object=model,filepath=save_model_path,overwrite=T,include_optimizer=T)
  adversarial_serialized<-keras::serialize_model(model=adversarial,include_optimizer = T)
  generator_serialized<-keras::serialize_model(model=generator,include_optimizer = T)
  discriminator_serialized<-keras::serialize_model(model=discriminator,include_optimizer = T)
  
  model<-list(stopped_by=stopped_by,kappa_validation_stop=kappa_validation_stop,min_max_train=min_max_train,predict=predict_ann_adv,architecture_discriminator=architecture_discriminator,architecture_generator=architecture_generator,activation_generator=activation_generator,activation_discriminator=activation_discriminator,dropout_discriminator=dropout_discriminator,dropout_generator=dropout_generator,target_ratio=target_ratio,model_columns=selected_columns,history=history,adversarial_serialized=adversarial_serialized,generator_serialized=generator_serialized,discriminator_serialized=discriminator_serialized,save_model_path=save_model_path)
  
  return(list(error=F,data=model,msg='ok'))
}
