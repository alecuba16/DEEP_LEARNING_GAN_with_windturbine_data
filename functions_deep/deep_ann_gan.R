prepare_conv<-function(wtdata=NULL,y=NULL,selected_rows=NULL,selected_columns=NULL,exclude_columns=c(date_time_name,'ld_id'),date_time_name='date_time',logfile=NULL,verbose=F,timesteps=15){
  #Only use selected columns,ld_id and date_time
  wtdata<-wtdata[,colnames(wtdata) %in% c(selected_columns,'ld_id',date_time_name)]
  sorted<-order(wtdata$ld_id,wtdata$date_time)
  wtdata<-wtdata[sorted,]
  if(!is.null(y)) y<-y[sorted]
  
  
  dim1<-length(selected_rows)
  dim1_names<-sort(selected_rows)
  
  ld_ids<-sort(unique(wtdata$ld_id[selected_rows]))
  
  #dt<-as.numeric(sort(unique(wtdata$date_time)))
  #dim1<-length(ld_ids)
  #dim2<-length(dt)
  dim3_names<-sort(selected_columns)
  dim3<-length(dim3_names)
  #[VARS,TIME,TURBINES]

  X<-array(NA,dim = c(dim1,timesteps,dim3),dimnames = list(dim1_names,paste0('t-',0:(timesteps-1)),dim3_names))
  filled_pos_absolute<-NULL
  filled_pos_relative<-NULL
  na_rows<-NULL
  for(pos in dim1_names){
    relative_pos<-which(dim1_names==pos)
    if((relative_pos>=timesteps)
       &&(length(unique(wtdata$ld_id[(pos-timesteps+1):pos]))==1)
       &&all(!is.na(wtdata[(pos-timesteps+1):pos,selected_columns]))){
      filled_pos_absolute<-c(filled_pos_absolute,pos)
      filled_pos_relative<-c(filled_pos_relative,relative_pos)
      
      for(i in 0:(timesteps-1)){
        X[relative_pos,i+1,]<-as.numeric(wtdata[(pos-i),selected_columns])  
      }
    }
  }
  
  #Remove incomplete X,y
  selected_rows<-dim1_names[dim1_names %in% filled_pos_absolute]
  X<-X[filled_pos_relative,,]
  if(!is.null(y)) y<-y[selected_rows]
  
  return(list(error=F,data=list(X=X,y=y,selected_rows=sort(selected_rows))))
}


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

kappa_manual<-function(y_real, y_pred,threshold=0.5){
  kappa<-NA
  tp<-sum((y_real>threshold)&(y_pred>threshold))
  fp<-sum((y_real<=threshold)&(y_pred>threshold))
  tn<-sum((y_real<=threshold)&(y_pred<=threshold))
  fn<-sum((y_real>threshold)&(y_pred<=threshold))
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

acc_manual<-function(y_real, y_pred,threshold=0.5){
  acc<-NA
  tp<-sum((y_real>threshold)&(y_pred>threshold))
  fp<-sum((y_real<=threshold)&(y_pred>threshold))
  tn<-sum((y_real<=threshold)&(y_pred<=threshold))
  fn<-sum((y_real>threshold)&(y_pred<=threshold))
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

precision_manual<-function(y_real, y_pred,threshold=0.5){
  pres<-NA
  tp<-sum((y_real>threshold)&(y_pred>threshold))
  pp<-sum(y_pred>threshold)
  if(pp!=0) pres <- tp/pp
  return(pres)
}

specificity<-function(y_true, y_pred){
  K <- backend()
  true_negatives = K$sum(K$round(K$clip((1-y_true) * (1-y_pred), 0, 1)))
  possible_negatives = K$sum(K$round(K$clip(1-y_true, 0, 1)))
  return(true_negatives / (possible_negatives + K$epsilon()))
}

specificity_manual<-function(y_real, y_pred,threshold=0.5){
  spec<-NA
  fp<-sum((y_real<=threshold)&(y_pred>threshold))
  tn<-sum((y_real<=threshold)&(y_pred<=threshold))
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

sensitivity_manual<-function(y_real, y_pred,threshold=0.5){
  sen<-NA
  tp<-sum((y_real>threshold)&(y_pred>threshold))
  fn<-sum((y_real>threshold)&(y_pred<=threshold))
  den<-(fn+tp)
  if(den!=0) sen <- tp/den
  return(sen)
}

toggle_trainable<-function(network,state){
  for(l in length(network$layers)){
    network$layers[[l]]$trainable <- state
  }
}

build_generator<-function(timesteps,nrows,nfeatures,architecture_generator,activation_generator,activation_discriminator,optimizer,dropout_generator){
  method<-'same'
  kernel_size<-ceiling(0.75*timesteps)
  #kernel_size<-5
  #strides<-kernel_size #No overlap
  strides<-1
  if(method=='valid')
    nfilters<-floor(timesteps/(kernel_size-(kernel_size-strides)))
  else
    nfilters<-ceiling(timesteps/(kernel_size-(kernel_size-strides)))
  
  input<-keras::layer_input(shape=c(timesteps,nfeatures))
  conv<-keras::layer_separable_conv_1d(object = input,padding = method,filters = nfeatures,strides = strides,kernel_size = kernel_size,depth_multiplier = 5,name = 'conv1d')
  model<-keras::layer_batch_normalization(object = conv)
  model<-keras::layer_activation(object = model,activation = 'relu')
  #model<-keras::layer_max_pooling_1d(object = model,name = 'maxpooling')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_conv_1d(object = model,padding = method,filters = floor(nfeatures/2),strides = strides,kernel_size = floor(kernel_size/2),name = 'conv1d_2')
  model<-keras::layer_batch_normalization(object = model)
  model<-keras::layer_activation(object = model,activation = 'relu')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_conv_1d(object = model,padding = method,filters = floor(nfeatures/4),strides = strides,kernel_size = floor(kernel_size/4),name = 'conv1d_3')
  model<-keras::layer_batch_normalization(object = model)
  model<-keras::layer_activation(object = model,activation = 'relu')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_flatten(object = model,name='flatten')
  
  model<-keras::layer_dense(activation = 'tanh',object = model,units = nfeatures*timesteps)
  model <-keras::layer_reshape(object = model,target_shape = c(timesteps,nfeatures),input_shape = nfeatures*timesteps)
  model <- keras::keras_model(inputs = input,outputs = model)
  
  cat('\n################### Generator summary ini ############################\n')
  summary(object = model)
  cat('\n################### Generator summary end ############################\n')
  
  return(model)
}

build_discriminator<-function(timesteps,nrows,nfeatures,architecture_discriminator,activation_discriminator,optimizer){
  method<-'same'
  kernel_size<-ceiling(0.75*timesteps)
  #kernel_size<-5
  #strides<-kernel_size #No overlap
  strides<-1
  if(method=='valid')
    nfilters<-floor(timesteps/(kernel_size-(kernel_size-strides)))
  else
    nfilters<-ceiling(timesteps/(kernel_size-(kernel_size-strides)))
  
  input<-keras::layer_input(shape=c(timesteps,nfeatures))
  conv<-keras::layer_separable_conv_1d(object = input,padding = method,filters = nfeatures,strides = strides,kernel_size = kernel_size,depth_multiplier = 5,name = 'conv1d')
  model<-keras::layer_batch_normalization(object = conv)
  model<-keras::layer_activation(object = model,activation = 'relu')
  #model<-keras::layer_max_pooling_1d(object = model,name = 'maxpooling')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_conv_1d(object = model,padding = method,filters = floor(nfeatures/2),strides = strides,kernel_size = floor(kernel_size/2),name = 'conv1d_2')
  model<-keras::layer_batch_normalization(object = model)
  model<-keras::layer_activation(object = model,activation = 'relu')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_conv_1d(object = model,padding = method,filters = floor(nfeatures/4),strides = strides,kernel_size = floor(kernel_size/4),name = 'conv1d_3')
  model<-keras::layer_batch_normalization(object = model)
  model<-keras::layer_activation(object = model,activation = 'relu')
  model<-keras::layer_dropout(object = model,rate = 0.2)
  
  model<-keras::layer_flatten(object = model,name='flatten')
  #model<-keras::layer_flatten(object = conv)
  #model<-keras::layer_reshape(object=conv,target_shape=c(strides,nfeatures),input_shape = c(strides,nfeatures,1))
  #model <- keras::layer_dropout(object = model,rate = 0.2)
  output <- keras::layer_dense(object = model,units = 1,activation = 'sigmoid')
  
  model <- keras::keras_model(inputs = input,outputs = output)
  conv_model <- keras::keras_model(inputs = input,outputs = conv)
  model %>% keras::compile(loss= 'binary_crossentropy',
                           optimizer=optimizer,
                           metrics= list(accuracy=accuracy,precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa))
  cat('\n################### Discriminator summary ini ############################\n')
  summary(object = model)
  cat('\n################### Discriminator summary end ############################\n')
  #convout1_f <- keras::k_function(c(keras::k_learning_phase(),input), c(conv_model$output))
  convout1_f <- keras::k_function(c(input), c(conv_model$output))
  return(list(model=model,convout1_f=convout1_f))
}

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
  
  rs<-prepare_conv(wtdata = X,y=NULL,selected_rows = selected_rows,selected_columns=selected_columns,timesteps=model$timesteps,verbose=verbose)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  rm(X)
  gc(verbose = F)
  X<-rs$data$X
  selected_rows_new<-rs$data$selected_rows
  rm(rs)
  gc(verbose = F)
  
  yhat_out<-rep(0,length(selected_rows))
  
  #selected_rows_new<-selected_rows
  #X<-X[selected_rows_new,selected_columns]
  #if(!is.matrix(X)) X<-as.matrix(X)
  
  
  if(verbose) cat("\nPredicting...")
  #Evaluate
  yhat<-model_obj$predict(x=X)
  #pos<-which(selected_rows_new %in% selected_rows)
  yhat_out[selected_rows %in% selected_rows_new]<-yhat
  return(list(error=F,data=list(yhat=yhat_out,selected_rows=selected_rows_new),msg='ok'))
}

deep_ann_gan<-function(X=NULL,y=NULL,selected_rows=NULL,X_val=NULL,y_val=NULL,selected_rows_val=NULL,val_size=0.1,selected_columns=NULL,lr=0.001,epochs=500,kappa_validation_stop=0.25,disc_to_gen_ratio=15,batch_size=length(y),architecture_discriminator=c(50,20,10,1),architecture_generator=c(50,20,10,1),activation_discriminator='tanh',activation_generator='leaky_relu',dropout_discriminator=NULL,dropout_generator=NULL,target_ratio=NULL,view_metrics=F,view_progress_bar=F,use_gpu=F,save_model_path=NULL,log_device_placement=F,date_time_name='date_time',inpute=T,normalize=F,logfile=NULL,stateful=F,testing=F,tmpfolder=NULL,verbose=F,timesteps=15,print_roc=T,bin_threshold=0.5){
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
  rs<-normalize_range(X,selected_rows=selected_rows,selected_columns=selected_columns,min=range[1],max=range[2],lstm=T)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  X<-rs$data$X
  min_max_train<-rs$data$min_max_train
  
  if(!is.null(X_val)&&!is.null(y_val)){
    #Normalize range
    rs<-normalize_range(X_val,selected_rows=selected_rows_va,selected_columns=selected_columns,min=range[1],max=range[2],lstm=T,min_max_train=min_max_train)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    X_val<-rs$data$X
    if(is.logical(y_val)) y_val<-as.numeric(y_val)
  }
  
  rs<-prepare_conv(wtdata = X,y=y,selected_rows = selected_rows,selected_columns=selected_columns,timesteps=timesteps,date_time_name=date_time_name,verbose=verbose)
  if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
  rm(X)
  rm(y)
  gc(verbose = F)
  X_train<-rs$data$X
  y_train<-rs$data$y
  rm(rs)
  gc(verbose = F)
  num_turbines<-dim(y_train)[1]
  #Val
  if(!is.null(X_val)&&!is.null(y_val)){
    rs<-prepare_conv(wtdata = X_val,y=y_val,selected_rows = selected_rows_val,selected_columns=selected_columns,timesteps=timesteps,date_time_name=date_time_name,verbose=verbose)
    if(rs$error) return(list(error=rs$error,data=NULL,msg=rs$msg))
    rm(X_val)
    rm(y_val)
    gc(verbose = F)
    X_val<-rs$data$X
    y_val<-rs$data$y
  }else{
    #Sort random
    sorted_random<-sample(1:num_turbines,size = num_turbines,replace = F)
    X_val<-X_train[!sorted_random,,]
    y_val<-y_train[!sorted_random,]
    X_train<-X_train[sorted_random,,]
    y_train<-y_train[sorted_random]
  }
  
  if(is.null(target_ratio)||is.na(target_ratio)) target_ratio<-floor((dim(y_train)[1]*dim(y_train)[2]/sum(y_train)))
  
  #Free memory
  #rm(selected_rows,X)
  #gc(verbose = F)
  
  #Prepare train, test data for convolution
  #X_train<-array(data = c(X_train[1],X_train[1],X_train[3]),dim = c(dim(X_train)[2],dim(X_train)[1],dim(X_train)[3]),dimnames = list(names(X_train[1,,1]),names(X_train[,1,1]),names(X_train[1,1,])))
  #X_val<-array(data = c(X_val[2],X_val[1],X_val[3]),dim = c(dim(X_val)[2],dim(X_val)[1],dim(X_val)[3]),dimnames = list(names(X_val[1,,1]),names(X_val[,1,1]),names(X_val[1,1,])))
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
  
  ### Init ###
  
  # Input shape
  optimizer <- keras::optimizer_adam(lr=lr)
  # Build and compile the discriminator
  rs <- build_discriminator(timesteps = dim(X_train)[2],nrows = dim(X_train)[1],nfeatures=dim(X_train)[3],architecture_discriminator,activation_discriminator,optimizer)
  discriminator<-rs$model
  convout1_f<-rs$convout1_f
  # Build the generator
  generator <- build_generator(timesteps = dim(X_train)[2],nrows = dim(X_train)[1],nfeatures=dim(X_train)[3],architecture_generator,activation_generator,activation_discriminator,optimizer,dropout_generator)
  
  # The generator takes noise and the target label as input
  # and generates the corresponding digit of that label
  noise <- keras::layer_input(shape=c(dim(X_train)[2],dim(X_train)[3]))
  generated_data <- generator(noise)
  
  # For the combined model we will only train the generator
  discriminator$trainable = F
  
  # The discriminator takes generated image as input and determines validity
  # and the label of that image
  valid <- discriminator(generated_data)
  
  # The combined model  (stacked generator and discriminator)
  # Trains generator to fool discriminator
  combined <- keras::keras_model(inputs = noise, outputs = valid)
  combined %>% keras::compile(loss= 'binary_crossentropy',
                              optimizer=optimizer,
                              metrics= list(accuracy=accuracy,precision=precision,specificity=specificity,sensitivity=sensitivity,kappa=kappa))
  
  ### Train loop  ###
  rocs<-list()
  
  stopped_by<-'iterations'
  la<-NULL
  ld<-NULL
  print_roc<-T
  #X_val<-array(X_val,dim = c(dim(X_val)[1],dim(X_val)[2],1))
  alarms_ids<-which(y_train==1)
  n_alarms<-length(alarms_ids)
  num_alarms_to_train<-round(0.75*n_alarms,0)
  for(epoch in 1:epochs){
    # Select a random half batch of images
    idxa<-sample(x = 1:n_alarms,size = num_alarms_to_train,replace = T)
    idxa<-alarms_ids[idxa]
    idxa<-unique(idxa)
    real_data_alarm<-X_train[idxa,,]
    
    idxh <- sample(x = which(y_train==0),size = min(length(idxa),sum(y_train==0)))
    #real_data_alarm<-array(X_train[idxa,],dim = c(length(idxa),ncol(X_train)))
    #real_data_healthy<-array(X_train[idxh,],dim = c(length(idxh),ncol(X_train)))
    real_data_healthy<-X_train[idxh,,]
    labels_alarm<-runif(n = length(idxa),min = 0.7,max=1.0)
    #labels_alarm<-rep(1,length(idxa))
    labels_healthy<- runif(n = length(idxh),min = 0.0,max=0.3)
    #labels_healthy<-rep(0,length(idxh))
    labels_healthy_fake<-runif(n = length(idxa),min = 0.0,max=0.3)
    #labels_healthy_fake<-rep(0,length(idxh))
    # Sample noise as generator input
    #noise <- runif(n = batch_size*ncol_noise,min = 0,max = 1)
    #noise <- array(noise,dim=c(batch_size,ncol_noise))
    idx <- sample(x = which(y_train==0),size = length(labels_healthy_fake))
    noise<-X_train[idx,,]
    # Generate a half batch of new data
    generated_data = generator$predict(noise)
    #tmp_X<-abind::abind(array(real_data_alarm,dim = c(dim(real_data_alarm)[1],dim(real_data_alarm)[2],1)),array(real_data_healthy,dim = c(dim(real_data_healthy)[1],dim(real_data_healthy)[2],1)),generated_data,along = 1)
    tmp_X<-abind::abind(real_data_alarm,real_data_healthy,generated_data,along = 1)
    #tmp_X<-abind::abind(real_data_alarm,real_data_healthy,along = 1)
    real_data_X<-abind::abind(real_data_alarm,real_data_healthy,along = 1)
    #tmp_X<-rbind(real_data_alarm,real_data_healthy,generated_data)
    tmp_y<-c(labels_alarm,labels_healthy,labels_healthy_fake)
    #tmp_y<-c(labels_alarm,labels_healthy)
    real_data_y<-c(labels_alarm,labels_healthy)
    
    sample_weight=c(rep(1,length(labels_alarm)),rep(1/target_ratio,length(labels_healthy)+length(labels_healthy_fake)))
    #sample_weight=c(rep(1,length(labels_alarm)),rep(1/target_ratio,length(labels_healthy)))
    #tmp_y<-array(tmp_y,dim=c(length(tmp_y),1))
    #By default train both
    train_g<-T
    train_d<-T
    
    if(!is.null(la)&&!is.null(ld)){
      minl<-min(la[1],ld[1])
      maxl<-max(la[1],ld[1])
      v<-(minl*(disc_to_gen_ratio/100))+minl
      if(ld>la){#Gen wins
        if(ld>=v) train_g<-F
      }else{
        if(la>=v) train_d<-F
      }
    }

    if(train_d){
      cat('\nTraining disc...')
      # Train the discriminator
      discriminator$trainable<-T
      toggle_trainable(discriminator,T)
      generator$trainable<-F
      toggle_trainable(generator,F)
      #tmp_X<-array(tmp_X,dim = c(dim(tmp_X)[1],dim(tmp_X)[2],1))
      #Train real Alarm
      #ld <- keras::fit(object=discriminator,x = real_data_alarm,y = labels_alarm,verbose=0,batch_size = min(dim(real_data_alarm)[1],batch_size),epochs = 500,shuffle = T,sample_weight=rep(target_ratio,length(labels_alarm)))
      #Get the convoluted result
      #a<-convout1_f(list(real_data_alarm))[[1]]
      #w<-keras::get_weights(discriminator$get_layer(name = 'conv1d'))[[1]]
      #b<-keras::get_weights(discriminator$get_layer(name = 'conv1d'))[[2]]
      #Train real Healthy
      #ld <- keras::fit(object=discriminator,x = real_data_healthy,y = labels_healthy,verbose=0,batch_size = min(dim(real_data_healthy)[1],batch_size),epochs = 500,shuffle = T,sample_weight=rep(1/target_ratio,length(labels_healthy)))
      #Train fake alarm
      #ld <- keras::fit(object=discriminator,x = generated_data,y = labels_healthy_fake,verbose=0,batch_size = min(dim(generated_data)[1],batch_size),epochs = 500,shuffle = T,sample_weight=rep(1/target_ratio,length(labels_healthy_fake)))
      
      ld <- keras::fit(object=discriminator,x = tmp_X,y = tmp_y,verbose=0,batch_size = batch_size,epochs = 500,shuffle = T,sample_weight=sample_weight)
      #d_loss_real = discriminator$train_on_batch(list(real_data_X, real_data_y), valid)
      #d_loss_fake = discriminator$train_on_batch(list(generated_data, labels_healthy_fake), fake)
      discriminator$trainable<-F
      toggle_trainable(discriminator,F)
      generator$trainable<-T
      toggle_trainable(generator,T)
      #d_loss = 0.5*(d_loss_real+d_loss_fake)
      #ld<-c(ld$metrics$loss[ld$params$epochs],ld$metrics$accuracy[ld$params$epochs],ld$metrics$precision[ld$params$epochs],ld$metrics$specificity[ld$params$epochs],ld$metrics$sensitivity[ld$params$epochs],ld$metrics$kappa[ld$params$epochs])
    }else{
      cat('\nEvaluating disc...')
    }
    ld<-keras::evaluate(object = discriminator,x=tmp_X,y=tmp_y,verbose=0)
    ld<-c(ld$loss,ld$accuracy,ld$precision,ld$specificity,ld$sensitivity,ld$kappa)
    
    labels_alarms<-runif(n = dim(noise)[1],min = 0.7,max=1.0)
    if(train_g){
      cat(' Training Generator...')
      # ---------------------
      #  Train Generator
      # ---------------------
      #labels_alarms <-rep(1,dim(noise)[1])
      # Train the generator
      discriminator$trainable<-F
      toggle_trainable(discriminator,F)
      generator$trainable<-T
      toggle_trainable(generator,T)
      la = keras::fit(object = combined,x = noise, y=labels_alarms,verbose = 0,epochs = 500,batch_size = batch_size,shuffle = T)
      discriminator$trainable<-T
      toggle_trainable(discriminator,T)
      generator$trainable<-F
      toggle_trainable(generator,F)
      la<-c(la$metrics$loss[length(la$metrics$loss)],la$metrics$accuracy[length(la$metrics$accuracy)],la$metrics$precision[length(la$metrics$precision)],la$metrics$specificity[length(la$metrics$specificity)],la$metrics$sensitivity[length(la$metrics$sensitivity)],la$metrics$kappa[length(la$metrics$kappa)])
    }else{
      cat('Evaluating gen...')
      la<-keras::evaluate(object = combined,x=noise,y=labels_alarms,verbose=0)
      la<-c(la$loss,la$accuracy,la$precision,la$specificity,la$sensitivity,la$kappa)
    }
    #Validation
    pv<-discriminator$predict(x = X_val)
    tmp_valid<-y_val
    k<-kappa_manual(y_real = tmp_valid,y_pred = pv,threshold=bin_threshold)
    
    if(print_roc&&!is.null(dev.list())) dev.off()
    
    roc_obj <- roc(y_val, as.numeric(pv))
    auc<-as.numeric(auc(roc_obj))
    rocs<-c(rocs,roc_obj)
    
    if(print_roc) plot(roc_obj,title=paste0('AUC: ',auc),xlim=c(1,0), ylim=c(0,1),legacy.axes=T,print.auc=TRUE,grid=T)
    
    val_met<-c(acc_manual(y_real = tmp_valid,y_pred = pv,threshold=bin_threshold),
               precision_manual(y_real = tmp_valid,y_pred = pv,threshold=bin_threshold),
               specificity_manual(y_real = tmp_valid,y_pred = pv,threshold=bin_threshold),
               sensitivity_manual(y_real = tmp_valid,y_pred = pv,threshold=bin_threshold),k)
    cat(paste0('\n',sprintf("%2.1f",round(epoch*100/epochs,1)),'%'))
    
    msg<-paste0('[Dis loss:',sprintf("%.3f", round(ld[1],3)),
                ' acc:',sprintf("%.3f", round(ld[2],3)),
                ' pres:',sprintf("%.3f", round(ld[3],3)),
                ' spec:',sprintf("%.3f", round(ld[4],3)),
                ' sen:',sprintf("%.3f", round(ld[5],3)),
                ' kap:',sprintf("%.3f", round(ld[6],3)),'] ')
    msg<-paste0(msg,' [Adv loss:',sprintf("%.3f", round(la[1],3)),
                ' acc:',sprintf("%.3f", round(la[2],3)),
                ' pres:',sprintf("%.3f", round(la[3],3)),
                ' spec:',sprintf("%.3f", round(la[4],3)),
                ' sen:',sprintf("%.3f", round(la[5],3)),
                ' kap:',sprintf("%.3f", round(la[6],3)),'] ')
    msg<-paste0(msg,' [VAL auc: ',sprintf("%.3f", round(auc,3)),' acc:',sprintf("%.3f", round(val_met[1],3)),
                ' pres:',sprintf("%.3f", round(val_met[2],3)),
                ' spec:',sprintf("%.3f", round(val_met[3],3)),
                ' sen:',sprintf("%.3f", round(val_met[4],3)),
                ' kap:',sprintf(" %.3f", round(val_met[5],3)),'] ')
    cat(msg)
    if(!is.null(k)&&!is.na(k)&&(k>=kappa_validation_stop)){
      stopped_by<-'kappa'
      break #stop with validation kappa of 0.8
    }
    
  }
  
  if(stopped_by=='kappa')
    cat('\n Stopped with Kappa>=',kappa_validation_stop,' on validation')
  else
    cat('\n Stopped by max iterations (',epochs,')')
  
  if(!is.null(save_model_path)) keras::save_model_hdf5(object=model,filepath=save_model_path,overwrite=T,include_optimizer=T)
  adversarial_serialized<-keras::serialize_model(model=combined,include_optimizer = T)
  generator_serialized<-keras::serialize_model(model=generator,include_optimizer = T)
  discriminator_serialized<-keras::serialize_model(model=discriminator,include_optimizer = T)
  
  model<-list(stopped_by=stopped_by,kappa_validation_stop=kappa_validation_stop,timesteps=timesteps,rocs=rocs,min_max_train=min_max_train,predict=predict_ann_adv,architecture_discriminator=architecture_discriminator,architecture_generator=architecture_generator,activation_generator=activation_generator,activation_discriminator=activation_discriminator,dropout_discriminator=dropout_discriminator,dropout_generator=dropout_generator,target_ratio=target_ratio,model_columns=selected_columns,history=history,adversarial_serialized=adversarial_serialized,generator_serialized=generator_serialized,discriminator_serialized=discriminator_serialized,save_model_path=save_model_path)
  
  return(list(error=F,data=model,msg='ok'))
}
