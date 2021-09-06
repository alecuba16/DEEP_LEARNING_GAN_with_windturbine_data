unitary_test<-function(iterations=10,nvars=3,npoints_orig=10,npoints_selected=5,npoints_no_selected=3){
  iam=match.call()[[1]]
  knn_pass<-T
  rbm_pass<-T
  som_pass<-T
  it<-1
  set.seed(1)
  while(knn_pass&&rbm_pass&&som_pass&&it<=iterations){
    cat(paste0('\n Iteration ',it,' of ',iterations,' ... '))
    #Knn
    m<-matrix(runif(npoints*(nvars+2)),nrow = npoints)
    m<-as.data.frame(m)
    m[,dim(m)[2]-1]<-1 #1 alarm 0 healthy
    m[,dim(m)[2]]<-0 #0 for original 1toselect 2noselect
    colnames(m)[dim(m)[2]-1]<-'target'
    colnames(m)[dim(m)[2]]<-'orig0_sel1_no2'
    #Generate selected
    generated<-0
    for(x in 1:dim(m)[1]){
      to_add<-min(npoints_selected-generated,ceiling(npoints_orig/npoints_selected))
      if(to_add>0){
        to_add<-runif(to_add,min=0.01,max=0.2)
        for(i in to_add){
          tmp<-NULL
          for(y in 1:(dim(m)[2]-2)){
            tmp<-c(tmp,m[x,y]+i)
          }
          m<-rbind(m,c(tmp,0,1))
          generated<-generated+1
        }
      }
    }
    
    #Generate no selected
    generated<-0
    for(x in 1:dim(m)[1]){
      to_add<-min(npoints_no_selected-generated,ceiling(npoints_orig/npoints_no_selected))
      if(to_add>0){
        to_add<-runif(to_add,min=0.5,max=0.8)
        for(i in to_add){
          tmp<-NULL
          for(y in 1:(dim(m)[2]-2)){
            tmp<-c(tmp,m[x,y]+i)
          }
          m<-rbind(m,c(tmp,0,2))
          generated<-generated+1
        }
      }
    }
    
    #Call knn
    rs<-select_good_with_bad_variables(wtdata=m,find_by='knn',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
    if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
    selected_good<-rs$data$selected_good
    selected_good<-selected_good[!is.na(selected_good)]
    if(!all(m$orig0_sel1_no2[selected_good]==1)){
      knn_pass<-F
      cat(' KNN failed')
    }else{
      #Call rbm
      rs<-select_good_with_bad_variables(wtdata=m,find_by='rbm',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
      if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
      selected_good<-rs$data$selected_good
      selected_good<-selected_good[!is.na(selected_good)]
      if(!all(m$orig0_sel1_no2[selected_good]==1)){
        rbm_pass<-F
        cat(' RBM failed')
      }else{
        #Call som
        rs<-select_good_with_bad_variables(wtdata=m,find_by='som',balance=30,selected_variables=colnames(m)[1:(dim(m)[2]-2)],target_name='target',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8)
        if(rs$error) return(list(error=T,data=NULL,msg=paste0(iam,' on call select_good_with_bad_variables:',rs$msg)))
        selected_good<-rs$data$selected_good
        selected_good<-selected_good[!is.na(selected_good)]
        if(!all(m$orig0_sel1_no2[selected_good]==1)){
          som_pass<-F
          cat(' SOM failed')
        }
      }
    }
    it<-it+1
  }
  return(list(error=F,data=list(knn_pass=knn_pass,rbm_pass=rbm_pass,som_pass=som_pass),msg='ok'))
}

fs_equal_freq <- function(x,nbins){
  nbins<-ceiling(nbins)
  nx <- length(x)
  nrepl <- floor(nx/nbins)
  nplus <- sample(1:nbins,nx - nrepl*nbins)
  nrep <- rep(nrepl,nbins)
  nrep[nplus] <- nrepl+1
  x[order(x)] <- rep(seq.int(nbins),nrep)
  x
}

fs_discretize_freq<-function(data=NULL,method='freedman_diaconis'){
  #freedman_diaconis https://en.wikipedia.org/wiki/Freedman–Diaconis_rule
  for(col in 1:ncol(data)){
    bins<-switch(method,
                 'freedman_diaconis'=(diff(range(data[,col]))/(2*IQR(data[,col])/length(data[,col])^(1/3))),
                 'nrow/3'=nrow(data)/3)
    y <- fs_equal_freq(data[,col],bins)
    ubins<-unique(y)
    bins_median<-sapply(1:max(ubins),function(bin) bin_median<-median(data[y==bin,col]))
    data[,col]<-sapply(1:nrow(data),function(row) bins_median[y[row]])
  }
  return(data)
}


select_good_with_bad_variables_wrapper<-function(wtdata=NULL,selected_rows=NULL,exclude_columns=NULL,find_by='knn',balance=30,selected_variables=NULL,target_name='ot',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8,verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('dbscan','deepnet','kohonen')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  
  rs<-select_good_with_bad_variables(wtdata=wtdata,selected_rows=selected_rows,find_by=find_by,balance=balance,selected_variables=selected_variables,target_name=target_name,k=k,discretize=discretize,standarize=standarize,n_epochs=n_epochs,n_batchsize=n_batchsize,learningrate=learningrate,verbose=verbose)
  if(rs$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call select_good_with_bad_variables\n",rs$msg)))
  selected_good<-rs$data$selected_good
  
  possible_good<-selected_rows[wtdata[selected_rows,target_name]==0]
  possible_bad<-selected_rows[wtdata[selected_rows,target_name]==1]
  
  t_selected_columns<-colnames(wtdata)[(!(colnames(wtdata) %in% exclude_columns))]
  t_selected_columns<-t_selected_columns[!t_selected_columns %in% selected_variables] #Exclude environmental vars
  tomek_selected<-tomek(wtdata=wtdata,selected_rows=selected_rows,selected_columns=t_selected_columns,target_name=target_name,verbose=verbose,standarize=standarize)
  
  tomek_selected<-tomek_selected[tomek_selected %in% possible_good]
  
  selected_good<-unique(c(selected_good,tomek_selected))
  selected_bad<-possible_bad
  
  selected_rows<-sort(c(selected_good,selected_bad))
  
  return(list(error=F, warning=F, data=list(wtdata=wtdata,selected_rows=selected_rows),msg=paste0("\n",iam,":on call select_good_with_bad_variables\n",rs$msg)))
}

select_good_with_bad_variables<-function(wtdata=NULL,selected_rows=NULL,find_by='knn',balance=30,selected_variables=NULL,target_name='ot',k=10,discretize=F,standarize=T,n_epochs=500,n_batchsize=100,learningrate=0.8,verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('dbscan','deepnet','kohonen')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  set.seed(1)
  selected_good<-NULL
  #selected_variables<-grep(pattern = 'TempAmb_.*|VelViento_.*',x = colnames(wtdata),value = T)
  #Select vars
  wtdata<-wtdata[selected_rows,c(selected_variables,target_name)]
  #Complete cases
  complete_cases<-which(complete.cases(wtdata))
  wtdata<-wtdata[complete_cases,]
  #Discretize
  if(discretize) wtdata[,selected_variables]<-fs_discretize_freq(wtdata[,selected_variables])
  #Standarize
  if(standarize) wtdata[,selected_variables]<-scale(wtdata[,selected_variables],center = T,scale = T)
  # selected_variables<-c('VelViento_avg','VelViento_max','VelViento_min','VelViento_sdv','TempAmb_avg','TempAmb_min','TempAmb_max','TempAmb_sdv')
  good_cases<-which(wtdata[,target_name]==0)
  bad_cases<-which(wtdata[,target_name]==1)
  n_good_cases_to_select <- round(((length(bad_cases)*(100-balance))/balance)-length(bad_cases),digits = 0)
  
  if(find_by=='knn'){
    d<-dist(wtdata[,selected_variables])
    d<-as.matrix(d)
    d<-d[bad_cases,good_cases]
    d<-lapply(1:nrow(d),function(rid){
      l<-lapply(1:length(d[rid,]),function(cid) c(rownames(d)[rid],colnames(d)[cid],d[rid,cid]))
      l<-do.call("rbind",l)
      return(l)
    })
    d<-do.call("rbind",d)
    d<-as.data.frame(d)
    colnames(d)<-c('badid','goodid','dist')
    d$badid<-as.numeric(d$badid)
    d$goodid<-as.numeric(d$goodid)
    d$dist<-as.numeric(levels(d$dist)[d$dist])
    tmpd<-NULL
    for(gid in unique(d$goodid)){
      tmpd<-rbind(tmpd,d[which((d$dist==min(d$dist[d$goodid==gid],na.rm = T)&(d$goodid==gid))),])
    }
    d<-tmpd
    #Normalize distance 0-1
    d$dist<-(d$dist-min(d$dist))/(max(d$dist)-min(d$dist))
    #Exclude dists > 25%
    d<-d[d$dist<=0.25,]
    d<-d[order(d$dist),]
    good_like_bad<-complete_cases[good_cases[d$goodid]]
  }
  
  if(find_by=='rbm'){
    if(verbose){cat('training RBM with original bad points...')}
    model_rbm <- rbm.train(as.matrix(wtdata[bad_cases,selected_variables]), hidden = length(selected_variables),batchsize = n_batchsize,numepochs = n_epochs,learningrate = learningrate)
    x_new <- rbm.up(model_rbm, as.matrix(wtdata[good_cases,selected_variables]))
    x_new <- rbm.down(model_rbm, x_new)
    #Compute distance
    x<-wtdata[good_cases,selected_variables]
    dist<-sapply(1:dim(x)[1],function(r) sqrt(sum((x[r,]-x_new[r,])^2,na.rm = T)))
    d<-data.frame(goodid=1:length(dist),dist=dist)
    #Normalize distance 0-1
    d$dist<-(d$dist-min(d$dist))/(max(d$dist)-min(d$dist))
    #Exclude dists > 25%
    d<-d[d$dist<=0.25,]
    d<-d[order(d$dist),]
    good_like_bad<-d$goodid[1:min(n_good_cases_to_select,dim(d)[1])]
    good_like_bad<-complete_cases[good_cases[good_like_bad]]
  }
  
  if(find_by=='som'){
    if(verbose){cat('training RBM with original bad points...')}
    model_params<-list(type='gsom',spreadFactor=0.8,hex=T) #Use som model
    rs<-create_som_model(wtdata=wtdata[,selected_variables],model_params=model_params,parallel_mode=F,log_file=NULL,verbose=verbose,standarize=!standarize)
    if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
    som_result<-rs$data
    model_params<-rs$data$model_params
    som_model<-som_result$model
    som_pts<-rs$data$som_pts
    som_codes<-rs$data$som_codes
    som_distances<-rs$data$som_distances
    som_center_neuron_position<-rs$data$center_neuron_position
    dim<-rs$data$dim
    som_selected_rows<-som_result$selected_rows
    som_selected_columns<-som_result$selected_columns
    wtdata_bmu<-rs$data$wtdata_bmu
    hex<-rs$data$hex
    stat<-rs$data$stat
    bad_neurons<-unique(wtdata_bmu[bad_cases])
    selected_good<-which(wtdata_bmu[good_cases] %in% bad_neurons)
    #Iteration 0 resampling
    selected_good<-sample(x = selected_good,size = n_good_cases_to_select,replace = T)
    good_like_bad<-complete_cases[good_cases[selected_good]]
  }
  
  good_like_bad<-unique(good_like_bad)
  selected_rows<-selected_rows[good_like_bad]
  
  #Plots
  #df<-wtdata[,c('TempAmb_avg','VelViento_avg','VelViento_sdv')]
  #df$type<-'good'
  #df$type[bad_cases]<-'bad'
  #df$type[good_like_bad]<-'good_sel'
  #plot_ly(df,x=~VelViento_avg,y=~VelViento_sdv,z=~TempAmb_avg,color=~type)
  
  return(list(error=F,data=list(selected_good=selected_rows),msg='ok'))
}

balance_by_oversampling<-function(wtdata=NULL,selected_rows=selected_rows,target_name='pre_alarm',balance=NULL){
  num_good<-sum(wtdata[,target_name]==0)
  num_bad<-sum(wtdata[,target_name]==1)
  possible_bad<-which(wtdata[,target_name]==1)
  possible_bad<-possible_bad[possible_bad %in% selected_rows]
  num_wanted_bad<-floor((balance*num_good)/100)
  num_wanted_bad<-num_wanted_bad-num_bad
  if(num_wanted_bad>0) selected_rows<-c(selected_rows,sample(possible_bad, size = num_wanted_bad-num_bad,replace = T))
  return(list(error=F,data=list(selected_rows=selected_rows,wtdata=wtdata),msg='ok'))
}

balance_by_undersampling<-function(wtdata=NULL,selected_rows=selected_rows,target_name='pre_alarm',date_time_name='date_time',select_good_same_bad_interval=T,select_all_goods_after_bad=T,marging_after_pre_alarm=24*60*60,balance=NULL){
  #Select refigster after change ot
  already_selected_good<-NULL
  if(select_all_goods_after_bad){
    for(ld in unique(wtdata$ld_id[train_selected_rows])){
      current_pre_alarms<-which((wtdata$ld_id==ld)&(wtdata[,target_name]))
      if(length(current_pre_alarms)>0){
        last_day<-max(wtdata[current_pre_alarms,date_time_name],na.rm = T)
        last_day<-last_day+as.difftime(marging_after_pre_alarm, units="secs")
        already_selected_good<-which((wtdata$ld_id==ld)&(wtdata[,date_time_name]>last_day))
      }
    }
  }
  
  bad_selected_rows<-which(wtdata[,target_name]==1) #Preselect alarms
  bad_selected_rows<-bad_selected_rows[bad_selected_rows %in% selected_rows]
  bad_selected_datetime<-date_time[bad_selected_rows]
  max_bad<-sum(wtdata[selected_rows,target_name]==1)
  max_good<-sum(wtdata[selected_rows,target_name]==0)
  
  total<-(max_bad*100)/balance
  num_wanted_good<-round(total-max_bad,0)
  #Add same datetime as bad from good
  possible_good<-which(wtdata[,target_name]==0)
  possible_good<-possible_good[possible_good %in% selected_rows]
  #real_balance<-length(selected_rows)*100/(length(possible_good)+length(selected_rows)) #Balance with available good.
  if(!is.null(select_good_same_bad_interval)&&select_good_same_bad_interval){
    tmp_sel<-which(date_time[possible_good] %in% bad_selected_datetime)
    already_selected_good<-c(already_selected_good,possible_good[possible_good %in% tmp_sel]) #Selects from good the same date_time as  from bad.
    possible_good<-possible_good[!(possible_good %in% already_selected_good)]
  }
  good_selected_rows<-c(already_selected_good,sample(possible_good, size = min(length(possible_good),num_wanted_good-length(already_selected_good)), replace = F))
  selected_rows<-c(bad_selected_rows,good_selected_rows)
  return(list(error=F,data=list(selected_rows=selected_rows,wtdata=wtdata),msg='ok'))
}

fix_with_rbm<-function(wtdata=NULL,original_bad_rows=NULL,bad_rows_tofix=NULL,n_epochs=500,n_batchsize=100,learningrate=0.8,normalize=T,verbose=F){
  if(verbose){cat('\nSmoothing with RBM...')}
  
  #Complete cases!
  complete_cases<-which(complete.cases(wtdata))
  bad_rows_tofix<-bad_rows_tofix[bad_rows_tofix %in% complete_cases]
  original_bad_rows<-original_bad_rows[original_bad_rows %in% complete_cases]
  wtdata<-as.matrix(wtdata[complete_cases,])
  
  #Normalize data 0-1
  if(normalize){
    original_ranges<-data.frame(name=colnames(wtdata),min=as.numeric(apply(wtdata[original_bad_rows,],2,min,na.rm = T)),max=as.numeric(apply(wtdata[original_bad_rows,],2,max,na.rm = T))) 
    wtdata<-lapply(original_ranges,1,function(r) (wtdata[,r[1]]-r[2])/(r[3]-r[2]))
    wtdata<-do.call("cbind",wtdata)
  }
  
  #Setting training data size to batch size rounded.
  if ((length(original_bad_rows) %% n_batchsize) != 0) {
    n_trim <- floor(length(original_bad_rows) / n_batchsize) * n_batchsize
    row_samp <- sample(original_bad_rows, n_trim)
    train <- wtdata[row_samp,]
  }
  if(verbose){cat('training RBM with original bad points...')}
  #library(deepnet)
  model_rbm <- rbm.train(train, hidden = ncol(train),batchsize = n_batchsize,numepochs = n_epochs,learningrate = 0.2)
  
  x_new <- rbm.up(model_rbm, wtdata[selected_rows_tofix,])
  x_new <- rbm.down(model_rbm, x_new)
  #tmp<-sapply(1:ncol(x_new),function(col) (x_new[,col]*(max[col]-min[col]))+min[col])
  #x_new<-matrix(tmp,nrow = dim(x_new)[1])
  colnames(x_new) <- colnames(wtdata)
  x_new <- as.data.frame(x_new)
  if(verbose){cat('finish RBM\n')}
  return(list(error=F,data=list(wtdata=x_new,selected_rows=complete_cases),msg='ok'))
}  

my_smote<-function(wtdata=NULL,balance=50,k=5,selected_rows=NULL,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL,verbose=F){
  if(verbose){cat('\nGenerating SMOTE points...\n')}
  smote_generated_rows<-NULL
  bad_rows<-which(wtdata[,target_name]==1)
  bad_selected_rows<-bad_rows[bad_rows %in% selected_rows]
  d<-data.frame(n=1:length(bad_selected_rows),o=bad_selected_rows)
  nnarray<-dbscan::kNN(wtdata[bad_selected_rows,!(colnames(wtdata) %in% exclude_columns)], k=k)
  alarm_cases<-sum(selected_rows %in% which(wtdata[,target_name]==1))
  ok_cases<-sum(selected_rows %in% which(wtdata[,target_name]==0))
  prob_balance<-balance/100
  num_wanted_bad<-floor((ok_cases*prob_balance/(1-prob_balance))-alarm_cases)
  older_alarm_turbine<-lapply(unique(wtdata$ld_id),function(ld_id){
    bads<-which((wtdata[,target_name]==1)&(wtdata$ld_id==ld_id))
    if(length(bads)>0){
      bads<-bads[bads %in% selected_rows]
      if(length(bads)>0) data.frame(ld_id=ld_id,date_time=min(wtdata[bads,date_time_name],na.rm = T))
    }
  })
  older_alarm_turbine<-do.call('rbind',older_alarm_turbine)
  for(i in bad_selected_rows){
    cat(paste0('\n',which(i==bad_selected_rows),' of ',length(bad_selected_rows)))
    N<-floor(num_wanted_bad/length(bad_selected_rows))
    ir<-d$n[d$o==i]
    while(N>0){
      tmp_df<-NULL
      current_row<-nnarray$id[ir,ceiling(runif(1,1,k))]
      neib<-wtdata[d$o[d$n==current_row],]
      if(date_time_name %in% colnames(wtdata)){#Datetime
        cld_id<-wtdata[i,'ld_id']
        new_dt<-older_alarm_turbine$date_time[older_alarm_turbine$ld_id==cld_id]-as.difftime(seconds_to_aggregate,units = 'secs')
        exists<-which((wtdata[,'ld_id']==cld_id)&(wtdata[,date_time_name]==new_dt))
        if(length(exists)==0){
          tmp_df<-data.frame(date_time=new_dt)
          colnames(tmp_df)[1]<-date_time_name
        }
        older_alarm_turbine$date_time[older_alarm_turbine$ld_id==cld_id]<-new_dt
      }
      columns<-colnames(wtdata)[colnames(wtdata)!=date_time_name]
      for(col in columns){
        if(!(col %in% exclude_columns)){
          diff<-wtdata[i,col]-neib[,col]
          tmp_val<-wtdata[i,col]+(diff*runif(1,0,1))
        }else if((col %in% exclude_columns)&&col!=date_time_name){
          tmp_val<-wtdata[i,col]
        }
        if(length(exists)>0){
          wtdata[exists[1],col]<-tmp_val
        }else{
          tmp_df<-cbind(tmp_df,tmp_val) 
          colnames(tmp_df)[ncol(tmp_df)]<-col
        }
      }
      if(length(exists)==0){
        tmp_df[,target_name]<-1
        wtdata<-rbind(wtdata,tmp_df)
        smote_generated_rows<-c(smote_generated_rows,nrow(wtdata))
        selected_rows<-c(selected_rows,nrow(wtdata))
        bad_rows<-c(bad_rows,nrow(wtdata))
      }else{
        if(!(exists[1] %in% selected_rows)) selected_rows<-c(selected_rows,exists[1])
        if(!(exists[1] %in% bad_rows)) bad_rows<-c(bad_rows,exists[1])
        wtdata[exists[1],target_name]<-1
        smote_generated_rows<-c(smote_generated_rows,exists[1])
      }
      N<-N-1
    }
  }
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows,smote_generated_rows=smote_generated_rows),msg='ok'))
}

balance_by_neighbor_clr_rule<-function(wtdata=NULL,selected_rows=NULL,k=3,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL){
  #Neighborhood Cleaning Rule
  iam=match.call()[[1]]
  set.seed(1)
  X<-wtdata[selected_rows,(!(colnames(wtdata) %in% exclude_columns))]
  y<-wtdata[selected_rows,(colnames(wtdata)==target_name)]
  tmp_to_wtdata<-data.frame(rowtmp=1:nrow(X),rowwtdata=selected_rows)
  rs<-ubNCL(as.matrix(X), y, k = k, verbose = TRUE)
  rm_index = rs$id.rm
  rm_index_wtdata<-tmp_to_wtdata$rowwtdata[tmp_to_wtdata$rowtmp %in% rm_index]
  tomek_selected_rows<-selected_rows[!(selected_rows %in% rm_index_wtdata)]
  selected_rows<-tomek_selected_rows
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows),msg='ok'))
}

tomek<-function(wtdata=NULL,selected_rows=NULL,selected_columns=NULL,target_name=NULL,verbose=T,standarize=T){
  if(verbose) cat('\nTOMEK Cleaning borders between classes...')
  wtdata<-wtdata[selected_rows,unique(c(selected_columns,target_name))]
  if(standarize) wtdata[,selected_columns]<-scale(wtdata[,selected_columns],center = T,scale = T)
  tomek = unbalanced::ubTomek(wtdata[,selected_columns],wtdata[,target_name],verbose = verbose)
  rm_index = tomek$id.rm
  rm_index_wtdata<-selected_rows[1:dim(wtdata)[1] %in% rm_index]
  tomek_selected_rows<-selected_rows[!(selected_rows %in% rm_index_wtdata)]
  return(tomek_selected_rows)
}

balance_by_tomek_smote_rbm<-function(wtdata=NULL,selected_rows=NULL,target_name='pre_alarm',date_time_name='date_time',seconds_to_aggregate=86400,exclude_columns=NULL,balance=50,standarize=T,plot=T,verbose=F){
  iam=match.call()[[1]]
  p<-NULL
  set.seed(1)
  selected_columns<-colnames(wtdata)[(!(colnames(wtdata) %in% exclude_columns))]
  
  #Tomek
  tomek_selected_rows<-tomek(wtdata=wtdata,selected_rows=selected_rows,selected_columns=selected_columns,target_name=target_name,verbose=verbose,standarize=standarize)
  if(length(unique(wtdata[tomek_selected_rows,target_name]))<2) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable is constant after tomek_selected_rows")))
  
  #SMOTE
  rs<-my_smote(wtdata=wtdata,balance=balance,k=5,selected_rows=tomek_selected_rows,target_name=target_name,date_time_name=date_time_name,seconds_to_aggregate=seconds_to_aggregate,exclude_columns=exclude_columns,verbose=verbose)
  wtdata<-rs$data$wtdata
  selected_rows<-rs$data$selected_rows
  smote_generated_rows<-rs$data$smote_generated_rows
  
  #Fix smote points with RBM
  bad_rows<-which(wtdata[,target_name]==1)
  selected_columns<-!(colnames(wtdata) %in% exclude_columns)
  rs<-fix_with_rbm(wtdata=wtdata[,selected_columns],original_bad_rows=bad_rows,bad_rows_tofix=smote_generated_rows,n_epochs=5000,n_batchsize=50,learningrate=0.2,normalize=T,verbose=verbose)
  if(rs$error)  return(list(error=T,data=NULL,msg=paste0("\n",iam," on call fix_with_rbm:",rs$msg)))
  rbm_generated_rows<-rs$data$selected_rows
  wtdata[rbm_generated_rows,selected_columns]<-rs$data$wtdata
  
  if(plot){
    #smote_generated_rows
    originalAlarms<-which(wtdata$pre_alarm==1)
    originalAlarms<-originalAlarms[originalAlarms %in% selected_rows]
    originalAlarms<-originalAlarms[!(originalAlarms %in% smote_generated_rows)]
    originalNoalarms<-which(wtdata$pre_alarm==0)
    originalNoalarms<-originalNoalarms[!(originalNoalarms %in% originalAlarms)]
    originalNoalarms<-originalNoalarms[!(originalNoalarms %in% smote_generated_rows)]
    p<-ggplot()+
      geom_point(data = wtdata[originalNoalarms,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='green',alpha=0.05)+
      geom_point(data = wtdata[originalAlarms,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='violet',alpha=0.2)+
      geom_point(data = wtdata[smote_generated_rows,],aes(x=Pot_avg,y=TempAceiteMultip_avg),color='red',alpha=0.07)+
      theme_bw()
  }
  
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows,plot=p),msg='ok'))
}

#Balance -> number of pre_alarm false in proportion of true in percentage.
sampling_dataset<-function(lstm=F,wtdata=NULL,selected_rows=NULL,target_name=NULL,seconds_to_aggregate=60*60*24,balance=NULL,balance_by='copy',select_good_same_bad_interval=NULL,select_all_goods_after_bad=NULL,date_time_name='date_time',standarize=T,environmental_selected_variables=NULL,exclude_columns='date_time,ld_id,health_status,ot,ot_block_code,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot_all,ot_all_block_code,pre_alarm',verbose=F){
  iam=match.call()[[1]]
  if(!exists("dependencyLoader")){
    if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
    source('functions_common/dependencyLoader.R')
  }
  
  sourcesandlib<-c('unbalanced','dbscan','deepnet')
  dep<-dependencyLoader(sourcesandlib)
  if(dep$error) return(list(error=TRUE, warning=FALSE, data=NULL,msg=paste0("\n",iam,":on call dependencyLoader\n",dep$msg)))
  set.seed(1)
  
  if(!is.null(exclude_columns)&&length(exclude_columns)==1) exclude_columns<-unlist(strsplit(x = exclude_columns,','))
  
  #Change target variable to numeric 0/1
  if(is.logical(wtdata[,target_name])) wtdata[,target_name]<-as.numeric(wtdata[,target_name])
  if(!(is.logical(wtdata[,target_name])||is.numeric(wtdata[,target_name]))||(is.numeric(wtdata[,target_name])&&((length(unique(wtdata[,target_name]))!=2)||!all(c(0,1) %in% unique(wtdata[,target_name]))))) return(list(error=T,data=NULL,msg=paste0("\n",iam," target variable(",target_name,") must be or logical or numeric with 0/1")))
  
  #Standarize only with selected rows
  if(standarize){
    selected_columns<-colnames(wtdata)[!(colnames(wtdata) %in% c(exclude_columns,target_name))]
    means<-sapply(selected_columns,function(col) mean(wtdata[selected_rows,col],na.rm = T) )
    sdv<-sapply(selected_columns,function(col) sd(wtdata[selected_rows,col],na.rm = T) )
    tmp<-lapply(1:length(selected_columns),function(i) (wtdata[,selected_columns[i]]-means[i])/sdv[i])
    tmp<-do.call("cbind",tmp)
    wtdata[,selected_columns]<-tmp
    rm(tmp)
    gc(verbose = F)
    stat<-data.frame(name=selected_columns,mean=means,sd=sdv)
  }
  
  #Balance
  if(!is.null(balance_by)&&!is.na(balance_by)){
    if((balance_by %in% c('oversampling','undersampling'))&&(is.null(balance)||is.na(balance)||balance>=100||balance<=0)) return(list(error=T,data=NULL,msg=paste0('\n',iam,': defined balance by:',balance_by,' which requires that balance variable is defined 0<balance<100')))
    rs<-switch(balance_by,
               'oversampling'=balance_by_oversampling(wtdata=wtdata,target_name=target_name,balance=balance,selected_rows=selected_rows),
               'undersampling'=balance_by_undersampling(wtdata=wtdata,target_name=target_name,date_time_name=date_time_name,selected_rows=selected_rows,select_good_same_bad_interval=select_good_same_bad_interval,select_all_goods_after_bad=select_all_goods_after_bad,balance=balance),
               'tomek_smote_rbm'=balance_by_tomek_smote_rbm(wtdata=wtdata,exclude_columns=exclude_columns,smote=smote,selected_rows=selected_rows,seconds_to_aggregate=seconds_to_aggregate,balance=balance,verbose=verbose,standarize=!standarize),
               'neighbor_clr_rule'=balance_by_neighbor_clr_rule(wtdata=wtdata,selected_rows=selected_rows,target_name=target_name,date_time_name=date_time_name,seconds_to_aggregate=seconds_to_aggregate,exclude_columns=exclude_columns),
               'select_good_with_bad_variables'=select_good_with_bad_variables_wrapper(wtdata=wtdata,find_by='knn',balance=balance,exclude_columns=exclude_columns,selected_rows=selected_rows,selected_variables=environmental_selected_variables,target_name=target_name,discretize=T,standarize=!standarize),
               return(list(error=T,data=NULL,msg="\nSpecified balance but not the method, possible values:select_good_with_bad_variables,oversampling,undersampling,tomek_smote_rbm")))
    if(rs$error) return(list(error=T,data=NULL,msg=rs$msg))
    selected_rows<-rs$data$selected_rows
    wtdata<-rs$data$wtdata
  }
  
  real_balance<-sum(wtdata[selected_rows,target_name]==1,na.rm = T)*100/length(selected_rows) #Balance with available good.
  #Return target name to boolean
  wtdata[,target_name]<-(wtdata[,target_name]==1)
  
  return(list(error=F,data=list(wtdata=wtdata,selected_rows=selected_rows,real_balance=real_balance),msg='ok'))
}