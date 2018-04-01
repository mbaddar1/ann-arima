
compute.model<-function(file.path_){
  fmla = TOT_UNITS~AVG_REL_DISCOUNT+TOT_FEATURE+TOT_DISPLAY+TOT_TPR_ONLY
  m.tr.data = read.csv(file = file.path_,header = T,stringsAsFactors = F)
  nn_ = neuralnet(formula = fmla,hidden = 10,data = m.tr.data)
  rm.ncols = ncol(nn_$result.matrix)
  err = nn_$result.matrix['error',rm.ncols]
  #cat('hdn = ',hdn,'=> ','error = ',err,'\n')
  compute_ = compute(x = nn_,covariate = m.tr.data[,c('AVG_REL_DISCOUNT','TOT_FEATURE','TOT_DISPLAY','TOT_TPR_ONLY')])
  et = m.tr.data$TOT_UNITS - compute_$net.result[,1]
  #et_1 = Lag(x = et,k = 1)
  #d = sum((et-et_1)^2,na.rm = T)/sum(et^2,na.rm = T)
  #dw_ = dwtest(formula = et~1)
  #TODO , check why p-value are differnet in dw from car and quantmod packages with each run
  dw_ = durbinWatsonTest(model = lm(et~1),simulate = T)
  if(dw_$p<0.1){
    print(file.path_)
    write(x = file.path_,file = target.files.dump1,sep = '\n',append = T)
    target.files.vec <<- append(x = target.files.vec,values = file.path_)
  }
  #cat('target.file.vec = ',target.files.vec,'\n')
  cat('len of target.file.vec',length(target.files.vec),'\n')
}

#Phase 1 : breakfast at the frat , monthly
#1)simple NN , default params 
#2) remedy no white noise errors with ARIMA

#get file list
#montly transactions files
library(neuralnet)
library(quantmod)
library(lmtest)
library(forecast)
#data.path = './data/breakfast-at-the-frat/subsets/monthly/complete/'
#m.tr.files = sort(list.files(path =data.path))
m.tr.files = read.csv(file = './data/targetFilesList1.txt',header = T,stringsAsFactors = F)
colnames(m.tr.files)<-'file.path'
target.files.vec = vector(mode = 'character',length = 0)
target.files.dump1 = './data/targetFilesList1.txt'
target.files.dump2 = './data/targetFilesList2.txt'
#sink(file = './data/samplefiles.txt',append = F)
for(i in 1:length(m.tr.files)){
  cat('file index = ',i,'\n')
  f.path = paste(data.path,m.tr.files[i],sep = '')
  tryCatch(expr = {
      compute.model(file.path_ = f.path)
    }
  ,error = function(e){print(e)})
}


write.table(x = target.files.vec,file = target.files.dump2)

#plot(nn_