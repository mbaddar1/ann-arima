compute.model<-function(file.path_){
  #browser()
  m.tr.data = read.csv(file = file.path_,header = T,stringsAsFactors = F)
  M = m.tr.data[,c('TOT_UNITS','AVG_REL_DISCOUNT','TOT_FEATURE','TOT_DISPLAY','TOT_TPR_ONLY','MONTH','YEAR')]
  M.d = M
  #differncing
  Y.ts = ts(data = M[,'TOT_UNITS'],frequency = 12)
  nsdiffs_ = nsdiffs(x = Y.ts)
  if(nsdiffs_ >0){
    M.d = diff(x = M.d,lag = frequency(Y.ts),differences = nsdiffs_)
  }
  ndiffs_ = ndiffs(x = Y.ts)
  if(ndiffs_>0){
    M.d = diff(x = M.d,lag = frequency(Y.ts),differences = ndiffs_)
  }
  cat('nsdiffs = ',nsdiffs_,'\n')
  cat('ndiffs = ',ndiffs_,'\n')
  #######
  n = nrow(M.d)
  L = rep(0,n)
  Y = M.d[,'TOT_UNITS']
  et = Y #et = Y-Y_predicted , at start prediction is 0's
  nn_ = NULL
  kt.arima = NULL
  max.iter = 3
  dw.p.thr = 0.1
  dw_ = durbinWatsonTest(model = lm(et~1))
  i=0
  while (i<=max.iter & dw_$p < dw.p.thr) {
    #browser()
    #Y = N+L
    #L = AX+K
    #Kt=>ARMA structure
    #phi(K) = theta(et)
    N = Y-L
    M.d$N = N 
    fmla = N ~ AVG_REL_DISCOUNT+TOT_FEATURE+TOT_DISPLAY+TOT_TPR_ONLY
    covariate.cols = c('AVG_REL_DISCOUNT','TOT_FEATURE','TOT_DISPLAY','TOT_TPR_ONLY')
    
    nn_ = neuralnet(formula = fmla,hidden = 20,data = M.d)
    compute.fitted_ = compute(x = nn_,covariate = M[,covariate.cols])
    et = M.d$TOT_UNITS - compute.fitted_$net.result[,1]
    dw_ = durbinWatsonTest(model = lm(et~1))
    if(dw_$p<0.1){
      browser()
      cat('dw p before arima',dw_$p,'\n')
      start.month = M.d[1,'MONTH']
      start.year = M.d[1,'YEAR']
      L.ts = ts(data = et,start = c(start.year,start.month),frequency = 12)
      kt.arima = auto.arima(y = L.ts,xreg = M.d[,covariate.cols])
      print(summary(kt.arima))
      L = kt.arima$fitted
      et = residuals(kt.arima)
      dw_ = durbinWatsonTest(model = lm(et~1))
      cat('dw p after arima',dw_$p,'\n')
    }else{
      L = rep(0,n)
    }
    i = i+1
  }
  #######
  #rm.ncols = ncol(nn_$result.matrix)
  #err = nn_$result.matrix['error',rm.ncols]
}

#Phase 1 : breakfast at the frat , monthly
#1)simple NN , default params 
#2) remedy no white noise errors with ARIMA

library(neuralnet)
library(quantmod)
library(lmtest)
library(forecast)
library(car)
m.tr.files = read.csv(file = './data/targetFilesList1.txt',header = T,stringsAsFactors = F)
colnames(m.tr.files)<-'file.path'
m.tr.files$file.path = sort(m.tr.files$file.path)
for(f.path in m.tr.files$file.path){
  tryCatch(expr = {
      compute.model(file.path_ = f.path)
    }
  ,error = function(e){print(e)})
}
write.table(x = target.files.vec,file = target.files.dump2)
