packageHandler <- function(packages, update_packages = TRUE, install_packages = TRUE) {
	bad_packages <- list()
	for (package in packages) {
		if (install_packages) {
			tryCatch({
				find.package(package)
			}, error = function(e) {
									if (package %in% available.packages()) {
						install.packages(package, repos = 'http://cran.rstudio.com/')
					} else {
						bad_packages <<- append(bad_packages, package)
					}
			}, warning = function(w) {
				paste(w, 'Shouldn\'t be here.')
			}, finally = {
				if (update_packages) {
					
						update.packages(package, repos = 'http://cran.rstudio.com/')
					}	
			})
		}
	}
	if (length(bad_packages) > 0) {
		if (length(bad_packages) == 1) {
			stop(paste('Package', paste('"', bad_packages, '"', sep = ''), 'is not available for', paste(version$version.string, '.', sep = '')))
		} else {
			stop(paste('Packages', paste(lapply(bad_packages, function(bad_package) {paste('"', bad_package, '"', sep = '')}), collapse = ', '), 'are not available for', paste(version$version.string, '.', sep = '')))
		}
	}
	return()
}
#FOR BRAZIL: DETREND
getTrend <- function(covar_vector, data) {
  new_data <- data
  new_data[c('bs1', 'bs2', 'bs3', 'bs4')] <- 0
  new_data$month_i <- as.factor(1)
  trend <- predict(glm(covar_vector~month_i + ., family = 'gaussian', data = data), type = 'response', newdata = new_data) #month_i is first to be the reference.
  names(trend) <- NULL
  return(trend)
}

DoSTL_trend <- function(new,t.windows,s.windows) {
  trend <- as.data.frame(matrix(NA, nrow=nrow(new), ncol=ncol(new)))
  for (j in 1:ncol(new)) {
    ts <- ts(new[,j], frequency=12)
    trend[,j] <- as.vector(stl(ts, s.window=s.windows, t.window=t.windows)[[1]][,2]) 
  }
  colnames(trend) <- c(paste(colnames(new),".trend.",t.windows,sep=""))
  return(trend)
}
#Simple Poisson model; note: tried glmer with bs random intercept--did not converge consistently in different datasets
glm.fun<-function(ds.fit.fun){
  covars.fit<-ds.fit.fun[-1]
  fixed.effects<-paste(names(ds.fit.fun)[-1], collapse="+")
  form1<-as.formula(paste0('y~', fixed.effects ))
  ds.fit.fun$obs<-as.factor(1:nrow(ds.fit.fun))
  mod1<-glm(form1, data=ds.fit.fun , family='poisson')
  aic.test<-AIC( mod1)
  V<- vcov( mod1)
  coef1<-coef(mod1)
  theta<-mod1$deviance/mod1$df.residual #overdispersion
  pred.mean<- exp(predict(mod1, newdata=ds.fit.fun))
  test.var<-   attributes(ds.fit.fun)$comment  #THIS IS DIFFERENT FOR BIVARIATE
  glm.out<-list(covars.fit,aic.test, V, coef1, pred.mean,test.var,theta) #save output in a named list
  names(glm.out)<-c('covars.fit','aic.test','V','coef1','pred.mean', 'test.var','theta')
  return(glm.out)
}

#Samples for parameter uncertainty piece
param.uncertainty<-function(param.ds){
   
  covars3<-cbind.data.frame(rep(1, times=nrow(data.fit)), param.ds$covars.fit)
  names(covars3)[1]<-"Intercept"
  N.draws.stage1<- round(sqrt(param.ds$Nsamps))
  if(N.draws.stage1>0){
    #In stage 1, generate N1 samples from parameter distribution and multiply by covariates to get pred
    pred.coefs<- mvrnorm(n = N.draws.stage1, mu=param.ds$coef1, Sigma=param.ds$V)
    if(N.draws.stage1==1){preds.stage1<- as.matrix(covars3) %*% pred.coefs
    }else{  
      preds.stage1<- as.matrix(covars3) %*% t(pred.coefs) #the number of columns in the first matrix has to be equal to the number of rows in the second matrix 
    }
  }else{
    preds.stage1<-NA
  }
  stage1.results<-list(N.draws.stage1, preds.stage1,param.ds$Nsamps,param.ds$theta)
  names(stage1.results)<-c('N.draws.stage1','preds.stage1',"Nsamps",'theta')
  return(stage1.results)
}

#Second stage sampling to add on observation uncertainty from negbin distribution
obs.uncertainty<-function(stage1.ds){
  N.draws.stage1<-stage1.ds$N.draws.stage1
  Nsamps<-stage1.ds$Nsamps
  if(Nsamps>0){
  preds.stage1<-stage1.ds$preds.stage1
  #In stage 2, take prediction from stage 1 and sample N2 timesfrom negbin
  N.draws.stage2<-rmultinom(1,prob=rep(1/N.draws.stage1, times=N.draws.stage1), size=Nsamps)
  #replicate mean vector N.draws.stage2 times 
  preds.stage1.rep<-preds.stage1[,rep(seq_len(ncol(preds.stage1)), as.vector(N.draws.stage2)  )]
  preds.stage1.rep<-matrix(preds.stage1.rep, nrow=nrow(preds.stage1), ncol=Nsamps)
  preds.stage2 <- rnegbin(exp(preds.stage1.rep), theta = stage1.ds$theta) #hack, using approximate dispersion
  preds.stage2 <-matrix(preds.stage2, nrow=nrow(preds.stage1.rep), ncol=ncol(preds.stage1.rep))
  }
}
