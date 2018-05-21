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
#from STL package:
 nextodd <- function(x){
	x <- round(x)
	if(x%%2==0) x <- x+1
	as.integer(x)
    }
DoSTL_trend <- function(new,t.windows,s.windows) {
  trend <- as.data.frame(matrix(NA, nrow=nrow(new), ncol=ncol(new)))
  for (j in 1:ncol(new)) {
    ts <- ts(new[,j], frequency=n_seasons)
    trend[,j] <- as.vector(stl(ts, s.window=s.windows, t.window=t.windows)[[1]][,2]) 
  }
  colnames(trend) <- c(paste(colnames(new),".trend.",t.windows,sep=""))
  return(trend)
}
glm.fun<-function(ds.fit.fun){
covars.fit<-ds.fit.fun[-1]
pre.index<-1:(post.start.index-1)
fixed.effects<-paste(names(ds.fit.fun)[-1], collapse="+")
ds.fit.fun$obs<-as.factor(1:nrow(ds.fit.fun))
form1<-as.formula(paste0('y~', fixed.effects, "+ (1|obs)" ))
mod1<-glmer(form1,data=ds.fit.fun[pre.index,], family='poisson',control=glmerControl(optimizer="bobyqa",
                                                                                      optCtrl=list(maxfun=2e5)) )
pred.mean<-predict(mod1, newdata=ds.fit.fun,re.form=NA )
aic.test<-AIC( mod1)
test.var<-   attributes(ds.fit.fun)$comment  #THIS IS DIFFERENT FOR BIVARIATE
glm.out<-list(pred.mean,ds.fit.fun, mod1,aic.test, test.var) #save output in a named list
names(glm.out)<-c('pred.mean','ds.fit.fun','mod1','aic.test','test.var')
return(glm.out)
}

obs.uncertainty<-function(param.ds){
   N.samps<-param.ds$Nsamps
  if(N.samps>=1){
    covar.mat1<-param.ds$ds.fit.fun
    mod1<-param.ds$mod1
    test.var<-param.ds$test.var
    if(test.var== 'nocovars'){ 	  
	covars3<-as.matrix(covar.mat1[c(names(season.dummies))])
      }else{
    covars3<-as.matrix(covar.mat1[c(names(season.dummies),param.ds$test.var)])
	    }
    covars3<-cbind.data.frame(rep(1, times=nrow(covars3)), covars3)
    names(covars3)[1]<-"Intercept"
    N.stage1<-round(sqrt(N.samps))
    pred.coefs.reg.mean<- mvrnorm(n = N.stage1, mu=fixef(mod1), Sigma=vcov( mod1))
    pred.coefs.reg.mean<-matrix(pred.coefs.reg.mean, nrow=N.stage1)
    preds.stage1.regmean<- as.matrix(covars3) %*% t(pred.coefs.reg.mean)
    re.sd<-as.numeric(sqrt(VarCorr(mod1)[[1]]))
    preds.stage1<-rnorm(n<-length(preds.stage1.regmean), mean=preds.stage1.regmean, sd=re.sd)
    preds.stage1<-matrix(preds.stage1, nrow=nrow(preds.stage1.regmean), ncol=ncol(preds.stage1.regmean))
    preds.stage2<-rpois(n=N.samps*nrow(preds.stage1), lambda=exp(preds.stage1))  
    preds.stage2<-matrix(preds.stage2,nrow=nrow(preds.stage1), ncol=N.samps)
    }else{
    preds.stage2<-NA
  }
}



