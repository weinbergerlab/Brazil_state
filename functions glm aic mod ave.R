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
glm.fun<-function(ds.fit.fun){
covars.fit<-ds.fit.fun[-1]
pre.index<-1:(post.start.index-1)
fixed.effects<-paste(names(ds.fit.fun)[-1], collapse="+")
ds.fit.fun$obs<-as.factor(1:nrow(ds.fit.fun))
form1<-as.formula(paste0('y~', fixed.effects, "+ (1|obs)" ))
mod1<-glmer(form1,data=ds.fit.fun[pre.index,], family='poisson')
pred.mean<-predict(mod1, newdata=ds.fit.fun,re.form=NA )
aic.test<-AIC( mod1)
test.var<-   attributes(ds.fit.fun)$comment  #THIS IS DIFFERENT FOR BIVARIATE
glm.out<-list(pred.mean,ds.fit.fun, mod1,aic.test, test.var) #save output in a named list
names(glm.out)<-c('pred.mean','ds.fit.fun','mod1','aic.test','test.var')
return(glm.out)
}

obs.uncertainty<-function(param.ds){
  mod1<-  param.ds$mod1
  ds.fit.fun<-param.ds$ds.fit.fun
  N.samps<-param.ds$Nsamps
  if(N.samps>=1){
  preds.stage2<-simulate(mod1, nsim=N.samps, newdata=ds.fit.fun, allow.new.levels=TRUE,re.form=NA)
  }else{
    preds.stage2<-NA
  }
}


