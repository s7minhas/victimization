########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('MASS','reshape2'))
########################################################

########################################################
# load data
load(paste0(pathData, 'iData_acled.rda'))

# org some vars
dv = 'civVicCount'
ivs = c('graph_dens','nConf','nActors')
struc = '+ factor(cname) - 1 '
########################################################

########################################################
# out of sample cross-val

# drop countries with less than five observations
toKeep = names(table(data$cname)[table(data$cname)>=5])
data = data[which(data$cname %in% toKeep),]
data = data[!is.na(data[,dv]),]

# divide dataset into folds
set.seed(6886)

# loocv
nFolds = nrow(data)
data$fold = 1:nFolds

# storage
errCompare = matrix(NA,
	nrow=nFolds, 
	ncol=2,
	dimnames=list(NULL,c('dens','noDens'))
	)

# model specs to compare
f1 = formula( paste0(dv, '~', 
		paste(ivs, collapse='+'), struc) )
f2 = formula( paste0(dv, '~', 
		paste(ivs[2:3], collapse='+'), struc) )

# run out samp analysis
for(f in 1:nFolds){

	# split sample
	train = data[data$fold!=f,]
	test = data[data$fold==f,]

	# mod1
	mod1 = glm.nb( f1, data=train )

	# mod2
	mod2 = glm.nb( f2, data=train )

	# pull out relev vars
	yObs = test[,dv]
	cntryObs = test[,'cname'] %>%
		paste0('factor(cname)',.)

	# pull out relev betas
	beta1 = coef(mod1)[1:3]
	beta1 = c(
		beta1, 
		coef(mod1)[names(coef(mod1))==cntryObs] )

	beta2 = coef(mod2)[1:2]
	beta2 = c(
		beta2, 
		coef(mod2)[names(coef(mod2))==cntryObs] )

	# make data a matrix
	test[,'cname'] = 1
	testData1 = data.matrix(test[,c(ivs,'cname')])
	testData2 = data.matrix(test[,c(ivs[-1],'cname')])

	# generate predictions
	yhat1 = exp(beta1 %*% t(testData1))
	# errCompare[f,'dens'] = (yhat1 - yObs)^2 
	errCompare[f,'dens'] = abs(yhat1 - yObs) 

	yhat2 = exp(beta2 %*% t(testData2))
	# errCompare[f,'noDens'] = (yhat2 - yObs)^2 
	errCompare[f,'noDens'] = abs(yhat2 - yObs)
}
########################################################

########################################################
round(apply(errCompare, 2, mean),3)

ggData = melt(errCompare)
ggplot(ggData, aes(x=value,color=Var2)) + geom_density()
########################################################