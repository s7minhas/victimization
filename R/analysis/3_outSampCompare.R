########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg('MASS')
########################################################

########################################################
# load data
load(paste0(pathData, 'iData_acled.rda'))
########################################################

########################################################
# out of sample cross-val
set.seed(6886)
nFolds = nrow(data)
# data$fold = sample(1:nFolds, nrow(data), replace=TRUE)
data$fold = 1:nFolds

rmseVec1 = NULL
rmseVec2 = NULL
vars = c('civVicCount','graph_dens','nConf','nActors','cname')
for(f in 1:nFolds){

	# split sample
	train = data[data$fold!=f,]
	test = data[data$fold==f,]

	# mod1
	mod1 = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors + factor(cname) -1,
			data=train )

	# mod2
	mod2 = glm.nb(
		civVicCount ~  # dv
			nConf + nActors + factor(cname) -1,
			data=train )

	# pull out relev vars
	yObs = test[,vars[1]]
	testData1 = test[,vars[-1]]
	testData2 = test[,vars[-c(1:2)]]

	# pull out relev betas
	beta1 = coef(mod1)[1:3]
	beta1 = c(beta1, coef(mod1)[grepl(testData1$cname,names(coef(mod1)))])

	beta2 = coef(mod2)[1:2]
	beta2 = c(beta2, coef(mod2)[grepl(testData2$cname,names(coef(mod2)))])	

	# make data a matrix
	testData1[,'cname'] = 1
	testData1 = data.matrix(testData1)

	testData2[,'cname'] = 1
	testData2 = data.matrix(testData2)	

	# generate predictions
	yhat1 = exp(beta1 %*% t(testData1))
	rmseVec1 = c(rmseVec1, (yhat1 - yObs)^2)

	yhat2 = exp(beta2 %*% t(testData2))
	rmseVec2 = c(rmseVec2, (yhat2 - yObs)^2)
}
########################################################

########################################################
sqrt(mean(rmseVec1, na.rm=TRUE))
sqrt(mean(rmseVec2, na.rm=TRUE))
########################################################