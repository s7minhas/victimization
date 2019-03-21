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
load(paste0(pathData, 'data_acled.rda'))
load(paste0(pathData, 'cntriesGED_byAll.rda'))
cData = cData[,c('country','year')]
cData$cnt = 1
cData = cData %>% group_by(country, year) %>%
	summarize(nConf = sum(cnt)) %>%
	data.frame()
cData$cname = cname(cData$country)
cData$id = with(cData, paste(cname, year, sep="_"))
data$nConf = cData$nConf[match(data$id, cData$id)]
data$nConf[is.na(data$nConf)] = 0
########################################################

########################################################
# naive just impute everything
loadPkg('sbgcop')
if(!file.exists(paste0(pathData, 'imputedData_acledUndirected.rda'))){
	impData = data.matrix(data[,c(5:6,10,18:19,22:38,42:43,47:48,51,53)])
	sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
	dimnames(sbgData$Y.impute)[[2]] = colnames(sbgData$Y.pmean)
	save(sbgData, file=paste0(pathData, 'imputedData_acledUndirected.rda'))
} else { load(file=paste0(pathData, 'imputedData_acledUndirected.rda')) }

# randomly pick a few imputed datasets to use
set.seed(6886)
# sbgToUse = sample(500:1000, 150, replace=FALSE)
iData = lapply(500:1000, function(i){
	sbgFrom = sbgData$Y.impute[,,i]
	data = cbind(
		data[,c('cname','year','cnameYear','nActors','nConf')], 
		sbgFrom
		)
	return(data)	})
########################################################

########################################################
# run base model
modBase_noImp = glm.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + factor(cname) -1 
	, data=data
	)
summBase_noImp = summary(modBase_noImp)$'coefficients'
round(summBase_noImp[!grepl('factor',rownames(summBase_noImp)),], 3)


# check against random effect
# loadPkg('lme4')
# toKeep = names(table(data$cname)[table(data$cname)>5])
# slice = data[data$cname %in% toKeep,]
# slice$ccode = factor(slice$ccode)
# modBase_noImp_RE = glmer.nb(
# 	civVicCount ~  # dv
# 		graph_dens + nConf + nActors + (1|ccode)
# 	, data=slice
# 	)
# summary(modBase_noImp_RE)$'coefficients'


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

sqrt(mean(rmseVec1, na.rm=TRUE))
sqrt(mean(rmseVec2, na.rm=TRUE))

modsBase = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors + factor(cname) -1 
		, data=data
		)
	summary(mod)
	return(mod) })

# run mod with controls
modsCntrls = lapply(iData, function(data){
	mod = glm.nb(
		civVicCount ~  # dv
			graph_dens + nConf + nActors
			+ factor(cname) -1 
			+ polity2   # structural controls
			+ rebsStronger # capabilities gov/rebels
			+ ethTens
			+ anyPeaceKeeper 
			+ rebSupportGov + govSupportGov # external shit
		, data=data
		)
	summary(mod)
	return(mod) })

# summarize
summBase = rubinCoef(modsBase, TRUE)
summCntrls = rubinCoef(modsCntrls, TRUE)

#
round(summBase[!grepl('factor',rownames(summBase)),], 3)
round(summCntrls[!grepl('factor',rownames(summCntrls)),], 3)
########################################################