####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg('MASS')
####

####
# load data
load(paste0(pathData, 'GEDdata.rda'))
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
####

####
# run models
dv = 'gedCivCountAny'
vars = c(
	'graph_dens', 'nConf', 
	'nActors'
	# ,'polity2',
	# 'popLog', 'gdpLog',
	# 'ethTens',
	# 'anyPeaceKeeper',
	# 'loot_goods'
	)

modData = data[,c('cname','ccode','year',dv,vars)]

# # naive just impute everything
# loadPkg('sbgcop')
# # impData = data.matrix(modData[,c(5:7,11,19,20,23,24:38,45,49,50,53,55)])
# impData = data.matrix(modData[,-(1:3)])
# sbgData = sbgcop.mcmc(Y=impData, seed=6886, nsamp=1000, verb=FALSE)
# sbgFrom = sbgData$Y.pmean
# modData = cbind(modData[,c('cname','ccode','year')], sbgFrom)

# run mod
# toKeep = names(table(modData$cname)[table(modData$cname)>5])
# modData = modData[which(modData$cname %in% toKeep),]
for(v in c('graph_dens','nConf','nActors')){
	modData[,v] = (modData[,v] - mean(modData[,v]))/sd(modData[,v])
}
modForm = formula(
	paste0(
		dv, '~', 
		paste(vars, collapse='+'),
		' + (1|ccode)'
		)
	)
loadPkg('lme4')
mod = glmer.nb( modForm, data=modData)
summary(mod)
####