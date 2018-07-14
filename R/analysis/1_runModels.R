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
load(paste0(pathData, 'data.rda'))
####

####
# run models
dv = 'civVicCount'
vars = c(
	'graph_trans', 'nActors', 
	'polity2', 'rebsStronger',
	'ethTens', 'anyPeaceKeeper', 
	'rebSupportGov', 'govSupportGov'
	)
modData = data[,c('cname','ccode','year',dv,vars)]

mod = glm.nb(
	civVicCount ~  # dv
		graph_dens*nActors + graph_dens + nActors + # net measures
		polity2 +   # structural controls
		# rebsStronger + # capabilities gov/rebels
		ethTens +
		anyPeaceKeeper 
		# + rebSupportGov + govSupportGov # external shit
	, data=data
	)
summary(mod)
####

####
cor(data[,c(dv,'graph_recip','graph_trans','graph_dens')], use='pairwise.complete.obs')[-1,1]

cntries = unique(data$cname)
corStats = lapply(cntries, function(x){
	slice = data[which(data$cname==x),]
	stats=cor(slice[,c(dv,'graph_recip','graph_trans','graph_dens')], use='pairwise.complete.obs')[-1,1]	
	return(round(stats,3))
})
corDF = data.frame(do.call('rbind', corStats))
corDF$cname = cntries
corDF[order(corDF$graph_dens),]
####