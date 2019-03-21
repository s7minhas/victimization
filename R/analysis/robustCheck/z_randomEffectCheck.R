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
# check against random effect
loadPkg('lme4')
toKeep = names(table(data$cname)[table(data$cname)>5])
slice = data[data$cname %in% toKeep,]
slice$ccode = factor(slice$ccode)
for(v in c('nConf','nActors','graph_dens')){
	slice[,v] = (slice[,v] - mean(slice[,v]))/sd(slice[,v])
}
modBase_noImp_RE = glmer.nb(
	civVicCount ~  # dv
		graph_dens + nConf + nActors + (1|ccode)
	, data=slice
	)
summary(modBase_noImp_RE)$'coefficients'
########################################################