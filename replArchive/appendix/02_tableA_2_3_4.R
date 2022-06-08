########################################################
source(paste0(here::here(), '/setup.R'))

# helpful pkgs
loadPkg(c('xtable', 'stringr'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# org into list
dataList = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)
########################################################

########################################################
# variables for desc analysis
modVars = names(dataList[[3]])[-c(1:4,6,7,9,13:16)]

varKey = data.frame(
	dirty=modVars, stringsAsFactors = FALSE )
varKey$clean = c(
	'Civ. Victimization',
	'Num. Actors', 'Num. Conflicts',
	'Network Comp.',
	'Polity', 'Log(Pop.)', 'Log(GDP Cap.)',
	'Excl. Pop.', 'Peacekeepers',
	'Reb. Stronger Govt.',
	'Reb. Supp. by Foreign Govt.',
	'Govt. Supp. by Foreign Govt.' )

# reorder
varKey = varKey[c(1,4,2:3,5:nrow(varKey)),]
########################################################

########################################################
# desc tables for unimputed data
shh=lapply(1:length(dataList), function(ii){
	# subset to relev vars
	x = dataList[[ii]]
	vars = intersect(names(x),modVars)
	x = x[,vars]

	# get summStats
	summStats = reshape2::melt(x) %>%
		group_by(variable) %>%
		summarize(
			cnt = sum(!is.na(value)),
			lo = min(value,na.rm=TRUE),
			med = median(value, na.rm=TRUE),
			mu = mean(value, na.rm=TRUE),
			hi = max(value, na.rm=TRUE),
			sd = sd(value, na.rm=TRUE) )

	# clean up
	summStats[,2:ncol(summStats)] = round(
		summStats[2:ncol(summStats)], 2)
	summStats$variable = varKey$clean[
		match(summStats$variable,varKey$dirty)]
	out = data.matrix(summStats[,-1])
	rownames(out) = summStats$variable
	colnames(out) = c(
		'N','Min.','Median','Mean','Max.', 'Std. Dev.')
	for(p in 1:ncol(out)){out[,p]=char(out[,p])}

	# print
	print(xtable(out,
		align=c('l',rep('c',ncol(out))), digits=NULL ),
		file=paste0(pathGraphics,
      'appendix/table_A',(ii+1),'.tex') ) })
########################################################
