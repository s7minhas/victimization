########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){
	source('~/ProjectsGit/victimization/R/setup.R') }

# helpful pkgs
loadPkg(c('xtable', 'stringr'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# org into list
dataList = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)

# basic samp stats
sampInfo = lapply(dataList, function(x){
	x = na.omit(x)
	time=summary(x$year)
	units = length(unique(x$cname))
	return(list(time,units)) })
########################################################

########################################################
# get list of countries in each sample
cntries = lapply(dataList, function(x){
	x = na.omit(x)
	sort(unique(x$cname))})
allCntries = sort(unique(unlist(cntries)))

# gen cntries tab
cntriesTab = matrix(' ',
	nrow=length(allCntries), ncol=3,
	dimnames=list(allCntries, c('Base', 'Cnt1', 'Cnt2')))

# lazily fill in
for(ii in 1:nrow(cntriesTab)){
	cntry = rownames(cntriesTab)[ii]
	b = cntry %in% cntries[[1]]
	c1 = cntry %in% cntries[[2]]
	c2 = cntry %in% cntries[[3]]
	if(b){ cntriesTab[ii,1] = 'X' }
	if(c1){ cntriesTab[ii,2] = 'X' }
	if(c2){ cntriesTab[ii,3] = 'X' } }

# gen latex tab for paper
### will clean up in paper
rownames(cntriesTab) = str_to_title(rownames(cntriesTab))
print(xtable(cntriesTab),
	file=paste0(pathResults, 'cntriesTab.tex'))
########################################################

########################################################
# variables for desc analysis
modVars = names(dataList[[3]])[-c(1:4,7,9,12:15)]

varKey = data.frame(
	dirty=modVars, stringsAsFactors = FALSE )
varKey$clean = c(
	'Civ. Victimization',
	'Graph Density', 'Num. Actors', 'Num. Conflicts',
	'Polity', 'Log(Pop.)', 'Log(GDP Cap.)',
	'Excl. Pop.', 'Peacekeepers',
	'Reb. Stronger Govt.',
	'Reb. Supp. by Foreign Govt.',
	'Govt. Supp. by Foreign Govt.' )
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
		file=paste0(pathResults,
			'descStatsRaw_',names(dataList)[ii],'.tex') ) })
########################################################

########################################################
# check variation in variables by country to
# help in choosing between FEs and REs
x = dataList[[3]]
x = x[,c('cname',modVars)]
x = reshape2::melt(x, id='cname')
cntryStats = x %>%
	group_by(cname, variable) %>%
	summarize(sd=round(sd(value, na.rm=TRUE),2))
avgSD=cntryStats %>% group_by(cname,variable) %>% summarize(musd=mean(sd, na.rm=T))
tmp = pivot_wider(avgSD, names_from=variable, values_from=musd)
########################################################

########################################################
# check number of obs by country by mod
cntryCnts = lapply(dataList, function(x){
	vars = intersect(names(x), c('cname',modVars))
	x = x[,c('cname',vars)]
	x = na.omit(x)
	out = x %>% group_by(cname) %>% summarize(cnt=n())
	return(out) })

# check based on imputation
load(paste0(pathData, 'modelDataCnt1.rda'))
dataCnt1 = iData[[1]]
load(paste0(pathData, 'modelDataCnt2.rda'))
dataCnt2 = iData[[1]]

# org into list
dataImpList = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)
cntryCntsImp = lapply(dataImpList, function(x){
	vars = intersect(names(x), c('cname',modVars))
	x = x[,c('cname',vars)]
	x = na.omit(x)
	out = x %>% group_by(cname) %>% summarize(cnt=n())
	return(out) })
cntryCntsImp
########################################################
