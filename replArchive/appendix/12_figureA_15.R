##########################
source(paste0(here::here(), '/setup.R'))

# helpful pkgs
loadPkg(c('MASS', 'glmmTMB'))
##########################

##########################
# load data
load(paste0(pathData, 'data.rda'))

# rescale herf var for interp purposes
data$herf = 1-data$herf
##########################

##########################
# define vars from model
# yrs when listed account for lag
ids = c('id', 'cname', 'ccode', 'year')
dv = 'civVicCount'
ivsBase = c(
	'herf', 'graph_avgDeg',
	'nActors', 'nEvents', 'nConf'
)
ivCnt1 = c(
	'polity2', # -2019
	'gdp', 'pop', 'gdpCap', 'gdpLog', 'popLog', 'gdpCapLog', # -2020
	'exclpop' # -2018
)
ivCnt2 = c(
	'anyPeaceKeeper', # -2013
	'rebsStronger', 'rebSupportGov', 'govSupportGov' # -2015
)

# create subsets of data based on yr breaks
dataBase = data[,c(ids, dv, ivsBase)]
dataCnt1 = data[
	which(data$year<=2018),
	c(ids, dv, ivsBase, ivCnt1)]
dataCnt2 = data[
	which(data$year<=2015),
	c(ids, dv, ivsBase, ivCnt1, ivCnt2)]
##########################

##########################
# limit countries with too few cyears
dataList_strt = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)
modVars = names(dataCnt2)[-c(1:4,7,9,12:15)]

cntryCnts = lapply(dataList_strt, function(x){
	vars = intersect(names(x), c('cname',modVars))
	x = x[,c('cname',vars)]
	x = na.omit(x)
	out = x %>% group_by(cname) %>% summarize(cnt=n())
	return(out) })

# construct diff data based on cutoff
# no cutoff
cntryKeep = lapply(cntryCnts, function(x){ x$cname[x$cnt>=0] })
length(cntryKeep[[1]])
dataList = lapply(1:length(cntryKeep), function(ii){
	toKeep = cntryKeep[[ii]] ; dat = dataList_strt[[ii]]
	dat = dat[dat$cname %in% toKeep,]
	return(dat) })
dataBase_noCut = dataList[[1]]

# five yr cutoff
cntryKeep = lapply(cntryCnts, function(x){ x$cname[x$cnt>=5] })
length(cntryKeep[[1]])
dataList = lapply(1:length(cntryKeep), function(ii){
	toKeep = cntryKeep[[ii]] ; dat = dataList_strt[[ii]]
	dat = dat[dat$cname %in% toKeep,]
	return(dat) })
dataBase_fiveCut = dataList[[1]]
##########################

##########################
# set up spec for baseMod
dv = 'civVicCount'
ivs = c('herf', 'nConf', 'nActors')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))
##########################

##########################
# run base mods
feModBase_noCut = glm.nb(
	feF, data=dataBase_noCut)
reModBase_noCut = glmmTMB(
	reF, data=dataBase_noCut, family='nbinom2')
feModBase_fiveCut = glm.nb(
	feF, data=dataBase_fiveCut)
reModBase_fiveCut = glmmTMB(
	reF, data=dataBase_fiveCut, family='nbinom2')

# get coefs
feModBaseCoef_noCut = summary(
	feModBase_noCut)$'coefficients'[1:p,]
reModBaseCoef_noCut = summary(
	reModBase_noCut)$'coefficients'$cond[1:(p+1),]
feModBaseCoef_fiveCut = summary(
	feModBase_fiveCut)$'coefficients'[1:p,]
reModBaseCoef_fiveCut = summary(
	reModBase_fiveCut)$'coefficients'$cond[1:(p+1),]
##########################

##########################
coefBase = list(
	feNoCut = feModBaseCoef_noCut,
	reNoCut = reModBaseCoef_noCut,
	feFiveCut = feModBaseCoef_fiveCut,
	reFiveCut = reModBaseCoef_fiveCut )

varKey = data.frame(
	dirty=rownames(coefBase[[1]]), stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors')

mLabs = c(
	'Base ACLED Model\n(Fixed Effects, No Min.)',
	'Base ACLED Model\n(Random Effects, No Min.)',
	'Base ACLED Model\n(Fixed Effects, Five Min.)',
	'Base ACLED Model\n(Random Effects, Five Min.)' )
##########################

##########################
# process and viz
ggDataBase_cuts = coefProcess(coefBase)
coefViz(ggDataBase_cuts, 'appendix/figure_A15.png')
##########################
