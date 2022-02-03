############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }

# load extra libs
loadPkg(c(
  'doParallel', 'foreach',
  'MASS', 'glmmTMB' ))
############################

############################
# load data
load(paste0(pathData, 'GEDEvent_v21_1.RData'))
ged = data.frame(GEDEvent_v21_1,stringsAsFactors=F)
rm(GEDEvent_v21_1)
############################

############################
# subset to relevant vars and event types
ged = ged[,c(
	'id','year','date_start','date_end',
	'type_of_violence','conflict_name',
	'dyad_new_id','dyad_name',
	'side_a_new_id','side_a',
	'side_b_new_id','side_b',
	'country', 'longitude', 'latitude','where_prec',
	'best', 'deaths_a', 'deaths_b', 'deaths_civilians'
	)]

# add cname
cKey = data.frame(
  dirty=sort(unique(ged$country)),
  stringsAsFactors=F )
cKey$clean = cname(cKey$dirty)
cKey$clean[cKey$dirty=='DR Congo (Zaire)'] = cname('Dem Rep Congo')
ged$cname = cKey$clean[match(ged$country, cKey$dirty)]

# subset to same countries and years as
# what was decided for in acled
load(paste0(pathData, 'rawModelData.rda'))
cntries = sort(unique(dataBase$cname))
yrs = sort(num(dataBase$year))
ged = ged[ged$cname %in% unique(dataBase$cname),]
ged = ged[ged$year %in% unique(dataBase$year),]

# get osv country year counts, which will
# serve as dv for ucdp analysis
osv = ged[ged$type_of_vi == 3,]
osv = osv %>%
  group_by(cname, year) %>%
  summarize(
    eventCnt=n(),
    best=sum(best),
    deaths_civilians=sum(deaths_civilians)
  ) %>%
  mutate(id=paste(cname, year, sep='_')) %>%
  data.frame(.,stringsAsFactors=F)

# subset to battles so we can calc net stats
ged = ged[ged$type_of_vi %in% c(1:2),]
############################

############################
# get net stats for each country-year

##############
# prep actor list for ged
# clean actor names for ged
ged$a1 = trim(ged$side_a)
ged$a2 = trim(ged$side_b)
##############

##############
# place actors separated by
# commas in different rows
expandCommaRows = function(var, data){

  # determine rows with comma
  commaRows = grep(', ', data[,var])
  out = data[-commaRows,]
  crow = commaRows[2]

  # iterate and expand rows w/ mult actors
  expRows = lapply(commaRows, function(crow){
    # get slice of data
    slice = data[crow,]
    actors = unlist(strsplit(slice[,var], ', '))
    # iterate through actors and create dupes of row
    newRows = lapply( actors, function(actor){
      slice[,var] = actor ; return(slice) })
    newRows = do.call('rbind', newRows)
    return(newRows) })

  # create new df and return
  out = rbind(out, do.call('rbind', expRows))
  return(out) }

# apply fn to a1 and a2
ged = expandCommaRows('a1', ged)
ged = expandCommaRows('a2', ged)
##############

##############
# get dates actors were active
orig = ged ; revOrig = orig
revOrig$a2 = orig$a1 ; revOrig$a1 = orig$a2
tmp = rbind(orig, revOrig)
yrs=seq(min(ged$year), max(ged$year), by=1)

# get min/max for each actor
actorDates = tmp %>%
  group_by(a1, country) %>%
  summarize(
    year.min = min(year),
    year.max = max(year) ) %>%
  data.frame(.,stringsAsFactors=F)

# get list of actors by country-year
actorsCT = lapply(unique(actorDates$country), function(cntry){
	aDateSlice = actorDates[which(actorDates$country==cntry),]
	actorsT = lapply( yrs, function(t){
	  actors = NULL
	  for( ii in 1:nrow(aDateSlice)){
	     if( t %in% aDateSlice$year.min[ii]:aDateSlice$year.max[ii] ) {
	      actors = append(actors, aDateSlice$a1[[ii]]) } }
	  return(actors)
	}) ; names(actorsT) = yrs
	return(actorsT) }) ; names(actorsCT) = unique(actorDates$country)
##############

##############
# org data into adj mats
yListAll = lapply(names(actorsCT), function(cntry){
	nData = ged[ged$country==cntry,]
	nData$dv = 1 ; yVar = 'dv'
	actorsT = actorsCT[[cntry]]
	yList = lapply(yrs, function(ii){
		if(
			is.null(actorsT[[char(ii)]]) |
			length(actorsT[[char(ii)]])<2){
			return(NULL)
		}
		actorSlice = actorsT[[char(ii)]]
		slice = nData[ which(
			nData$year==ii &
			nData$a1 %in% actorSlice &
			nData$a2 %in% actorSlice
			), c('a1', 'a2', yVar) ]
		if(nrow(slice)==0){return(NULL)}
		adjMat = matrix(0,
			nrow=length(actorSlice), ncol=length(actorSlice),
			dimnames=list(actorSlice,actorSlice) )
		for(r in 1:nrow(slice)){ adjMat[slice$a1[r],slice$a2[r]]=1  }
		return(adjMat)
	}) ; names(yList) = yrs
	return(yList)
}) ; names(yListAll) = names(actorsCT)
##############

##############
# calc net stats
loadPkg(c('doParallel', 'foreach'))
cl = makeCluster(20)
registerDoParallel(cl)
netStats <- foreach(
	cntry = names(yListAll)
	) %dopar% {
	cntryStats = lapply(yrs, function(t){
		mat = yListAll[[cntry]][[char(t)]]
		if(is.null(mat)){return(NULL)}

		# gen desc stats
    nActors = nrow(mat)
		nEvents = sum(c(mat))/2

		# herf index
		aCnts = apply(mat, 1, sum, na.rm=TRUE)
		aShare = aCnts/sum(c(mat), na.rm=TRUE)
		herf = sum(aShare^2)

    #
		out = data.frame(
      nActors=nActors,
			nEvents=nEvents,
			herf=herf,
			year=t )
		out$country = cntry
		rownames(out) = NULL ; return(out) })
	cntryDF = do.call('rbind', cntryStats)
	return(cntryDF)
}
stopCluster(cl)
netStats = do.call('rbind', netStats)
##############

##############
# add cname labs to netStats
netStats$cname = cKey$clean[match(netStats$country, cKey$dirty)]
netStats$id = with(netStats, paste(cname, year, sep='_'))
##############
############################

############################
# org into df with covars from
# main analysis

##############
# load data for modeling
load(paste0(pathData, 'modelDataCnt2.rda'))

# randomly sample iData
set.seed(6886)
toSamp = sample(1:length(iData), 10, replace=FALSE)
dataList = iData[toSamp]

# add ucdp calcs
dataList = lapply(dataList, function(dat){
	dat$civVicCountU = osv$deaths_civilians[match(dat$id, osv$id)]
	dat$civVicCountU[is.na(dat$civVicCountU)] = 0
	dat$herfU = netStats$herf[match(dat$id, netStats$id)]
	return(dat) })
##############

##############
# base mod spec
dv = 'civVicCountU'
ivs = c('herf', 'nConf', 'nActors')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# iterate across imputed datasets
feModBase_imp = lapply(dataList, function(dataImp){
  glm.nb(feF, data=dataImp) })
reModBase_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModBaseCoef_imp = lapply(feModBase_imp,
  function(x){ summary(x)$'coefficients'[1:p,] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
reModBaseCoef_imp = lapply(reModBase_imp,
  function(x){ summary(x)$'coefficients'$cond[1:(p+1),] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
##############

##############
# cnt 1
ivs = c( ivs,
  'polity2', 'popLog', 'gdpCapLog',
  'exclpop', 'anyPeaceKeeper' )
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# iterate across imputed datasets
feModCnt1_imp = lapply(dataList, function(dataImp){
  glm.nb(feF, data=dataImp) })
reModCnt1_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModCnt1Coef_imp = lapply(feModCnt1_imp,
  function(x){ summary(x)$'coefficients'[1:p,] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
reModCnt1Coef_imp = lapply(reModCnt1_imp,
  function(x){ summary(x)$'coefficients'$cond[1:(p+1),] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
##############

##############
# cnt 2
ivs = c( ivs,
  'rebsStronger', 'rebSupportGov', 'govSupportGov')
p = length(ivs)
feF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+factor(cname)-1'))
reF = formula(paste0(
  dv, '~', paste(ivs, collapse='+'), '+(1|cname)'))

# iterate across imputed datasets
feModCnt2_imp = lapply(dataList, function(dataImp){
  glm.nb(feF, data=dataImp) })
reModCnt2_imp = lapply(dataList, function(dataImp){
  glmmTMB(reF, data=dataImp, family='nbinom2') })

# get coefs
feModCnt2Coef_imp = lapply(feModCnt2_imp,
  function(x){ summary(x)$'coefficients'[1:p,] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
reModCnt2Coef_imp = lapply(reModCnt2_imp,
  function(x){ summary(x)$'coefficients'$cond[1:(p+1),] }) %>%
  do.call('rbind', .) %>% rubinCoef(.)
##############
############################

############################
# get viz

# org coefs
coefFE = list(
	base=feModBaseCoef_imp,
	cnt1=feModCnt1Coef_imp,
	cnt2=feModCnt2Coef_imp )

coefRE = list(
	base=reModBaseCoef_imp,
	cnt1=reModCnt1Coef_imp,
	cnt2=reModCnt2Coef_imp )

# add labels
mLabs = c(
	'Base UCDP Model',
	'Base + Controls\n(1997-2018)', 'Base + Controls\n(1997-2015)' )

varKey = data.frame(
	dirty=coefFE$'cnt2'$var, stringsAsFactors = FALSE )
varKey$clean = c(
	'Network\nCompetition', 'Number of\nConflicts', 'Number of\nActors',
	'Polity', 'Log(Population)', 'Log(GDP per Capita)',
	'Excluded\nPopulation', 'Presence of\nPeacekeepers',
	'Rebel(s) Stronger\nthan Govt.',
	'Rebel(s) Supported\nby Foreign Govt.',
	'Govt. Supported\nby Foreign Govt.' )

# process coefs
ggDataFE = coefProcess(coefFE)
ggDataRE = coefProcess(coefRE)

# create and save viz to pathGraphics
coefViz(ggDataFE, 'coefPlot_FE_ucdp.pdf')
coefViz(ggDataRE, 'coefPlot_RE_ucdp.pdf')
############################
