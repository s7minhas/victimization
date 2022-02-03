############################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }

# load extra libs
loadPkg(c('doParallel', 'foreach'))
############################

###########################
# load data
  # actorDates, actorsCT, yListAll,
  # actCnts, actCntsSumm, toKeep,
load(paste0(pathData, 'actorAdjList.rda'))
############################

############################
# cntries and years
cntries = names(yListAll)
yrs = names(yListAll[[1]])
############################

############################
# calc net stats
cl = makeCluster(20)
registerDoParallel(cl)
allyProp <- foreach(
	cntry = cntries,
	.packages=c('reshape2')
	) %dopar% {

	# calc cntry-year level alliance proportions
  # main fn here is getAllyProps and this is located
  # in R/funcs/allyPropHelpers.R

  # get adjacency matrices for country
	cntryList = yListAll[[cntry]]

  # remove null elements in cntry list
	cntryList = cntryList[!unlist(lapply(cntryList, is.null))]

  # check to make sure there are elements left in cntryList
  # cntryList
	if(length(cntryList)!=0){

    # set up frame to which we can add allyProp scores
		allyDF = data.frame(
      country=cntry, year=names(cntryList), stringsAsFactors=FALSE)

    # calc allyProp
		allyDF$allyProp = getAllyProps(cntryList)

  } else { allyDF=NULL }

	#
	return(allyDF) }
stopCluster(cl)

#
allyProp = do.call('rbind', allyProp)
############################

############################
save(allyProp,
  file=paste0(pathData, 'allyProp_acled.rda'))
############################
