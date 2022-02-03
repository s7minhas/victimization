char = function(x){ as.character(x) }
num = function(x){ as.numeric(char(x)) }
trim = function (x) { gsub("^\\s+|\\s+$", "", x) }
pasteMult = function(x,y,sepZ){
	apply(expand.grid(x,y), 1, paste, collapse=sepZ) }
cname = function(x) {countrycode(x,'country.name','country.name')}
f = function(y,x){ formula(paste0(y, '~', paste(x, collapse='+'))) }
cleaner = function(x){
	out = trim(
		gsub('}','',
			gsub(']','',
				gsub(':','',
					gsub('[0-99]', '',
						gsub('[','',
							gsub('{', '', x,
								fixed=TRUE),fixed=TRUE)))),fixed=TRUE))
	return(out) }

# merge
simpleMerge = function(toData, fromData, vars, toID, fromID, lagVars=TRUE){

	# lag vars
	if(lagVars){
		fromData$yrForMerge = unlist(
			lapply(strsplit(fromData[,fromID],'_'),function(x){x[2]}))
		fromData$unitForMerge = unlist(
			lapply(strsplit(fromData[,fromID],'_'),function(x){x[1]}))
		fromData$yrForMerge = num(fromData$yrForMerge) + 1
		fromData[,fromID] = with(fromData,
			paste0(unitForMerge, '_', yrForMerge)) }

	# merge
	fromData = fromData[
		match(toData[,toID], fromData[,fromID]),]
	toData = cbind(toData, fromData[,vars])
	return(toData) }

# cat var
addCat = function(baseVar, labs, breaks){
	catVar = rep(labs[1], length(baseVar))
	for(ii in 1:length(breaks)){
		catVar[baseVar>=breaks[ii]] = labs[ii+1] }
	catVar = factor(catVar,
		levels = labs)
	return(catVar) }
options(warn=-1)

lagVar = function(
	data, vars,
	unit='cname', time='year',
	lagLength=1
	){

  # subset to relevant data and create id for matching
  data = data[,c(unit, time, vars)]
  data$id = paste(data[,unit], data[,time], sep='_')

  # create lagged version of data and move year forward
  lagData = data
  lagData$tlag = lagData[,time] + lagLength
  lagData$id = paste(lagData[,unit], lagData[,'tlag'], sep='_')

  # return lagged var back in order of original data
  lagVars = lagData[match(data[,'id'], lagData[,'id']), vars]
  lagVars = data.frame(lagVars, stringsAsFactors=FALSE)
  names(lagVars) = paste0('lag',lagLength,'_', vars)

  #
  return(lagVars) }
