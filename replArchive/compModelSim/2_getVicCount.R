# setup #########################################
source(paste0(here::here(), '/setup.R'))

# pkgs
loadPkg('stringr')

# helpers
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
################################################

# load in data #################################
abmData = read.csv(paste0(pathData, 'python39Run.csv'), header=FALSE, stringsAsFactors=FALSE)
save(abmData, file=paste0(pathData, 'abmData.rda'))
################################################

################################################
# calc gov strength
nActors = abmData$V1
nRegions = abmData$V2
govStrength = nRegions - nActors + 1
govStrength = data.frame(govStrength = govStrength, gameID = 1:nrow(abmData))
################################################

# victimization info ###########################
# clean stuff up
abmData$V12 = as.character(abmData[,12])
tmp = strsplit(abmData$V12, '],', fixed=TRUE)
out = lapply(tmp, cleaner)

# create turn level db
turnsPerGame = unlist(lapply(out, length))
numGames = length(out)
df = lapply(1:numGames, function(i){
	mat = cbind(i, 1:turnsPerGame[i])
	return(mat) }) %>% do.call('rbind', .)
df = data.frame(df, stringsAsFactors = FALSE)
names(df) = c('gameID', 'turnID')

# overall count of vic per turn
df$vicCount = 0
for(i in 1:nrow(df)){
	turnResults = out[[ df$gameID[i] ]][ df$turnID[i] ]
	if(nchar(turnResults)!=0){
		concatResult=trim(gsub(', ', '', turnResults, fixed=TRUE))
		concatResult=gsub(
			')','',gsub(
				'(','',concatResult,fixed=TRUE),fixed=TRUE)
		vicCount = nchar(concatResult) } else { vicCount=0 }
	df$vicCount[i] = vicCount }
################################################

################################################
# add in gov strength score
df$govStrength = govStrength$govStrength[
	match(df$gameID, govStrength$gameID)]
################################################

################################################
save(df, file=paste0(pathData, 'df_withVicCount.rda'))
################################################
