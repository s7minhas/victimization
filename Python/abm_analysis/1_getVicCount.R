# setup #########################################
if(Sys.info()['user'] %in% c('Owner','herme')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

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
abmPyPath = paste0(pathGit, '/Python/')
abmData1 = read.csv(paste0(abmPyPath, 'abmGovIdeoRandom.csv'), header = F)
abmData2 = read.csv(paste0(abmPyPath, 'abmGovIdeoRandom2.csv'), header = F)
abmData = rbind(abmData1,abmData2)
save(abmData, file=paste0(abmPath, 'abmData.rda'))
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
save(df, file=paste0(abmPath, 'df_withVicCount.rda'))
################################################
