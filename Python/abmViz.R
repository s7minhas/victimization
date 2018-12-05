if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('maxgallop')){
	source('~/Documents/victimization/R/setup.R') }

# abm path
abmPath = paste0(pathDrop, 'abm/')
abmPath = paste0(pathGit, "python/")
# load in file
abmData = read.csv(paste0(abmPath, 'abmViz.csv'), header=FALSE)
# V12: who victimized and where
# V13 is what attacks took place
# V14 is groups + their territory per period
# V15 = adjacency matrix

###hmmmm, dont see any territory turnover
# as.character(abmData[1,'V14'])
# as.character(abmData[10,'V14'])
# as.character(abmData[50,'V14'])

# clean stuff up
abmData$V12 = char(abmData$V12)
tmp = strsplit(abmData$V12, '],', fixed=TRUE)
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
out = lapply(tmp, cleaner)

# create turn level db
turnsPerGame = unlist(lapply(out, length))
numGames = length(out)
df = lapply(1:numGames, function(i){
	mat = cbind(i, 1:turnsPerGame[i])
	return(mat) }) %>% do.call('rbind', .)
df = data.frame(df, stringsAsFactors = FALSE)
names(df) = c('gameID', 'turnID')