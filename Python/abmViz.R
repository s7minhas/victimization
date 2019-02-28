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

###hmmmm, dont see any territory turnover, lets make a fn to check
abmData$V14 = char(abmData$V14)
terrChange = rep(NA, nrow(abmData))
# for(i in 1:nrow(abmData)){
i=1
	# split string element into a vector using turn ids
	# that come at beginning of string
	x = strsplit(abmData[i,'V14'], '[0:9]*: ')
	# remove turn id for first turn
	x = x[[1]][-1]
	# remove turn ids that got folded into end of each string
	x = lapply(x, function(z){gsub('\\], [0-9]*', '', z)})
	# remove any curly braces
	#  ... only seem to appear for last turn, prolly dont need lapply
	x = lapply(x, function(z){gsub('}', '', z, fixed=TRUE)})
	# remove square bracket wrapping beginning of seach character element
	x = lapply(x, function(z){substring(z,2)})
	# remove end square bracket from last turn
	x[[length(x)]] = substring(x[[length(x)]], 1, nchar(x[[length(x)]])-1)

	# final clean up to assess whether there is terr change
	x = lapply(x, function(z){
		# remove all commas
		z = gsub(', ', ' ', z)
		# remove parentheses that wrap each actor
		z = gsub('\\(|\\)','',z)
		# extract terrs for each actor into vector
		# remove the first element because it will be empty
		terrs = gsub('\\[|\\]','',trim(strsplit(z, '[A-Z]')[[1]][-1]))
		# extract all letters to get corresponding actors for terrs
		actors = regmatches(z, gregexpr("[[:alpha:]]+", z))[[1]]
		names(terrs) = actors
		return(sort(terrs)) })
	out = ifelse(length(unique(x))==1, 'no territory change', 'woohoo')
	terrChange[i] = out }
unique(terrChange)

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