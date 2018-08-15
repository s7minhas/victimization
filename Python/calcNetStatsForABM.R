if(Sys.info()['user'] %in% c('s7m', 'janus829')){ source('~/Research/victimization/R/setup.R') }

# abm path
abmPath = paste0(pathGit, 'Python/')

# load in file
abmData = read.csv(paste0(abmPath, 'abmresults1.csv'), header=FALSE)

#
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

# if you want counts of vic by actor
# then finish organizing this code
# getCount = function(x){
# 	res = table(trim(unlist(strsplit(x,','))))
# 	if(length(res)==0){return(0)}
# 	actors = names(res)

# }

# sapply(x, getCount)

# overall count of vic per turn
if(!file.exists(paste0(abmPath, 'df_withVicCount.rda'))){
	df$vicCount = 0
	for(i in 1:nrow(df)){
		turnResults = out[[ df$gameID[i] ]][ df$turnID[i] ]
		if(nchar(turnResults)!=0){
			concatResult=trim(gsub(', ', '', turnResults, fixed=TRUE))
			vicCount = nchar(concatResult) } else { vicCount=0 }
		df$vicCount[i] = vicCount }
	save(df, file=paste0(abmPath, 'df_withVicCount.rda'))
} else { load(paste0(abmPath, 'df_withVicCount.rda')) }

# net stats
abmData$V13 = char(abmData$V13)
tmp = strsplit(abmData$V13, '],', fixed=TRUE)
out = lapply(tmp, cleaner)
dyadConf = do.call('rbind', lapply(1:length(out), function(gameIter){
	x = out[[gameIter]]
	toExtractFrom = strsplit(x, '),',fixed=TRUE)
	gameSumm = lapply(1:length(toExtractFrom), function(turnIter){
		z = toExtractFrom[[turnIter]]
		z = gsub(')','',gsub('(', '', z, fixed=TRUE),fixed=TRUE)
		zMat = do.call('rbind', lapply(strsplit(z, ', ', z, fixed=TRUE), function(u){t(cbind(u))}))
		zMat[,1] = trim(zMat[,1]) ; zMat[,2] = trim(zMat[,2])
		zMat = cbind(gameIter, turnIter, zMat)
		if(length(z)==0){ zMat = NULL }		
		return(zMat) })
	return(do.call('rbind', gameSumm)) }) )

# clean up edge list
dyadConf = data.frame(dyadConf, stringsAsFactors = FALSE)
dyadConf$gameIter = num(dyadConf$gameIter)
dyadConf$turnIter = num(dyadConf$turnIter)
dyadConf$V3 = char(dyadConf$V3)
dyadConf$V4 = char(dyadConf$V4)
rownames(dyadConf) = NULL

# get actor list for each game iter
abmData$V14 = char(abmData$V14)
tmp = strsplit(abmData$V14, '],', fixed=TRUE)
out = lapply(tmp, cleaner)
actorSet = lapply(out, function(x){
	lapply(x, function(turn){
		actors=sort(trim(unlist(strsplit(turn, ','))))
		n=length(actors)
		adjMat=matrix(0,nrow=n,ncol=n,dimnames=list(actors,actors))
	})	 })

# fill in adjmat
for(i in 1:nrow(dyadConf)){
	game = dyadConf$gameIter[i]
	turn = dyadConf$turnIter[i]
	sender = dyadConf$V3[i]
	receiver = dyadConf$V4[i]
	actorSet[[game]][[turn]][sender,receiver] = 1 }

