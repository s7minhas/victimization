# setup #########################################
source(paste0(here::here(), '/setup.R'))
################################################

# load in data #################################
load(paste0(pathData, 'abmData.rda'))
load(paste0(pathData, 'df_withVicCount.rda'))
load(paste0(pathData, 'dyadConf.rda'))
load(paste0(pathData, 'abmNetStats.rda'))
################################################

# create abm df for analysis ###################
# merge in hyperparams
netStats = data.frame(netStats)
abmData$game = 1:nrow(abmData)
hyperparams = paste0('V',1:11)
for(v in hyperparams){
	netStats$tmp = abmData[match(netStats$game,abmData$game),v]
	names(netStats)[ncol(netStats)] = v }

# merge in vic
netStats$vic = 0
netStats$id = with(netStats, paste(game, turn, sep='_'))
df$id = with(df, paste(gameID, turnID, sep='_'))
netStats$vic = df$vicCount[match(netStats$id,df$id)]

# merge in numConf
dyadConf$id = with(dyadConf, paste(gameIter, turnIter, sep='_'))
netStats$numConf = dyadConf$numConf[match(netStats$id, dyadConf$id)]
netStats$numConf[is.na(netStats$numConf)] = 0

#
save(netStats, file=paste0(pathData, 'netStats.rda'))
################################################
