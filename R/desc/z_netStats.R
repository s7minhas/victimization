if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
	source('~/Research/victimization/R/setup.R') }
pathGraphics = paste0(pathDrop, 'graphics/netStatsExplore/')

# reorg to df
load(paste0(pathData, 'netStats.rda'))
netDF = do.call('rbind', netStats)
rownames(netDF) = NULL

# sep actor and graph level stats
netGraphDF = unique(netDF[,c(
	'country','year',
	names(netDF)[15:17])])

netActorDF = netDF[,c(
	'country','actor','year',
	names(netDF)[1:14] )]

# graph level plots
ggNetGraphDF = gather(netGraphDF[,-c(1:2)])
g=ggplot(ggNetGraphDF, aes(x=value)) +
	geom_density() +
	facet_wrap(~key, scales='free')
ggsave(g, file=paste0(pathGraphics, 'graph_level_distributions.pdf'), width=8, height=3)

ggNetGraphCntryDF = gather(netGraphDF[,-c(2)], key=key, value=value, -country)
g = ggplot(ggNetGraphCntryDF, aes(x=country, y=value)) +
	geom_violin() + geom_point(alpha=.3) +
	facet_wrap(~key, scales='free') +
	xlab('') + ylab('') +
	theme(
		axis.text.x = element_text(angle=45, hjust=1, size=5)
		)
ggsave(g, file=paste0(pathGraphics, 'graph_level_distributions_by_country.pdf'), width=8, height=4)

# actor level plots
ggActorGraphDF = gather(netActorDF[,-c(1:3)])
g=ggplot(ggActorGraphDF, aes(x=value)) +
	geom_density() +
	facet_wrap(~key, scales='free')
ggsave(g, file=paste0(pathGraphics, 'actor_level_distributions.pdf'), width=8, height=6)

ggNetActorCntryDF = gather(netActorDF[,-c(2:3)], key=key, value=value, -country)
g = ggplot(ggNetActorCntryDF, aes(x=country, y=value)) +
	geom_violin() + geom_point(alpha=.3) +
	facet_wrap(~key, scales='free') +
	xlab('') + ylab('') +
	theme(
		axis.text.x = element_text(angle=45, hjust=1, size=5)
		)
ggsave(g, file=paste0(pathGraphics, 'actor_level_distributions_by_country.pdf'))
