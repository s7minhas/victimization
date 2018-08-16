####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') 
}

# helpful pkgs
loadPkg('reshape2')
####

####
# load data
load(paste0(pathData, 'GEDdata.rda'))

# dv
dv = 'gedCivCountAny'

# netVars
vars = c(
	'graph_dens', 'graph_recip', 'graph_trans', 'nActors')

# idVars
idVars = c('cname','year')
####

####
# viz
data = data[,c(dv,vars,idVars)]
data$logCivVic = log(data$gedCivCountAny + 1)
data = data[,c(6:7,1:5,8)]

# plot dv vs graph_dens
makePlot = function(ggData, varName){
	names(ggData)[3] = 'iv'

	g=ggplot(ggData, aes(x=year, y=iv)) +
	geom_line() + geom_point() +
	labs(
		x='', 
		y=''
		) +
	facet_wrap(~cname, scales='free_y') +
	theme(
		axis.ticks = element_blank(),
		panel.border = element_blank(),
		legend.position = 'bottom',
		legend.title = element_blank()
		)
	ggsave(g, 
		file=paste0(pathGraphics, varName, 'timeSeriesTest.pdf'),
		width=12, height=10
		)
}

#
for(i in 3:ncol(data)){
	makePlot(data[,c(1:2,i)], names(data)[i])
}
####