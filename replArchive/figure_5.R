########################################################
source(paste0(here::here(), '/setup.R'))

# pkgs
loadPkg(c('patchwork'))
########################################################

########################################################
# load raw model data
load(paste0(pathData, 'rawModelData.rda'))

# load actor counts from acled
load(paste0(pathData, 'actorCntsID.rda'))
########################################################

########################################################
# visualize herf measure
# calc yearly averages
yrAgg = dataBase %>%
  group_by(year) %>%
  summarize(
    herfMu = mean(herf),
    herfMed = median(herf),
		herf25 = quantile(herf, .25),
		herf75 = quantile(herf, .75),
		herf10 = quantile(herf, .1),
		herf90 = quantile(herf, .9)
  )
yrBrks = round(seq(1997, 2020, length.out=4))

# plot
ggMed = ggplot(dataBase, aes(x=factor(year), y=herf) ) +
 	geom_violin(
		kernel='rectangular',
		alpha=.5,
		fill='grey60', color='grey60'
		)	+
	geom_jitter(
		data=dataBase,
		alpha=.1, width=.25) +
	geom_line(
		data=yrAgg, aes(x=factor(year), y=herfMed, group=1),
		size=.8, color='grey50' ) +
	geom_point(data=yrAgg,
		aes(x=factor(year), y=herfMed),
		size=20, shape='-', color='grey40' ) +
	xlab('') + ylab('Network Competition') +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0) )
####################################

####################################
# visualize changes in number of actors over time

# construct categorical var
actorCntsID$nCat3 = addCat(
	actorCntsID$nActors,
	c('4 or\nless','5-9','10 or\nmore actors'), c(5,10))

# viz diff in # actors over time via cat variable
actorCats = ggplot(actorCntsID,
  aes(
    x=factor(year),
    fill=factor(nCat3)
  )) +
  geom_bar(colour='grey40') +
  scale_fill_brewer(palette='Greys') +
  labs(
    x='',
    y='# Countries',
    fill=''
  ) +
	theme_light(base_family="Source Sans Pro") +
  smTheme +
  theme(
    axis.text.x=element_text(angle=45),
	 	legend.position='top' )
###########################################################

###########################################################
# organize into one plot using patchwork
top = actorCats + theme(
	axis.text.x=element_blank(),
	panel.grid.minor=element_blank()
	,plot.margin = unit(c(0,0,-10,0), "cm") )
bottom = ggMed + theme(
	axis.text.x=element_text(angle=90),
	panel.grid.minor=element_blank()
	,plot.margin = unit(c(-10,0,0,0), "cm") )
ggViz = top/bottom
ggsave(ggViz,
	file=paste0(pathGraphics, 'figure5.png'),
	width=8, height=6, dpi=600 )
###########################################################
