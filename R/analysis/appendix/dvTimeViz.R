########################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
	source(paste0(
		'C:/Users/',Sys.info()['user'],
		'/Research/victimization/R/setup.R')) }
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))
########################################################

########################################################
slice = dataBase

slice$herfCat = 'low'
slice$herfCat[slice$herf >= 0.25  & slice$herf < .5] = 'low-ish'
slice$herfCat[slice$herf == 0.5] = 'exactly .5'
slice$herfCat[slice$herf > 0.5  & slice$herf < .75] = 'mid'
slice$herfCat[slice$herf >= 0.75 ] = 'high'
slice$herfCat = factor(slice$herfCat,
	levels=c('low', 'low-ish', 'exactly .5', 'mid', 'high')
)

table(slice$cname, slice$herfCat)
table(slice$year, slice$herfCat)
########################################################

########################################################
# viz over time
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
ggplot(yrAgg, aes(x=year, y=herfMu, group=1)) +
  geom_line() + geom_point() +
	xlab('') + ylab('Network Competition (Average)') +
	scale_x_continuous(breaks=yrBrks) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0)
	)

# box plot version
library(gghalves)
n=0.02
ggAvg = ggplot(dataBase, aes(x=factor(year), y=herf) ) +
 	# geom_boxplot(size=.2, outlier.shape=NA)	+
 	# geom_violin(
	# 	kernel='rectangular',
	# 	alpha=.5,
	# 	aes(fill=year)
	# 	)	+
  # geom_half_boxplot(center=TRUE, errorbar.draw=FALSE,
  #                   width=0.5, nudge=n) +
  geom_half_violin(center=TRUE, nudge=n) +
  geom_half_dotplot(dotsize=0.3, alpha=0.2, fill="grey",
                    position=position_nudge(x=n, y=0)) +
	geom_line(
		data=yrAgg, aes(x=factor(year), y=herfMu, group=1),
		size=.4 ) +
	geom_point(data=yrAgg, aes(x=factor(year), y=herfMu, group=1),
	size=.6 ) +
	xlab('') + ylab('Network Competition') +
	scale_x_discrete(breaks=yrBrks) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0)
	)
ggAvg
# ggsave(
# 	ggAvg,
# 	width=8, height=5,
# 	file=paste0(pathGraphics, 'netCompAvg.pdf'),
# 	device=cairo_pdf )

# box plot version 2
ggplot(yrAgg, aes(x=factor(year), y=herfMu) ) +
 	geom_point(size=.6)	+
	geom_linerange(aes(ymin=herf25, ymax=herf75), size=.2) +
	# geom_linerange(aes(ymin=herf10, ymax=herf90), size=.2) +
	geom_line(aes(group=1), size=.2 ) +
	geom_jitter(
		data=dataBase,
		aes(x=factor(year), y=herf),
		alpha=.1, width=.1) +
	xlab('') + ylab('Network Competition') +
	scale_x_discrete(breaks=yrBrks) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0)
	)

#
ggplot(yrAgg, aes(x=year, y=herfMed ) ) +
 	geom_point(size=1)	+
	geom_ribbon(aes(ymin=herf25, ymax=herf75), alpha=.3) +
	# geom_linerange(aes(ymin=herf10, ymax=herf90), size=.2) +
	geom_line(aes(group=1), size=.5 ) +
	geom_jitter(
		data=dataBase,
		aes(x=year, y=herf),
		alpha=.1, width=.1) +
	xlab('') + ylab('Network Competition') +
	scale_x_discrete(breaks=yrBrks) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.y=element_text(hjust=0)
	)

#
ggplot(dataBase, aes(x=herf)) +
	geom_density() +
	geom_histogram(alpha=.3) +
	facet_wrap(~year, scales='free')

#
library(ggridges)
ggplot(dataBase, aes(x = herf, y = factor(year) )) +
	geom_density_ridges(
		scale = 4, alpha=.1,
		jittered_points = TRUE,
		position = position_points_jitter(width = 0.05, height = 0),
		point_shape = '|', point_size = 3, point_alpha = 1
	) +
	xlab("Network Competition") + ylab('') +
	theme(
		panel.border=element_blank(),
		axis.ticks=element_blank()
	)
########################################################

########################################################
# viz over time by countries

# add country acronyms for plots
dataBase$cabb = countrycode(dataBase$cname, 'country.name', 'cowc')

# # calc vic total and construct as ordered factor
# vicCntry = dataBase %>%
# 	group_by(cabb) %>%
# 	summarize(
# 		vic = sum(civVicCount, na.rm=TRUE),
# 		vicSD = sd(civVicCount, na.rm=TRUE)
# 	) %>%
# 	arrange(-vicSD) %>% data.frame(.,stringsAsFactors=FALSE)
#
# # # pick cntries based on variability in civ vic
# # # exclude ssd because of limited sample
# # cntries = c('ANG', 'NIG', 'SUD', 'DRC', 'ALG', 'BUI')
# # dataSel = dataBase[dataBase$cabb %in% cntries,]
#
# #
# cntries = vicCntry$cabb
dataSel = dataBase
#
# # create factor based on vic ordering
# dataSel$cabb = factor(dataSel$cabb, levels=cntries)

# plot
ggCntry = ggplot(
  dataSel, aes(x=year, y=herf, group=1) ) +
  geom_line(size=.4) + geom_point(size=.6) +
  facet_wrap(~cabb, scale='free_y') +
	xlab('') + ylab('Network Competition by Country') +
	theme_light(base_family="Source Sans Pro") +
	theme(
		legend.position='top', legend.title=element_blank(),
		panel.border=element_blank(), axis.ticks=element_blank(),
		axis.text.x=element_text(angle=45, hjust=1, size=7),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252'))
ggsave(
	ggCntry,
	width=8, height=8,
	file=paste0(pathGraphics, 'netCompCntry.pdf'),
	device=cairo_pdf )
########################################################
