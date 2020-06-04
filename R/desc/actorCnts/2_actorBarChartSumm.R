###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }
###########################################################

###########################################################
load(paste0(pathData, 'actorCntsID.rda'))
###########################################################

###########################################################
# viz diff in # actors over time via cat variable
actorCats = ggplot(actorCntsID,
  aes(
    x=factor(year),
    fill=factor(nCat)
  )) +
  geom_bar() +
  scale_fill_brewer(palette='Dark2') +
  labs(
    x='',
    y='# Active Armed Groups',
    fill=''
  ) +
  smTheme +
  theme(
    axis.text.x=element_text(angle=45)
  )
ggsave(actorCats,
  file=paste0(pathGraphics, 'actorCntCats.pdf'),
  width=8, height=6
)
###########################################################
