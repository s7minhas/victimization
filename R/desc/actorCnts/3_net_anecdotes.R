###########################################################
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }

if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  u = Sys.info()['user']
  source(paste0('C:/Users/',u,'/Research/victimization/R/setup.R')) }

if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

loadPkg(c(
  'readr', 'abind', 'gridExtra'
))

## NOTE:
## TO DO: could remove actCtsId and use the dyn data bc it has both
## fatals and actor count
## also so far this analysis is kind of useless

###########################################################

###########################################################
## load data & explore based on familiar cases
load(paste0(pathData, 'actorCntsID.rda'))
load(paste0(pathData, 'netStats.rda'))
load(paste0(pathData, 'actConfDynFat.rda'))
load(paste0(pathData, 'actorAdjList.rda'))


# potentially good candidates for anecdotes:
actorCntsID[actorCntsID$country=="Pakistan",]
actorCntsID[actorCntsID$country=="Nigeria",]
actorCntsID[actorCntsID$country=="Central African Republic",]
actorCntsID[actorCntsID$country=="Brazil",]
actorCntsID[actorCntsID$country=="Colombia",] #why only enters in recently?
###########################################################

###########################################################
## quick comparison plot of two cases with similar years
toPlot <- actorCntsID %>% 
          filter(country=="Pakistan" | country=="Nigeria") %>%
          filter(year>=2010 & year<=2019)
pak_nigeria<-
  ggplot(data=toPlot, aes(x=as.factor(year), y=nActors, group=country)) +
  geom_line()
pak_nigeria
###########################################################

###########################################################
## these cases look interesting, but what is the rel btwn
## density and # of actors and vic over time for one case?
## get density over time
nigeriaGraph <- netStats$Nigeria
nigeriaGraph <- nigeriaGraph %>%
              select(year, graph_dens, graph_recip, graph_trans, country) %>%
              filter(year>=2000 & year <=2019) %>%
              group_by(year)

## get nActors over time
nigeriaActs <- actorCntsID %>%
              filter(country=="Nigeria") %>%
              filter(year>=2000 & year <=2019)

## get fatalities over time
nigeriaFats <- actConfDynFat %>%
  filter(country=="Nigeria", fatals=="Civilian Fatalities") %>%
  filter(year>=2000 & year <=2019)
  
plot_nigeria_density<-
  ggplot(data=nigeriaGraph, aes(x=year, y=graph_dens)) + 
  geom_line() +
  xlab('') + ylab('Graph Density')
plot_nigeria_density

plot_nigeria_actors <-
  ggplot(data=nigeriaActs, aes(x=year, y=nActors)) + 
  geom_line() + 
  xlab('') + ylab('Number of Actors')
plot_nigeria_actors

plot_nigeria_fatals <-
  ggplot(data=nigeriaFats, aes(x=year, y=value)) + 
  geom_line() + 
  xlab('') + ylab('Number of Civilian Fatalities')
plot_nigeria_fatals

grid.arrange(plot_nigeria_actors, plot_nigeria_density, plot_nigeria_fatals)

## what about the network? (Cd currently isn't sure what to make of this)
## unfortunately the data i grabbed for this only goes to 2016
## need to remake
set.seed(12344)
netNigeria <- yListAll$Nigeria
netNigeria2008 <- netNigeria$`2008`
netNigeria2008 <- graph_from_incidence_matrix(netNigeria2008, weighted=TRUE)
V(netNigeria2008)$label <- NA
plot(netNigeria2008, vertex.size=4, vertex.color="gray50")

netNigeria2014 <- netNigeria$`2014`
netNigeria2014 <- graph_from_incidence_matrix(netNigeria2014, weighted=TRUE)
V(netNigeria2014)$label <- NA
plot(netNigeria2014, vertex.size=4, vertex.color="gray50")

netNigeria2016 <- netNigeria$`2016`
netNigeria2016 <- graph_from_incidence_matrix(netNigeria2016, weighted=TRUE)
V(netNigeria2016)$label <- NA
plot(netNigeria2016, vertex.size=4, vertex.color="gray50")

