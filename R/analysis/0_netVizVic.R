####
if(Sys.info()['user'] %in% c('s7m', 'janus829')){ 
	source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('cassydorff')){ 
	source('~/ProjectsGit/victimization/R/setup.R') }

#libraries
library(igraph)
library(network)

set.seed(12345)
#low vic
gFirst <- graph.formula(1-2, 3-4, 5-6,  7-8, 8, 9, 8-10)
V(gFirst)$color <- "gray26"
s <- layout_with_fr(gFirst) #get this layout and use it elsewhere
gFirst$layout <- coords
plot(gFirst, vertex.label=NA, main="Low Victimization", layout=s, edge.curved=0.1)

#med vic
gSec <- graph.formula(2-4, 3-4, 4-5, 4-6, 5-4, 6-5, 7-5, 8-4, 9-4, 10-4)
V(gSec)$color <- "gray26"
E(gSec)$weight <-c(.5, .5, .5, 1, 1, 1, .5,.5,.5)
#l <- layout_in_circle(gMed)
#s <- layout_with_kk(gMed)
plot(gSec, vertex.label=NA, main="Moderate Victimization", 
		layout=s, edge.width=E(gSec)$weight, edge.curved=0.1)

#high vic
gLast <- graph.formula(1-2, 1-3, 1-4, 1-5, 2-3, 2-4, 2-5, 3-4, 3-5, 4-1, 4-2, 4-5, 4-6,
                   4-7, 4-8, 4-9, 4-10,5-6, 6-1, 6-2, 6-3, 6-7, 6-8, 6-7, 7-8, 8-9, 8-10, 9-10)
V(gLast)$color <- "gray26"
E(gLast)$weight <- runif(ecount(gLast)) #random value from uniform for each edge
plot(gLast, vertex.label=NA, main="High Victimization", 
		layout=s, edge.width=E(gLast)$weight*3, edge.curved=0.1)

par(mfrow=c(1,3))
