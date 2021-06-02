####################################################
if(Sys.info()['user'] %in% c('Owner','herme','S7M')){
  source(paste0(
    'C:/Users/',Sys.info()['user'],
    '/Research/victimization/R/setup.R')) }
if(Sys.info()['user'] %in% c('s7m', 'janus829')){
  source('~/Research/victimization/R/setup.R') }
if(Sys.info()['user'] %in% c('dorffc')){
  source('~/ProjectsGit/victimization/R/setup.R') }

#libraries
loadPkg(c('igraph','network'))
####################################################

####################################################
# gen nets
set.seed(123456)

#low vic
gFirst <- graph.formula(1-2, 1-4, 1-6,  8, 9)
V(gFirst)$color <- "gray26"
s = coords <- layout_with_fr(gFirst) #get this layout and use it elsewhere
gFirst$layout <- coords
eN = ecount(gFirst) ; wts = runif(eN) ; wts = wts/sum(wts)
E(gFirst)$weight <-  8/eN + wts

#med vic
gSec <- graph.formula(1-4, 3-2, 2-1, 1-6, 1-5, 2-5, 1-4, 2-4, 10-4)
V(gSec)$color <- "gray26"
eN = ecount(gSec) ; wts = runif(eN) ; wts = wts/sum(wts)
E(gSec)$weight <-  8/eN + wts

#high vic
gLast <- graph.formula(
  1-2, 3-4, 1-3, 1-4, 1-5, 2-3, 2-4, 2-5, 3-4, 3-5, 4-1,
  2-4, 4-5, 4-6, 10-4, 8-4, 10-4, 1-8)
V(gLast)$color <- "gray26"
eN = ecount(gLast) ; wts = runif(eN) ; wts = wts/sum(wts)
E(gLast)$weight <-  8/eN + wts
####################################################

####################################################
lapply(list(gFirst, gSec, gLast), function(x){
  mat = data.matrix(
    as_adjacency_matrix(x, attr='weight'))
	aCnts = apply(mat, 1, sum, na.rm=TRUE)
	aShare = aCnts/sum(c(mat), na.rm=TRUE)
	herf = sum(aShare^2)
	iHerf = 1-herf
  out = c( sum(c(mat)), mean(c(mat)), ecount(x), iHerf ) })
####################################################

####################################################
par(mfrow=c(1, 3), mar=c(0,0,1,0))
#low vic
plot.igraph(gFirst,
            vertex.label=NA,
            main="Low Expected Victimization",
            edge.width=E(gFirst)$weight*3, layout=s, edge.curved=0.1)

#med vic
plot.igraph(gSec,
            vertex.label=NA,
            main="Moderate Expected Victimization",
            layout=s, edge.width=E(gSec)$weight*2, edge.curved=0.1)

#high vic
plot.igraph(gLast, vertex.label=NA,
            main="High Expected Victimization",
            layout=s, edge.width=E(gLast)$weight, edge.curved=0.1)
####################################################
