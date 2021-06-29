####################################################
source(paste0(here::here(), '/R/setup.R'))

#libraries
loadPkg(c('igraph','network'))
####################################################

####################################################
# gen nets
set.seed(123456)

#low vic
gFirst <- graph.formula(
	1-+2, 1-+3, 1-+4,  1-+5, 1-+6
)
# gFirst <- graph.formula(
# 	1-2, 1-3, 1-4, 1-5, 1-6, 1-7, 1-8, 1-9
# )
V(gFirst)$color <- "gray26"
s = coords <- layout_with_fr(gFirst) #get this layout and use it elsewhere
gFirst$layout <- coords
eN = ecount(gFirst) ; wts = runif(eN) ; wts = wts/sum(wts)
# E(gFirst)$weight <-  8/eN + wts

mat=data.matrix(as_adj(gFirst))

#med vic
gSec <- graph.formula(
	1-2, 2-3, 1-3, 1-4, 1-5, 1-6
)
V(gSec)$color <- "gray26"
eN = ecount(gSec) ; wts = runif(eN) ; wts = wts/sum(wts)
# E(gSec)$weight <-  8/eN + wts

#high vic
# gLast <- graph.formula(
#   1-2, 1-3, 1-4,  1-5, 1-6,
# 	2-3, 2-4,  2-5,
# 	3-4,  3-5,
# 	4-5,
# )
gLast <- graph.formula(
  1-2, 1-4,  1-6,
	2-3,  2-5,
	3-4,  3-5
)
V(gLast)$color <- "gray26"
eN = ecount(gLast) ; wts = runif(eN) ; wts = wts/sum(wts)
# E(gLast)$weight <-  8/eN + wts
####################################################

####################################################
lapply(list(gFirst, gSec, gLast), function(x){
  mat = data.matrix(
    as_adjacency_matrix(x))
	aCnts = apply(mat, 1, sum, na.rm=TRUE)
	aShare = aCnts/sum(c(mat), na.rm=TRUE)
	herf = sum(aShare^2)
	iHerf = 1-herf
  out = c( sum(c(mat)), mean(c(mat)), ecount(x), iHerf ) })
####################################################

####################################################
loadPkg(c('ggraph', 'tidygraph'))

# get edgelists from igraph objects
nets = list(gFirst, gSec, gLast)
labs = paste(
	c('Low','Moderate','High'),
	'Network Competition',
	# c('Low','Moderate','High'),
	'Expected Victimization'
)
ggData = lapply(1:length(nets), function(ii){
	edges = as_edgelist(nets[[ii]])
	edges = data.frame(edges, stringsAsFactors=FALSE)
	names(edges) = c('from', 'to')
	edges$type = ii
	return(edges) })
ggData = do.call('rbind', ggData)

# size nodes
ggGrph = as_tbl_graph(ggData)
ggGrph = ggGrph %>%
    mutate(Popularity = centrality_degree(mode = 'total'))
####################################################

####################################################
feLab = function(labels) { list(g = labs) }

ggraph(ggGrph, layout = 'kk') +
    geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
		# geom_edge_link() +
    geom_node_point(aes(size = Popularity)) +
		geom_node_label(aes(label=name)) +
    facet_edges(~type, labeller=feLab) +
		theme(
			legend.position='none'
		)
####################################################
