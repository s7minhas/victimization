####################################################
source(paste0(here::here(), '/R/setup.R'))

#libraries
loadPkg(c('igraph','network','ggraph','tidygraph'))
####################################################

####################################################
# gen nets
#low vic
gFirst <- graph.formula(
	1-+2, 1-+3, 1-+4,  1-+5, 1-+6, 1-+7
)
V(gFirst)$color <- "gray26"
s = coords <- layout_with_fr(gFirst)
gFirst$layout <- coords

#med vic
gSec <- graph.formula(
	1-2, 2-3, 1-3, 1-4, 1-5, 1-6, 1-7, 6-4
)
V(gSec)$color <- "gray26"

#high vic
gLast <- graph.formula(
  1-2, 1-4,  1-6, 1-3,
	2-3,  2-5, 2-6,
	3-4,  3-5, 4-6, 4-2,
	4-7, 7-3
)
V(gLast)$color <- "gray26"
####################################################

####################################################
# get edgelists from igraph objects
nets = list(gFirst, gSec, gLast)
labs = paste(
	c('Low','Moderate','High'),
	'Network Competition' )
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
set.seed(6886)
gg = ggraph(ggGrph, layout = 'fr') +
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = Popularity)) +
  facet_edges(~type, labeller=feLab) +
	theme(
		legend.position='none' )
ggsave(gg,
	file=paste0(pathGraphics, 'hypNet.pdf'),
	width=8, height=4 )
####################################################
