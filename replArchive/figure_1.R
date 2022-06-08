####################################################
source(paste0(here::here(), '/setup.R'))

#libraries
loadPkg(c('igraph','ggraph','tidygraph'))
####################################################

####################################################
# gen nets
#low vic
gFirst <- graph.formula(
	1-+2, 1-+3, 1-+4, 1-+5,
	1-+6, 1-+7, 2-+1, 6-+1
)
V(gFirst)$color <- "gray26"
s = coords <- layout_with_fr(gFirst)
gFirst$layout <- coords

#high vic
gLast <- graph.formula(
	1-+2, 3-+4, 1-+5, 6-+7,
	6-+4, 1-+6, 3-+6, 7-+5
)
V(gLast)$color <- "gray26"
####################################################

####################################################
# get edgelists from igraph objects
nets = list(gFirst, gLast)
labs = paste(
	c('Low','High'),
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
  geom_edge_fan(aes(alpha = after_stat(index)), show.legend = FALSE) +
  geom_node_point(size=4) +
  facet_edges(~type, labeller=feLab) +
	theme_light(base_family="Source Sans Pro") +
	theme(
		axis.text = element_blank(),
		axis.title=element_blank(),
		axis.ticks=element_blank(),
		panel.grid.minor=element_blank(),
		legend.position='none',
		panel.border=element_blank(),
		strip.text.x = element_text(size = 9, color='white'),
		strip.background = element_rect(
			fill = "#525252", color='#525252') )
ggsave(gg,
	file=paste0(pathGraphics, 'Figure1.png'),
	width=8, height=3, dpi=600 )
####################################################
