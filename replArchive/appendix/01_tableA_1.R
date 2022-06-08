########################################################
source(paste0(here::here(), '/setup.R'))

# helpful pkgs
loadPkg(c('xtable', 'stringr'))
########################################################

########################################################
# load raw model data (dataBase, dataCnt1, dataCnt2)
load(paste0(pathData, 'rawModelData.rda'))

# org into list
dataList = list(base=dataBase, cnt1=dataCnt1, cnt2=dataCnt2)

# basic samp stats
sampInfo = lapply(dataList, function(x){
	x = na.omit(x)
	time=summary(x$year)
	units = length(unique(x$cname))
	return(list(time,units)) })
########################################################

########################################################
# get list of countries in each sample
cntries = lapply(dataList, function(x){
	x = na.omit(x)
	sort(unique(x$cname))})
allCntries = sort(unique(unlist(cntries)))

# gen cntries tab
cntriesTab = matrix(' ',
	nrow=length(allCntries), ncol=3,
	dimnames=list(allCntries, c('Base', 'Cnt1', 'Cnt2')))

# lazily fill in
for(ii in 1:nrow(cntriesTab)){
	cntry = rownames(cntriesTab)[ii]
	b = cntry %in% cntries[[1]]
	c1 = cntry %in% cntries[[2]]
	c2 = cntry %in% cntries[[3]]
	if(b){ cntriesTab[ii,1] = 'X' }
	if(c1){ cntriesTab[ii,2] = 'X' }
	if(c2){ cntriesTab[ii,3] = 'X' } }

# gen latex tab for paper
### will clean up in paper
rownames(cntriesTab) = str_to_title(rownames(cntriesTab))
print(xtable(cntriesTab),
	file=paste0(pathGraphics, 'appendix/table_A1.tex'))
########################################################
