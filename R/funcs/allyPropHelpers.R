# helpers to calculate proportion of "latent" alliances in system

####
# get sorted, unique vector of rownames from list of mats
getListRows = function(l){ sort(unique(unlist(lapply(l, rownames)))) }
####

####
# create sum over matrices with different dims, dims will
# be determined by total number of unique actors across the mats
sumMats = function(mList){

	# get vector of actors
	ids = getListRows(mList) ; idsN = length(ids)

	# set up matrix to store agg info
	fMat = matrix(0, nrow=idsN, ncol=idsN, dimnames=list(ids,ids))

	# fill in mat
	for(jj in 1:length( mList )){
		toAdd = mList[[jj]] ; toAddIDs = rownames(toAdd)
		fMat[toAddIDs,toAddIDs] = toAdd + fMat[toAddIDs,toAddIDs] }

	#
	return(fMat) }
####

####
# get proportion of allied actors
# at each iteration of the game
#### note: this function depends on reshape2
getAllyProps = function(netList){

	# create gameSumm list where we store rolling longit
	# info about actor interactors, list starts with
	# single mat at t, next mat in the list will sum
	# interactions in t and t+1, etc
	gameSumm = vector('list', length=length(netList))
	gameSumm[[1]] = netList[[1]]
	if(length(gameSumm)>1){
		gameSumm[2:length(gameSumm)] = lapply(
			2:length(netList), function(ii){

			# subset to lists that we want to sum over
			gamePastList = netList[1:ii]

			# check if actor composition changes
			actorCompStatic = Reduce(
				identical,
				lapply(gamePastList, function(x){ sort(rownames(x)) }) )

			# choose method to sum over depending on above check
			if(actorCompStatic){
				out = Reduce('+', gamePastList)
			} else {
				out = sumMats(gamePastList) }

			return(out) }) }

	# calculate summary measure of level of alliances
	# between actors in system at every point in the game
	allyProp = lapply(gameSumm, function(mat){

		# set diags to zero
		diag(mat)=0

		# stdz
		matZ = (mat - mean(c(mat))/sd(c(mat), na.rm=TRUE))

		# run decomp
		mod = svd(matZ)

		# get cosine similarity scores and org
		simRels = cosSimMat(t(mod$u[,1:2]))
		diag(simRels) = NA
		simRels = data.matrix(simRels)
		rownames(simRels) = colnames(simRels) = rownames(mat)

		# require an actor to have at least one conflict event
		# for them to really be involved in the system
		isos = which((rowSums(mat) + colSums(mat))==0)
		for(iso in isos){ simRels[iso,] = 0 ; simRels[,iso] = 0 }

		# actors cannot be allies if they have fought up
		# until that point
		edges = reshape2::melt(mat)
		edges$Var1 = char(edges$Var1) ; edges$Var2 = char(edges$Var2)
		edges = edges[edges$value>0,]
		edges = edges[edges$Var1 != edges$Var2,]
		for(ii in 1:nrow(edges)){
			simRels[edges$Var1[ii], edges$Var2[ii]] = 0
			simRels[edges$Var2[ii], edges$Var1[ii]] = 0 }

		# calculate level of alliance in system
		out = sum(c(simRels)>.99, na.rm=TRUE)/( nrow(simRels)^2-nrow(simRels) )

		#
		return(out) })

		#
		return(unlist(allyProp)) }
####
