# function to calculate cos sim score
getCosSim = function(x,y){
  c = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(c) }

# loop over matrix of positions and calculate cos sim for every pair
cosSimMat = function(x) {
  # org output object
  m = matrix(NA,
    nrow=ncol(x),ncol=ncol(x),
    dimnames=list(colnames(x),colnames(x)))
  cos = as.data.frame(m)

  # loop through pairs and store in cos
  for(i in 1:ncol(x)) {
    for(j in i:ncol(x)) {
      co_rate_1 = x[which(x[,i] & x[,j]),i]
      co_rate_2 = x[which(x[,i] & x[,j]),j]
      cos[i,j]= getCosSim(co_rate_1,co_rate_2)
      cos[j,i]=cos[i,j] } }

  # return cos
  return(cos) }
