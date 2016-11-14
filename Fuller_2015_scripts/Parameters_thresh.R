# Parameters_thresh.R
# parameters for threshold simulations (no proportional harvesting)
	harvests = 1
	thresholds = seq(0,1,by=0.1)

# build dataframes
	summaries <- data.frame(
		Equil.pop = rep(NA,length=length(speeds)*length(thresholds)), 
		Equil.sd = rep(NA, length=length(speeds)*length(thresholds)), 
		speed = rep(NA,length=length(speeds)*length(thresholds)), 
		harvest = rep(NA,length=length(speeds)*length(thresholds)), 
		thresh=rep(NA,length=length(speeds)*length(thresholds)))


# index for row number
	rownumber <- matrix(seq(1:(length(thresholds)*length(speeds))),ncol=length(speeds))
#------------------------------------------------------------------------#
