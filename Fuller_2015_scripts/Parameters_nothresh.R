# Parameters_nothresh.R
# parameters for no-threshold simulations (just proportional harvesting)

# build dataframes
	summaries <- data.frame(
		Equil.pop = rep(NA,length=length(speeds)*length(harvests)),
		Equil.sd = rep(NA,length=length(speeds)*length(harvests)), 
		speed = rep(NA,length=length(speeds)*length(harvests)), 
		harvest = rep(NA,length=length(speeds)*length(harvests)), 
		thresh = rep(NA,length=length(speeds)*length(harvests))
		)


# index for row number
	rownumber <- matrix(seq(1:(length(harvests)*length(speeds))),ncol=length(speeds))
	
#------------------------------------------------------------------------#