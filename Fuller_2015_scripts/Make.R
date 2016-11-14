# Make.R --> Wrapper script to run simulations
rm(list=ls())
require(plyr)
require(lattice)

# load parameters, functions
	source("Parameters.R")
	source("Functions.R")
	
# set up different parameter combinations for all simulations

sims <- data.frame(model = c("noThresh","noThresh","noThresh","noThresh","noThresh","Thresh"),
					MPA = c("cons","cons","fish","fish","null","null"),
					effort_allocate = c(NA, "yes", NA, "yes", NA, NA), stringsAsFactors=FALSE)
					
for(run in 1:nrow(sims)){
# run analysis
	# choose threshold or no threshold
	model = sims$model[run]	# "noThresh"; "Thresh"

	# if no threshold, choose MPA: "null", "cons", "fish"
	MPA = sims$MPA[run]
		# choose how effort should be allocated if MPAs present. 
		effort_allocate = sims$effort_allocate[run]
		effort_allocate = ifelse(MPA!="null",effort_allocate,NA)

# analysis
timed <- system.time(
	if(model=="noThresh") {sapply(c("Parameters_nothresh.R","Sim_noThresh.R"),source,.GlobalEnv)} else {
		if(model=="Thresh" & MPA == "null") {sapply(c("Parameters_thresh.R","Sim_thresh.R"),source,.GlobalEnv)} else {
			warning("model needs to be 'noThresh' or 'Thresh', MPA needs to be 'null'")
			}
		}
	)
		
cat(paste("Time elapsed: ",round(timed[1]/3600,3)," hours\n","Finished running a ", model, " simulation with ", MPA, " MPAs", " and effort re_allocate set to ", effort_allocate,".\n",nrow(sims)-run, " simulations left to go...",sep=""))
}
#------------------------------------------------------------------------#