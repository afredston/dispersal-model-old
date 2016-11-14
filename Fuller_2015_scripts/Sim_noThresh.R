# Sim_noThresh.R
# runs simulations in which there is no threshold management, MPAs are possible

# set MPAs
if(MPA=="cons") {mpa.yes=cons.yes; mpa.no=cons.no} else {
	if(MPA=="fish") {mpa.yes=fish.yes; mpa.no=fish.no} else {
		if(MPA=="null") {mpa.yes=null.yes; mpa.no=null.no} else{
			if(exists("MPA")) warning(paste("MPA needs to be 'cons', 'fish', or 'null'.",sep=""))
		}
	}
}

# initializing the population with no pressure (no harvesting, no climate)
	init<-rep(0,w) # rows are world, columns are time
	init[which(patch==0.55)]=50
	MPA.start = rep(c(mpa.yes,mpa.no),length.out=length(world))
	
	output <- startUp(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in, Fharv=NA, Fthresh=NA, init=init, MPA.start = MPA.start, effort_re_allocate=NA)
	init.s <- output[[1]]
	MPA.start <- output[[2]]
	
for(q in 1:length(speeds)){
	for(j in 1:length(harvests)){
		# adding harvesting
			output <- startUp(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in,Fharv=harvests[j],Fthresh=NA, init=init.s, MPA.start = MPA.start, effort_re_allocate=effort_allocate)
		  	init.h <- output[[1]]
			MPA.start <- output[[2]]
		  	 
		# adding speed
			output <- longRun(s=speeds[q], mpa.yes=mpa.yes, mpa.no=mpa.no, Fthresh=NA, Fharv=harvests[j], init = init.h, MPA.start = MPA.start, generations_total=generations_total, generations_av=generations_av, effort_re_allocate=effort_allocate)
			
		# save output
			pop = output[[1]]
			pop.sd = output[[2]]
		   	summaries[rownumber[j,q],] <- c(pop, pop.sd, speeds[q], harvests[j], ifelse(exists("Fthresh"), Fthresh,NA))
	}
}	


write.csv(summaries,file = paste("Data/MPA",MPA,"_",effort_allocate,"_",Sys.Date(),".csv",sep=""))
#------------------------------------------------------------------------#
