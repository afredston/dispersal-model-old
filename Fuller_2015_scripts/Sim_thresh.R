# Sim_thresh.R
# runs simulations in which there is threshold management, MPAs are not possible

# initializing the population with no pressure (no harvesting, no climate)
	init<-rep(0,w) # rows are world, columns are time
	init[which(patch==0.55)]=50
	MPA.start = rep(c(mpa.yes,mpa.no),length.out=length(world))
	
	output <- startUp(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in, Fharv=NA, Fthresh=NA, init=init, MPA.start = MPA.start)
	init.s <- output[[1]]
	MPA.start <- output[[2]]
	
for(q in 1:length(speeds)){
	for(j in 1:length(thresholds)){
		# adding harvesting
			output <- startUp(s=0,mpa.yes=mpa.yes,mpa.no=mpa.no,burn_in=burn_in,Fharv=1,Fthresh=thresholds[j], init=init.s, MPA.start = MPA.start, effort_re_allocate=effort_allocate)
		  	init.h <- output[[1]]
			MPA.start <- output[[2]]Fharv=1
		  	 
		# adding speed
			output <- longRun(s=speeds[q], mpa.yes=mpa.yes, mpa.no=mpa.no, Fthresh=thresholds[j], Fharv=1, init = init.h, MPA.start = MPA.start, generations_total=generations_total, generations_av=generations_av, effort_re_allocate=effort_allocate)
			
		# save output
			pop = output[[1]]
			pop.sd = output[[2]]
		   	summaries[rownumber[j,q],] <- c(pop, pop.sd, speeds[q], 1, ifelse(exists("thresholds"), thresholds[j],NA))
	}
}

write.csv(summaries,file = paste("Data/Thresh_",Sys.Date(),".csv",sep=""))

#------------------------------------------------------------------------#
