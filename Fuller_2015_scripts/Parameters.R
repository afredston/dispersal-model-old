# Parameters.R
#######################################
## Parameters & Building Structures ##
######################################

step_size=0.01 #distance between points in space
b=.5 #parameter for Laplace dispersal kernel
R0=5 #growth parameter for recruitment
K=100 #carrying capacity parameter for juvenile density dependence
threshold = 0.001 #difference between generation populations. 
burn_in = 2000 # number of generations to run simulations before checking for equilibrium conditions
speeds = seq(0,.5,by=0.02)
harvests = seq(0,.2,by=0.01)
f_ind = 1 #per capita reproductive rate
generations_total = 8000
generations_av = 2000

patch = seq(0,1,by=step_size)
world = seq(-.51,4.5, by = step_size)   # to run the MPA versions, world has to be at least 400 steps (max distance between MPAs in "cons" run)
w = length(world)

cons.yes = rep(1,4*b/step_size)
cons.no = rep(0,8*b/step_size)
fish.yes = rep(1,floor((1/3*b)/step_size))  # had to round because not complete step size. Rounded down. 
fish.no = rep(0,floor((2/3*b)/step_size))

null.yes = rep(0,length(world))
null.no = rep(0, length(world))

move_window = 100

#------------------------------------------------------------------------#