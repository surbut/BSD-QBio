# load the data
load("../Data/MTneuron.RData")


# Make a new array that instead of spike times has a vector of 
# length 550 (the duration of the trial in ms) withs 1s where the 
# spikes occur (rounded to the nearest ms)

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

mydata <- array(0, c(550, 24, 46)) # start with an array of 0s, then we will put 1s where the spikes are
for (n in 1:length(directions)){ # 24 directions
  # grab all the repeats for a given direction from the array theta
  index <- which(theta == n)
  for (i in 1:length(index)){ # loop through each trial
    spks <- round(diturne[index[i], ]) # get the spikes from that trial and round to integer values
    spks <- spks[spks > 0] # just pick out the positions of the spike from all the 0s
    mydata[spks, n, i] <- 1 # set those elements in data to 1
  }
}

rm(spks, i, n, index) # clean up what we don't need in the workspace

# For each direction, count number of replicates
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- sum(theta == n)
}

PSTH <- matrix(0, 550, 24)
for (n in 1:length(directions)){
  PSTH[ ,n] <- rowMeans(mydata[, n, 1:(nReps[n])]) # it is important to average just over the actual trials
}         

# Note that PSTH has units of spikes/per 1ms time bins.  To get spikes/s
# multiply by 1000 ms/s
print(
filled.contour(seq(-180, 165, by = 15), # its nice to have real units like degrees!
                                        # these numbers are in the vector called directions 
                                        # in your workspace
               1:550, # y-axis
               (t(PSTH)[,550:1]) * 1000, # transpose and invert y 
                                          # so that time starts at the bottom of the plot
               nlevels = 30, # number of different colors to use
               plot.title = title(main = 'PSTH vs direction for an MT neuron',
                                  xlab = 'direction (degrees)', 
                                  ylab = 'time since motion onset (ms)'),
               # choose colors
               col = topo.colors(31)
)
)
