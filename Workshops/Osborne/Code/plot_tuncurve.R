# load the data
load("../Data/MTneuron.RData")

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

# For each direction, count number of replicates
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- sum(theta == n)
}

# create array data that uses a vector of 1s and 0s instead of spike times
# this isn't strictly necessary, but it will come in handy 
mydata <- array(0, c(550, 24, 46)) # [HARD-CODED]
for (n in 1:nDirs){
  # which are the corresponding thetas?
  index <- which(theta == n)
  for (i in 1:length(index)){
    spks <- round(diturne[index[i], ]) # make integers
    spks <- spks[spks > 0] # take only those > 0
    mydata[spks, n, i] <- 1 # set to 1 in the array mydata
  }
}

# You can just count all the spikes in the array and plot the direction
# tuning of the total count, but then you will be conflating contributions
# from stimulus driven and non-stimulus driven activity.  One way around
# this is to estimate the duration of the neural response and to reject 
# spikes coming earlier or later.

toplot <- 1000 * rowMeans(mydata[, 12, 1:nReps[12]])
print(
plot(toplot, 
     type = "l", # plot lines
     xlab = 'time from motion onset (ms)', # x label
     ylab = 'Average firing rate (spikes/s)',
     main = paste('PSTH of MT neuron response to theta = ', directions[12], 'degrees')
     )
)
# Spikes between 50ms and 350ms seem most likely to be motion driven

counts <- array(0, c(nDirs, max(nReps)))
mcounts <- rep(0, nDirs)

for (n in 1:nDirs){
  index <- which(theta == n)
  for (i in 1:length(index)){
    spks <- which(round(diturne[index[i], ]) > 0) # WHY DO WE NEED ROUND [??? ASK LESLIE]
    spks <- spks[spks > 45 & spks < 355]
    counts[n, i] <- length(spks)
  }
  mcounts[n] <- mean(counts[n,1:length(index)])
}


#Plot the tuning curve
mrates <- mcounts * (1000 / length(45:355))
print(
plot(directions, # x-axis
     mrates, #y-axis
     xlab = 'degrees',
     ylab = 'Avg. rate (spikes/s)',
     ylim = c(0, 170)
     )
)

# now fit a normal distribution
# first, go from frequencies to a list of values
myvals <- rep(directions, mrates)
# the maximum likelihood estimates are
# mu <- sample mean 
# sigma^2 <- unadjusted sample variance

mu <- mean(myvals)
sigma <- sqrt(mean((myvals - mu)^2))

# now add a curve to the plot
curve(dnorm(x, mu, sigma) * length(myvals) * 15, # 15 is the "bin size"
      directions, 
      add = TRUE)


# To see if tuning changes over time, create a density plot of data,
# averaging over the number of trial repeats in the 3rd dimension (i.e.
# nReps).
mdata <- array(0, dim(mydata)[1:2])
for (n in 1:dim(mdata)[2]){
  mdata[, n] <- rowMeans(mydata[, n, 1:(nReps[n])])
}

print(
filled.contour(seq(-180, 165, by = 15), # x-axis
               1:550, # y-axis
               (t(mdata)[,550:1]) * 1000, # transpose and invert y so that time starts at the bottom of the plot
               nlevels = 30,
               plot.title = title(main = 'PSTH vs direction for an MT neuron',
                                  xlab = 'direction (degrees)', 
                                  ylab = 'time since motion onset (ms)'),
               # choose colors
               col = topo.colors(31)
)
)
