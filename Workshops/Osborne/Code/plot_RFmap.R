# load the data
load("../Data/MTneuron.RData")


# see what has been loaded
print(ls())

# initialize 15 x 10 matrix filled with 0s
numspks <- matrix(0, 10, 15) # [HARD CODED]
# find number of trials
nTrials <- dim(RFmap)[3]
# find max number of spikes
max_num_spks <- dim(RFmap)[4]

for (yind  in 1:10){ # [HARD CODED]
  for (xind in 1:15){ # [HARD CODED]
    # We care about how many spikes were fired at each grid position, not which
    # stimulus repeat they were fired on.  So let's count how many spikes are 
    # in the RFmap at each grid location. 
    # Note that RFmap[yind, xind, , ] is a matrix. We just count how
    # many values in the matrix are not zero, and store the value in numspks[yind,xind]
    
    numspks[yind, xind] <- sum(RFmap[yind, xind, , ] != 0)
  }
}

x <- seq(-14, 14, by = 2) # [HARD CODED]
y <- seq(-9, 9, by = 2) # [HARD CODED]

# for plotting, we want to modify the data slightly:
# first, now numspks is has y coordinates in the rows, 
# and x coordinates in the columns

# This will tilt the matrix
numspks <- t(numspks)

# The plotting routine treats the cell [1,1] as the bottom-left corner,
# while we want it to be in the upper-left corner
# This fixes the problem
numspks <- numspks[,(dim(numspks)[2]):1]

# A simple plot
print(image(x, y, numspks / nTrials, 
            main = "RF map of an MT neuron", 
            xlab = "degrees", 
            ylab = "degrees",
            col = topo.colors(27)))

# a fancier plot
# filled contour automatically smooths (linearly...) the data
print(
filled.contour(x, y, numspks / nTrials, nlevels = 25,
               plot.title = title(main = "RF map of an MT neuron",
                                  xlab = "degrees", ylab = "degrees"),
               # choose colors
               col = topo.colors(27),
               # add a point
               plot.axes={axis(1); # plot the x-axis
                 axis(2); # plot the y axis
                 points(7.5, -7.5, pch = 4, cex = 2, col = "red", font = 2)}
               )
)
