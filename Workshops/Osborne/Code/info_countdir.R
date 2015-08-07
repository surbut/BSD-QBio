# =============================================
# Copied from plot_tuncurve.R
# =============================================
# load the data
load("../Data/MTneuron.RData")

# Number of directions
directions <- as.vector(directions)
nDirs <- length(directions)

# For each direction, count number of replicates
# the command rep repeats a certain value a number of times, and creates a vectors
nReps <- rep(0, nDirs)
for (n in 1:nDirs){
  nReps[n] <- sum(theta == n)
}

# create array data that uses a vector of 1s and 0s instead of spike times
# What is the maximum time?
maxTime <- round(max(dirtune))

mydata <- array(0, c(maxTime, nDirs, max(nReps))) 
# Let's fill the array with ones and zeros
# For each direction
for (n in 1:nDirs){
  # which are the corresponding thetas?
  index <- which(theta == n)
  # For each trial
  for (i in 1:length(index)){
    spks <- round(dirtune[index[i], ]) # make spike times integers
    spks <- spks[spks > 0] # take only those > 0
    mydata[spks, n, i] <- 1 # set to 1 in the array mydata
  }
}
# =============================================
# END Copied from plot_tuncurve.R
# =============================================

# create a Poisson shuffled version of the data for the optional part of the
# exercise
data_shuffle <- array(0, dim(mydata))

for (dind in 1:nDirs){
  for (tind in 1:dim(mydata)[1]){
   index <-  sample(1:nReps[dind]) # shuffle the indices from 1 to nReps[dind]
   data_shuffle[tind, dind, 1:nReps[dind]] <- mydata[tind, dind, index]
  }
}


# use t= 1 to 350 for neuron 1 so we don't consider the spiking after the stimulus is
# over
# neuron 2 has 256 time points
T <- 1:min(dim(mydata)[1], 350) # [WHY THE MIN?]
cumcounts <- cumsum(mydata[T, , ])
cumcounts_shuffle=cumsum(data_shuffle(T,:,:),1);
maxcount = max(max(max(cumcounts)));

countbins = [0:4:maxcount]; %26 bins for neuron 1
% how many bins can we use?  could we use 0:maxcount (as many bins as the
% count)?  it depends on how many repetitions we have and how variable the
% response is.  If you have time, you can try different binning and see how
% it changes your results.
ncountbins = length(countbins);

probcumcounts = zeros(length(T),nDirs,ncountbins);
probcumcounts_shuffle = probcumcounts;
for n=1:length(T);
for m=1:nDirs;
probcumcounts(n,m,:) = hist(squeeze(cumcounts(n,m,1:nReps(m))),[countbins])/nReps(m);
probcumcounts_shuffle(n,m,:) = hist(squeeze(cumcounts_shuffle(n,m,1:nReps(m))),[countbins])/nReps(m);
end
end;

pdir = zeros(nDirs,1); %the probability of each direction is not uniform
for n=1:nDirs
pdir(n) = nReps(n)/sum(nReps);
end


% a basic algorithm without finite sample size correction to compute the 
% mutual information between the cumulative spike count and
% motion direction over time

Icount_dir = zeros(length(T),1);
I = Icount_dir;
I_shuffle = I;
Pcount_given_dir = zeros(ncountbins,nDirs);

for tind = 1:length(T)     
P = reshape(probcumcounts(tind,:,:),nDirs,ncountbins);
P = P./sum(sum(P));
Icount_dir(tind) = sum(sum(P.*log2(P./(sum(P,2)*sum(P,1)+eps) +eps)));

P_shuff = reshape(probcumcounts_shuffle(tind,:,:),nDirs,ncountbins);
P_shuff = P_shuff./sum(sum(P_shuff));
I_shuffle(tind) = sum(sum(P_shuff.*log2(P_shuff./(sum(P_shuff,2)*sum(P_shuff,1)+eps) +eps)));

%or less compactly
Pcount = sum(P,1)';
Pcount = Pcount/sum(Pcount);
Scount(tind) = -sum(Pcount.*log2(Pcount+eps));
Pdir = sum(P,2)';
Pdir = Pdir/sum(Pdir);
for dind = 1:nDirs
Pcount_given_dir(:,dind) = P(dind,:)'./(sum(P(dind,:)')+eps);
                                            Scount_given_dir(tind,dind)= -sum(Pcount_given_dir(:,dind).*log2(Pcount_given_dir(:,dind)+eps));
                                            I(tind) = I(tind) + ...
                                            Pdir(dind)*sum(Pcount_given_dir(:,dind).*log2(Pcount_given_dir(:,dind)./(Pcount +eps) + eps));
                                            end
                                            
                                            end;
                                            
                                            %Make a figure
                                            
                                            figure;
                                            set(gca,'FontSize',14);
                                            h = plot(T,I,'k',T,I_shuffle,'r');
                                            legend(h,'neuron','Poisson model');
                                            xlabel('time from motion onset (ms)');
                                            ylabel('information (bits)');
                                            title('Mutual information between count and direction');
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            