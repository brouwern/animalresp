dat <- read.csv("./data-raw/23-apr-orig.csv",skip = 1, sep = ",")
names(dat)


nrow(dat)/50

#make subset of focal columns
dat <- dat[, c("time","O2","channel")]

#number of chambers in run
n.chambers <- 8
n.cycle <- 4

#vector of chamber IDs
chamber.vector <- 1:n.chambers

cycle.vector <- 1:n.cycle

#create empty column to hold cumulative chamber index
dat$i.chamber <- NA

dat$i.within <- NA

summary(factor(dat$channel))

# for loop
a.function <- function(){
  for(i in 1:length(chamber.vector)){
    
    #working chamber number
    channel.current <- chamber.vector[i]
    print(channel.current)
    
    #row indices of working chamber number
    channel.i <- which(dat$channel == channel.current)
    
    #cumulative index for all measurements 
    ## from a single chamber
    dat$i.chamber[channel.i] <- 1:length(channel.i)
    
    
    i.full.cycles <- which(dat$i.chamber <= n.cycle*48  & dat$channel == channel.current )
    
    dat$i.within[i.full.cycles]  <- seq(1:48)
  }
  
  return(dat)
}

debugonce(a.function)

dat <- a.function()

#column to hold info on which phase of cycle
dat$phase <- "mouse"
dat$phase[which(dat$i.within < 19)] <- "flush"

library(ggplot2)
ggplot(data = dat,
       aes(y = O2, x = i.chamber, 
           shape  = phase,color = factor(phase))) +
  geom_point() +
  facet_wrap(~channel)



library(reshape2)


dcast(data = dat,
      formula = channel ~ phase,
      value.var = "O2",
      fun.aggregate = min,
      na.rm = T)

library(doBy)

summaryBy