code_resp_dat <- function(file.path,
         n.Ch = 7, #total number of chambers using during data collection
         n.m.pCh.pP = 48, ## n.mpCh.pP = measurements per chamber per pass standard number of measurements per chamber for a full pass through that chamber
         flush_cutoff = 19){ # 
  
dat <- read.csv(file.path)

#create index for each unique row in the dataset
## each row = 1 unique measurement of respiration
tot.measure <- nrow(dat)
dat$i.row <- 1:tot.measure

## n.pass = number of passes through a chamber


## determine the total number of complete passes (n.pass.tot) 
##   through all of the chambers chambers
##   that is, the total number of times data was collected from the chambers
##
## n.pass.tot = n.mpCh*n.Ch*n.cycles
## however, since n.cyles varies by experiments, its easiest to calcualte 
##   n.pass.tot from the data starting w/ the total number of measurements
##   (tot.measure); so, we want to calcualte
## n.pass.tot = tot.measure/n.m.pCh.pP
## 
## the floor() function is used to round n.pass.tot down
##  this is because the last pass on the last chabmer in the study
##  is often not complete and not used
n.pass.tot <- tot.measure/n.m.pCh.pP
n.pass.tot <- floor(n.pass.tot)


## Total number of full cycles
n.full.cycle <- floor(n.pass.tot/n.Ch)


## total number of measurements taken during complete passes
### these are "valid" measurements taht can be used in the study
valid.measure <- n.pass.tot*n.m.pCh.pP

valid.measure < tot.measure

# Code the replicate measures within a pass 
dat$pass.rep.i <- NA

dat$pass.rep.i[1:c(valid.measure)] <- 1:n.m.pCh.pP


# Which measurements were coded "NA" b/c they occurred in an incomplete pass?
i.NA.rep <- which(is.na(dat$pass.rep.i) == TRUE)

#remove NAs
dat2 <- dat[-i.NA.rep, ]

#Determine the number of full cycles through all chambers

## total number of valid measures that occured during a full pass 
## where full pass = all 48 measurements taken within in a chamber
##  (measureing not ended b/f a pass through a chamber complete)
valid.measure2 <- nrow(dat2)


## create a dataframe that layouts all combinations of
### chambers, passes, and cyles
study.layout <- expand.grid(pass.rep.i = 1:n.m.pCh.pP,
                            chamber = 1:n.Ch,
                            cycle.number = 1:n.full.cycle)

#the total size of this dataframe is n.m.pCh.pP*n.Ch*n.full.cycle
## that is, the (number of measurements per chamber per pass) * 
##              (number of chambers) *
##              (number of complete cycles)

# row index to match data
study.layout$i.row <- 1:nrow(study.layout)

# using "i.row" for matching, merge the data with the study layout
# since study.layout only has info for complete passes in complete cycles
# NA will be assigned to any measurement taken during and incomplete cycle
dat3 <- merge(dat2, 
                      study.layout,all = T)



dat3$pass.i <- sort(rep(1:n.pass.tot,n.m.pCh.pP))

dat3$measure.type <- "valid"

#If index is < 19 then code as "flush
dat3$measure.type[which(dat3$pass.rep.i < flush_cutoff)] <- "flush"
dat3$measure.type <- factor(dat3$measure.type)

return(dat3)
}