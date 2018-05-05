code_flushes <- function(dat,
                         time_col = "time.min",
                         variable_col = "Q.S102.O2.Pcor",
                         channel_col = "channel",
                         n_chambers = 8,
                         n_cycles = 4,
                         measures_per_cycle = 48,
                         flush_cutoff = 19){
 
  #vector of chamber IDs
  chamber.vector <- 1:n_chambers
  
  #vector of cycle IDs
  cycle.vector <- 1:n_cycles
  
  #create empty column to hold cumulative chamber index
  ## eg, the 1st time a column is measured = 1
  ##     and each subsequent time recieves unique number
  ##     eg 2, 3, ... n, where n is the total number of
  ##     observations for the chamber
  
  dat$chamb.measure.i <- NA
  
  ## Create empty colum to hold indes for each measurement 
  ##  in a given chambe within each cycle

  dat$chamb.within.cycle.i <- NA
  
  
  # Loop over data to code indices
    for(i in 1:length(chamber.vector)){
      
      #working chamber number
      channel.current <- chamber.vector[i]
      cat("\n",channel.current)
      
      #row indices of working chamber number
      channel.i <- which(dat$channel == channel.current)
      
      #cumulative index for all measurements 
      ## from a single chamber
      dat$chamb.measure.i[channel.i] <- 1:length(channel.i)
      
      
      i.full.cycles <- which(dat$chamb.measure.i <= n_cycles*measures_per_cycle  & 
                             dat$channel         == channel.current )
      
      dat$chamb.within.cycle.i[i.full.cycles]  <- seq(1:measures_per_cycle)
    }
    
    
  

  
  # Code which phase
  dat$phase <- "mouse"
  
  #If index is < 19 then code as "flush
  dat$phase[which(dat$chamb.within.cycle.i < flush_cutoff)] <- "flush"
  
  cat("\nData coded as `mouse` or `flush` in `phase` column")
  
  return(dat)
}


