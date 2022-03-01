## Parse raw audl data into table of throws with possession outcome data
## Each row should include: Game id,
##                          Team id,
##                          Player id,
##                          Throw origin (x, y),
##                          Throw destination (x, y),
##                          Throw outcome {Completion, Block, etc},
##                          Possession outcome {0,1}

library(tidyverse)

setwd('C:/Users/hiros/Desktop/Storage/documents/Ultimate-Frisbee-Possession-Analysis/data/audl')

raw = read.csv('audl.csv')

## Categorize types of events

# Possession continuers
event_list_1 = c(20)

# Turnovers
event_list_2 = c(19,8,17)

# Goals
event_list_3 = c(22)

# Events that shouldn't come right after throws (error cases)
event_list_4 = c(2,3,4,44,45,5,9,6,21,18,11,13,15,31,32,
                 1,23,24,25,26,27,28)

# Callahans
event_list_5 = c(7)

## initialize list of throws. Each throw will be a tuple appended to this
throw_list = list()

## variable for tracking throw_list index of current throw
current_throw = 0

## variable for tracking throw_list index of throw at start of possession
start_throw = 0

## boolean for if between possessions (i.e. searching for first throw)
between_poss = 1

## iterate through rows of raw data table
for (i in 1:(nrow(raw) - 1)) {
#for (i in 1:200) {
  event = raw[i,]
  ## filter for throw events
  if (event$t == 20) {
    current_throw = current_throw + 1
    
    ## check if throw is first throw in the possession
    if (between_poss == 1) {
      start_throw = current_throw
    }
    
    ## initialize throw tuple
    throw = list()
    ## append data from current event
    throw = append(throw,list(event$game_id,
                              event$team_id,
                              event$r,
                              event$x,
                              event$y))
    ## variables for finding event at end of throw
    next_bool = FALSE
    next_count = 1
    
    ## append data from next event
    while (!next_bool) {
      ## retrieve next event row
      next_throw = raw[i + next_count,]
      next_event = next_throw$t
      ## match event types to cases
      if (next_event %in% event_list_1) {
        throw = append(throw,list(next_throw$x,
                                  next_throw$y,
                                  'Completion'))
        ## append throw tuple to throw list
        throw_list = append(throw_list,list(throw))
        
        between_poss = 0
        next_bool = TRUE
        
      } else if (next_event %in% event_list_2) {
        throw = append(throw,list(next_throw$x,
                                  next_throw$y,
                                  'Turnover'))
        
        ## append throw tuple to throw list
        throw_list = append(throw_list,list(throw))
        
        ## apply possession outcome to previous throws in possession
        for (j in start_throw:current_throw) {
          throw_list[j] = list(append(throw_list[[j]],0))
        }
        
        between_poss = 1
        next_bool = TRUE
        
      } else if (next_event %in% event_list_5) {
        throw = append(throw,list(next_throw$x,
                                  next_throw$y,
                                  'Callahan'))
        
        ## append throw tuple to throw list
        throw_list = append(throw_list,list(throw))
        
        ## apply possession outcome to previous throws in possession
        for (j in start_throw:current_throw) {
          throw_list[j] = list(append(throw_list[[j]],0))
        }
        
        between_poss = 1
        next_bool = TRUE
        
      } else if (next_event %in% event_list_3) {
        throw = append(throw,list(next_throw$x,
                                  next_throw$y,
                                  'Goal'))
        ## append throw tuple to throw list
        throw_list = append(throw_list,list(throw))

        ## apply possession outcome to previous throws in possession
        for (j in start_throw:current_throw) {
          throw_list[j] = list(append(throw_list[[j]],1))

        }
        
        between_poss = 1
        next_bool = TRUE
      } else if (next_event %in% event_list_4) {
        throw = append(throw,list(NA,
                                  NA,
                                  'Unknown'))
        ## append throw tuple to throw list
        throw_list = append(throw_list,list(throw))
        
        ## apply possession outcome to previous throws in possession
        for (j in start_throw:current_throw) {
          throw_list[j] = list(append(throw_list[[j]],NA))
        }
        
        between_poss = 1
        next_bool = TRUE
      }
      
      next_count = next_count + 1
    }
    
    
    
    
  }
}


df = as.data.frame(do.call(rbind,throw_list[]))
colnames(df) = c('Game_id','Team_id','Player_id',
                'x_0','y_0','x_1','y_1',
                'Throw_outcome','Possession_outcome')

df <- apply(df,2,as.character)

write.csv(df,"audl_possession_outcomes.csv",row.names = FALSE)
