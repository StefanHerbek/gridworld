# library(R6)
# library(ggplot2)
# library(reshape2)
# library(animation)

Racetrack <- R6Class(
  "Racetrack",
  inherit = Gridworld,
  public = list(
    restart = function(random = 0.2) {
      
      if (runif(1) < random) {
        M <- which(self$layout > 0, arr.ind = T) # any non wall or goal
        v1 <- sample(-self$controller$maxspeed:self$controller$maxspeed, 1)
        v2 <- sample(-self$controller$maxspeed:self$controller$maxspeed, 1)
        self$controller$velocity <- c(v1, v2)
      } else { 
        M <- which(self$layout == 2, arr.ind = T) # start fields (2)
        self$controller$velocity <- c(0,0)
      }
      
      startpos <- sample(1:nrow(M), 1)
      self$state = as.numeric(M[startpos,])
      
    },
    
    move = function(explore = T, random_starts = 0) {
      
      velocity = self$controller$call(self$state, explore)
      endpoint = self$state + velocity
      
      if (any(endpoint > dim(self$layout)) |
          any(endpoint <= 0)) {
        self$restart(random_starts) # race track restarts after crash
        return(-1) # out of bounds
      }
      
      if (self$layout[endpoint[1],endpoint[2]] == 0) {
        self$restart(random_starts) # race track restarts after crash
        return(-1) # crash
      }
      
      move_h <- seq(self$state[1], endpoint[1])
      move_v <- seq(self$state[2], endpoint[2])
      
      diff_len <- length(move_h) - length(move_v)
      
      if (diff_len > 0) {
        move_v <- c(move_v, rep(move_v[length(move_v)], diff_len))
      } else if (diff_len < 0) {
        move_h <- c(move_h, rep(move_h[length(move_h)], abs(diff_len)))
      }
      
      route <- mapply(function(h,v) {
        self$layout[h,v]
      }, move_h, move_v)
      
      if (prod(route) == 0) {
        self$restart(random_starts)
        return(-1) # crash along the way
      }
      
      self$state = endpoint
      
      if (self$layout[endpoint[1], endpoint[2]] == -1) {
        self$restart(random_starts)
        return(1) # reached goal
      } else {
        return(0) # continue
      }
    }
  )
)
