RandomRacer <- R6Class(
  "controller",
  inherit = RandomWalker,
  public = list(
    maxspeed = NULL,
    maxacc   = NULL,
    
    initialize = function(maxs = 2, maxa = 1) {
      self$velocity = c(0,0)
      self$maxspeed = maxs
      self$maxacc   = maxa
    }, 
    
    call = function(pos, explore) {
      change <- sample(c(-self$maxacc,self$maxacc),2)
      self$velocity <- self$accelerate(change)
      return(self$velocity)
    },
    
    accelerate = function(change) {
      vel    <- self$velocity + change
      if (vel[1] > self$maxspeed) {
        vel[1] <- self$maxspeed
      } else if (vel[1] < -self$maxspeed) {
        vel[1] <- -self$maxspeed
      }
      
      if (vel[2] > self$maxspeed) {
        vel[2] <- self$maxspeed
      } else if (vel[2] < -self$maxspeed) {
        vel[2] <- -self$maxspeed
      }
      return(vel)
    }
  )
)

QRacer <- R6Class(
  "q-learning race controller",
  inherit = RandomRacer,
  public = list(
    actions = NULL,
    eps     = NULL,
    gamma = NULL,
    alpha = NULL,
    maxspeed = NULL,
    iter = 0,
    maxiter = NULL,
    converged = FALSE,
    lasta = c(0,0),
    lastvelocity = c(0,0),
    qtable = NULL,
    
    initialize = function(layout, maxa = 1, maxs = 2,
                          eps = 0.05, gamma = 1, alpha = 0.05, 
                          maxiter = 200) {
      self$velocity = c(0,0)
      self$maxspeed = maxs
      self$eps = eps
      self$gamma = gamma
      self$alpha = alpha
      self$maxiter = maxiter
      self$actions  = self$gen_actions(maxa)
      self$qtable <- self$gen_qtable(layout)
    },
    
    gen_actions = function(maxa) {
      expand.grid(
        h_act = seq(-maxa,maxa),
        v_act = seq(-maxa,maxa)
      )
    },
    
    gen_qtable = function(layout) {
      maxs <- self$maxspeed
      qtable <- cbind(
        merge(
          which(layout > 0, arr.ind = T), # 0 is wall, -1 goal
          expand.grid(seq(-maxs,maxs), seq(-maxs,maxs))
        ),
        matrix(0, nrow = 1, ncol = nrow(self$actions)) # Q init at 0
      )
      
      names(qtable)[1:4] <- c("h_pos","v_pos","h_vel","v_vel")
      
      # stay still action should not be feasible
      row <- qtable$h_vel == 0 & qtable$v_vel == 0
      col <- 4 + which(self$actions$h_act == 0 & self$actions$v_act == 0)
      qtable[row, col] <- -Inf
      
      # identify starting fields and add identifier to qtable
      M <- which(layout == 2, arr.ind = T) # start fields (2)
      M <- paste0(M[,"row"],"_",M[,"col"]) # doesn't look like the smartest solution
      qtable$start <- (paste0(qtable$h_pos, 
                              "_", qtable$v_pos) %in% M)
      
      return(qtable)
    },
    
    call = function(position, explore = T) {
      
      # select best action (or explore)
      randnum <- runif(1)
      if (randnum <= self$eps & explore) {
        bestactions <- self$actions  
      } else {
        qvals <- self$thisQ(position)
        bestactions <- self$actions[qvals == max(qvals), ]
      }
      
      bestaction <- bestactions[sample(1:nrow(bestactions),1), ,drop = F]
      bestaction <- c(bestaction[ ,1], bestaction[ ,2])
      
      self$lasta <- bestaction
      self$lastvelocity <- self$velocity 
      
      self$velocity <- self$accelerate(bestaction)
      return(self$velocity)
      
    },
    
    thisQ = function(position) {
      # simple query of the q table - could do without?
      row <- self$qtable$h_pos == position[1]
      row <- row & self$qtable$v_pos == position[2]
      row <- row & self$qtable$h_vel == self$velocity[1]
      row <- row & self$qtable$v_vel == self$velocity[2]
      nvcols <- c("h_pos","v_pos","h_vel","v_vel","start") # non-value columns
      return(as.numeric(self$qtable[row, !colnames(self$qtable) %in% nvcols]))
    },
    
    update = function(oldpos, newpos, reward, result, stick = 0, carrot = 0) {
      self$iter <- self$iter + 1
      
      if (result == 1) {
        newq <- carrot # final state has action values of carrot everywhere
        print("*")
      } else if (result == -1) {
        reward <- reward + stick
        # crash should result in return to start (label 2)
        # but due to random restarts actual newpos might be a different field
        # hence here choose a random position from the start line
        newpos <- self$qtable[self$qtable$start == T, ]
        newpos <- newpos[sample(1:nrow(newpos),1), ]
        newpos <- c(newpos$h_pos, newpos$v_pos)
        newq   <- unique(max(self$thisQ(newpos)))
      } else {
        newq <- unique(max(self$thisQ(newpos)))
      }
      
      oldrow <- self$qtable$h_pos == oldpos[1]
      oldrow <- oldrow & self$qtable$v_pos == oldpos[2]
      oldrow <- oldrow & self$qtable$h_vel == self$lastvelocity[1]
      oldrow <- oldrow & self$qtable$v_vel == self$lastvelocity[2]
      actionid <- which(self$actions$h_act == self$lasta[1] & 
                          self$actions$v_act == self$lasta[2])
      oldq <- self$qtable[oldrow, 4 + actionid]
      
      if (oldq == -Inf) {
        update <- -Inf # should not be updated
      } else {
        update <- oldq + self$alpha*(reward + self$gamma*newq - oldq)
      }
      
      self$qtable[oldrow, 4 + actionid] <- update
      
      if (self$iter >= self$maxiter) {
        self$converged <- TRUE
      }
      
      if (self$iter %% 100 == 0) {
        print(result)
      }
      
      return(1)
    }
  )
)
