RandomWalker <- R6Class(
  "walker",
  public = list(
    velocity = NULL,
    
    initialize = function() {
      self$velocity = c(0,0)
    }, 
    
    call = function(pos) {
      self$velocity <- sample(c(-1,0,1),2)
      return(self$velocity)
    }
  )
)

QWalker <- R6Class(
  "q-learning controller",
  inherit = RandomWalker,
  public = list(
    actions = NULL,
    eps = NULL,
    gamma = NULL,
    alpha = NULL,
    iter = 0,
    maxiter = NULL,
    converged = FALSE,
    lasta = c(0,0),
    qtable = NULL,
    
    initialize = function(layout,
                          eps = 0.05, gamma = 1, alpha = 0.05, 
                          maxiter = 200) {
      self$velocity = c(0,0)
      self$maxspeed = maxs
      self$eps = eps
      self$gamma = gamma
      self$alpha = alpha
      self$maxiter = maxiter
      self$actions  = self$gen_actions()
      self$qtable = self$gen_qtable(layout)
    },
    
    gen_actions = function() {
      expand.grid(
        h_act = seq(-1,1),
        v_act = seq(-1,1)
      )
    },
    
    gen_qtable = function(layout) {
      qtable <- cbind(
        which(layout > 0, arr.ind = T),
        matrix(0, nrow = 1, ncol = nrow(self$actions)) # Q init at 0
      )
      
      names(qtable)[1:2] <- c("h_pos","v_pos")
      
      # stay still action should not be feasible
      col <- 2 + which(self$actions$h_act == 0 & self$actions$v_act == 0)
      qtable[, col] <- -Inf
      
      # identify starting fields and add identifier to qtable
      M <- which(layout == 2, arr.ind = T) # start fields (2)
      M <- paste0(M[,"row"],"_",M[,"col"]) # doesn't look like the smartest solution
      qtable$start <- (paste0(qtable$h_pos, "_", qtable$v_pos) %in% M)
      
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
      
      bestaction <- bestactions[sample(1:nrow(bestactions),1), ]
      bestaction <- c(bestaction[ ,1], bestaction[ ,2])
      
      self$lasta <- c(bestaction[ ,1], bestaction[ ,2])
      
      self$velocity <- c(bestaction[ ,1], bestaction[ ,2])
      return(self$velocity)
      
    },
    
    thisQ = function(position) {
      # simple query of the q table - could do without?
      row <- self$qtable$h_pos == position[1]
      row <- row & self$qtable$v_pos == position[2]
      nvcols <- c("h_pos","v_pos","start") # non-value columns
      return(as.numeric(self$qtable[row, !colnames(self$qtable) %in% nvcols]))
    },
    
    update = function(oldpos, newpos, reward, result, stick = 0, carrot = 0) {
      self$iter <- self$iter + 1
      
      if (result == 1) {
        newq <- carrot # final state has action values of carrot everywhere
        print("*")
      } else if (result == -1) {
        reward <- reward + stick
        newq   <- unique(max(self$thistQ(newpos)))
      } else {
        newq <- unique(max(self$thisQ(newpos)))
      }
      
      oldrow <- self$qtable$h_pos == oldpos[1]
      oldrow <- oldrow & self$qtable$v_pos == oldpos[2]
      actionid <- which(self$actions$h_act == self$lasta[1] & 
                          self$actions$v_act == self$lasta[2])
      oldq <- self$qtable[oldrow, 2 + actionid]
      
      if (oldq == -Inf) {
        update <- -Inf # should not be updated
      } else {
        update <- oldq + self$alpha*(reward + self$gamma*newq - oldq)
      }
      
      self$qtable[oldrow, 2 + actionid] <- update
      
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
