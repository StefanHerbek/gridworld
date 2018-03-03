library(R6)
library(ggplot2)
library(reshape2)
library(animation)

Racetrack <- R6Class(
  "racetrack",
  public = list(
    layout = NULL,
    state = NULL,
    controller = NULL,
    
    initialize = function(lout,cntrl) {
      self$layout = lout
      self$controller = cntrl
      self$restart()
    },
    
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
        self$restart(random_starts)
        return(-1) # out of bounds
      }
      
      if (self$layout[endpoint[1],endpoint[2]] == 0) {
        self$restart(random_starts)
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
    },
    
    lap = function(plt = F) {
      self$restart(random = 0)
      stat <- 0
      
      if (plt) {
        dev.new()
      }
      
      while (stat == 0) {
        print(self$state)
        if (plt) {
          g = self$showtrack()
          print(g)
          Sys.sleep(1.4)
        }
        stat <- self$move(explore = F, random_starts = 0)
        print(stat)
      }
    },
    
    train = function(laplim = 80, stick = 0, carrot = 0) {
      stopcond <- T
      self$controller$iter <- 0
      self$controller$converged <- FALSE
      while(stopcond) {
        self$restart(random = 1)
        result <- 0
        laptim <- 0
        while (result != 1 & laptim < laplim) {
          olds <- self$state
          result <- self$move(random_starts = 0)
          reward <- -1
          news <- self$state
          
          self$controller$update(olds, news, reward, result, stick, carrot)
          stopcond <- !self$controller$converged 
          laptim <- laptim + 1
        }
      }
    },
    
    showtrack = function() {
      blank_theme <- theme(
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position="none"
      )
      
      cols <- c("-1" = "lightgreen", "0" = "gray", 
                "1" = "white", "2" = "cyan", "4" = "yellow")
      
      grid <- self$layout
      grid[self$state[1],self$state[2]] <- 4
      
      g <- ggplot(melt(grid), aes(Var1, Var2)) + 
        geom_tile(aes(fill = as.factor(value)), color = "gray") + blank_theme +
        scale_fill_manual(values = cols)
      g
    },
    
    perpeto = function(frames = 20) {
      frame <- 1
      while(frame <= frames){
        self$lap(plt = T)
        frame <- frame + 1
      }
    }
  )
)

RandomControl <- R6Class(
  "controller",
  public = list(
    velocity = NULL,
    maxspeed = NULL,
    maxacc   = NULL,
    
    initialize = function(maxs = 2, maxa = 1) {
      self$velocity = c(0,0)
      self$maxspeed = maxs
      self$maxacc   = maxa
    }, 
    
    call = function(pos) {
      change <- sample(c(-self$maxacc,self$maxacc),2)
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
      self$velocity <- vel
      return(self$velocity)
    }
  )
)

QLearner <- R6Class(
  "q-learning controller",
  public = list(
    velocity = NULL,
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
      self$actions  = expand.grid(
        h_acc = seq(-maxa,maxa),
        v_acc = seq(-maxa,maxa)
      )
      self$maxiter = maxiter
      
      self$qtable <- cbind(
        merge(
          which(layout > 0, arr.ind = T), # 0 is wall, -1 goal
          expand.grid(seq(-maxs,maxs), seq(-maxs,maxs))
        ),
        matrix(0, nrow = 1, ncol = nrow(self$actions)) # Q init at 0
      )
      
      # stay still action should not be feasible
      row <- self$qtable$h_vel == 0 & self$qtable$v_vel == 0
      col <- 4 + which(self$actions$h_acc == 0 & self$actions$v_acc == 0)
      self$qtable[row, col] <- -Inf
      
      names(self$qtable)[1:4] <- c("h_pos","v_pos","h_vel","v_vel")
      
      # identify starting fields and add identifier to qtable
      M <- which(layout == 2, arr.ind = T) # start fields (2)
      M <- paste0(M[,"row"],"_",M[,"col"]) # doesn't look like the smartest solution
      self$qtable$start <- (paste0(self$qtable$h_pos, 
                                   "_", self$qtable$v_pos) %in% M)
      
    },
    
    call = function(position, explore = T) {
      
      # select best action (or explore)
      randnum <- runif(1)
      if (randnum <= self$eps & explore) {
        bestactions <- self$actions  
      } else {
        qvals <- self$bestQ(position)
        bestactions <- self$actions[qvals == max(qvals), ]
      }
      
      bestaction <- bestactions[sample(1:nrow(bestactions),1), ]
      bestaction <- c(bestaction[ ,1], bestaction[ ,2])
      
      self$lasta <- bestaction
      self$lastvelocity <- self$velocity # copy?
      
      vel    <- self$velocity + bestaction
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
      
      self$velocity <- vel
      return(self$velocity)
      
    },
    
    bestQ = function(position) {
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
        newq   <- unique(max(self$bestQ(newpos)))
      } else {
        newq <- unique(max(self$bestQ(newpos)))
      }
      
      oldrow <- self$qtable$h_pos == oldpos[1]
      oldrow <- oldrow & self$qtable$v_pos == oldpos[2]
      oldrow <- oldrow & self$qtable$h_vel == self$lastvelocity[1]
      oldrow <- oldrow & self$qtable$v_vel == self$lastvelocity[2]
      actionid <- which(self$actions$h_acc == self$lasta[1] & 
                          self$actions$v_acc == self$lasta[2])
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

