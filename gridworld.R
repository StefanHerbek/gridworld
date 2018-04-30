library(R6)
library(ggplot2)
library(reshape2)
library(animation) # for animations

Gridworld <- R6Class(
  "gridworld",
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
        v1 <- sample(-1:1, 1)
        v2 <- sample(-1:1, 1)
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
        return(-1) # out of bounds
      }
      
      if (self$layout[endpoint[1],endpoint[2]] == 0) {
        return(-1) # crash
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
          g = self$showgrid()
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
    
    showgrid = function() {
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
