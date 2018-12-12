library(clickstream)
library(tidyverse)

set.seed(201300810)

# The notes below are snippets from vignettes on the 'clickstream' R package

# A clickstream is a sequence of click events for exactly one session with an 
# online store user. For example:
# Session 1: P1 P2 P1 P3 P4 Defer
# Session 2: P3 P4 P1 P3 Defer
# Session 3: P5 P1 P6 P7 P6 P7 P8 P7 Buy
# Session 4: P9 P2 P11 P12 P11 P13 P11 Buy
# Session 5: P4 P6 P11 P6 P1 P3 Defer
# Session 6: P3 P13 P12 P4 P12 P1 P4 P1 P3 Defer
# Session 7: P10 P5 P10 P8 P8 P5 P1 P7 Buy
# Session 8: P9 P2 P1 P9 P3 P1 Defer
# Session 9: P5 P8 P5 P7 P4 P1 P6 P4 Defer
# 
# This gets encoded as follows

cls <- list(Session1 = c("P1", "P2", "P1", "P3", "P4", "Defer"),
            Session2 = c("P3", "P4", "P1", "P3", "Defer"),
            Session3 = c("P5", "P1", "P6", "P7", "P6", "P7", "P8", "P7", "Buy"),
            Session4 = c("P9", "P2", "P11", "P12", "P11", "P13", "P11", "Buy"),
            Session5 = c("P4", "P6", "P11", "P6", "P1", "P3", "Defer"),
            Session6 = c("P3", "P13", "P12", "P4", "P12", "P1", "P4", "P1", "P3", "Defer"),
            Session7 = c("P10", "P5", "P10", "P8", "P8", "P5", "P1", "P7", "Buy"),
            Session8 = c("P9", "P2", "P1", "P9", "P3", "P1", "Defer"),
            Session9 = c("P5", "P8", "P5", "P7", "P4", "P1", "P6", "P4", "Defer"))

class(cls) <- "Clickstreams"

class(cls); typeof(cls)

# Interestingly, clickstreams can be generated for practise using a custom 
# function in the clickstream package
# We illustrate this with a website that has 7 pages
# This example is based on the one given in the package

num_user_sessions <- 1e5
num_web_pages <- 7
avg_pages_browsed <- 50 # How many pages (including revisits) does a typical user 
                        # browse before taking a purchase decision

# Generate a starting probability vector for the pages
# In real-life this can be obtained from basic probabilities
# What proportion of users land on home page directly?
# What proportion of users land on the other pages (probably from search engines)?

# The following formulation for the starting probabilities is unrealistic
# We are using here the probabilities from the package 

# sp <- rnorm(n = num_web_pages, mean = 0.4, sd = 0.01)
# sp <- sp/sum(sp)
# sp <- c(sp, 0, 0)

sp <- c(0.2, 0.25, 0.1, 0.15, 0.1, 0.1, 0.1, 0, 0) # The absorbing states have 0 starting probability

# Build the transition matrix for the web pages
# In real-life, this will be gleaned from data. What proportion of users move
# between each pair of web pages and how many move to a decision from each
# web page

tm <- matrix(nrow = num_web_pages + 2, 
             ncol = num_web_pages + 2) # 2 is added for the absorbing states

for (i in 1:num_web_pages) {
  x <- runif(num_web_pages + 2)
  tm[, i] <- x/sum(x)
}

tm[, num_web_pages + 1] <- rep(0, num_web_pages + 2)
tm[, num_web_pages + 2] <- rep(0, num_web_pages + 2)

random_cls <- randomClickstreams(states = c(paste0("P", 1:num_web_pages), "Defer", "Buy"),
                                 startProbabilities = sp,
                                 transitionMatrix = tm,
                                 meanLength = avg_pages_browsed,
                                 n = num_user_sessions)

summary(random_cls)

# writeClickstreams(random_cls, "example.csv", header = TRUE, sep = ",")

# Lets fit multiple markov chains, i.e., of different orders and check which 
# fits the best
# This is wasteful since we are using up all the data, but good to see what happens
# with the underlying mechanics

# The maximal order maxOrder is the minimal length more than 50% of the 
# clickstreams have. Lets calculate this for this example 

stream_length <- rep(NA, num_user_sessions)

# Remember that the clickstream object is a list type

for (session in 1:num_user_sessions) {
  stream_length[session] <- length(random_cls[[session]])
}

max_order <- median(stream_length)

result <- data.frame()

for (k in 1:2) {
  mc <- fitMarkovChain(clickstreamList = random_cls,
                       order = k)
  result <- rbind(result, c(k, summary(mc)$aic, summary(mc)$bic))
}



clusters <- clusterClickstreams(random_cls, order = 2, centers = 3)

