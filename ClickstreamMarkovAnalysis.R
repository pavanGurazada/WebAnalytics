library(clickstream)

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
# We illustrate this with a website that has 20 pages

numWebPages <- 20

# Build the transition matrix for the web pages
# In real-life, this will be gleaned from data. What proportion of users move
# between each pair of web pages

tm <- matrix(nrow = numWebPages, ncol = numWebPages)

for (i in 1:numWebPages) {
  x <- runif(numWebPages)
  tm[, i] <- x/sum(x)
}

# Generate a starting probability vector for the pages
# In real-life this can be obtained from basic probabilities
# What proportion of users land on home page directly?
# What proportion of users land on the other pages (probably from search engines)?

sp <- rnorm(n = numWebPages, mean = 0.4, sd = 0.01)
sp <- sp/sum(sp)

randomCls <- randomClickstreams(states = paste0("P", 1:numWebPages),
                                startProbabilities = sp,
                                transitionMatrix = tm,
                                meanLength = 10,
                                n = 100)

summary(randomCls)
writeClickstreams(randomCls, "example.csv", header = TRUE, sep = ",")

mc <- fitMarkovChain(clickstreamList = randomCls,
                     order = 2,
                     control = list(optimizer = "quadratic"))
summary(mc)

