#create a function for Situation B to be able to replicate 15,000 times
SituationB <- function(alpha){

  #create two samples of the same population to perform a two sample t-test 
  Group1 <- rnorm(50,0,1)
  Group2 <- rnorm(50,0,1)
  
  #perform a two sample t-test for first 20 observations
  t.test1 <- t.test(Group1[1:20], Group2[1:20], var.equal=TRUE)
  #add 10 observations to the sample (1:30) and perform t-test
  t.test2 <- t.test(Group2[1:30], Group2[1:30], var.equal=TRUE)
  #add 10 more observations to sample(1:40) and perform t-test
  t.test3 <- t.test(Group2[1:40], Group2[1:40], var.equal=TRUE)
  #add 10 more observations to sample (1:50) and perform t-test
  t.test4 <- t.test(Group2[1:50], Group2[1:50], var.equal=TRUE)

  #signif = 1 if the t-test returns a p-value < alpha, 0 otherwise
  #if not signif, then add 10 observations to the sample 
  signif <- ifelse(t.test1$p.value < alpha | 
                     t.test2$p.value < alpha | 
                     t.test3$p.value < alpha |
                     t.test4$p.value < alpha,
                   1, 0)
  return(signif)
}
  
#replicate Situation B 15,000 times for different p-values (alpha)
#p = alpha = 0.1
replicates1 <- replicate(15000, SituationB(0.1))
SitBSim1 <- mean(replicates1)

#p = alpha = 0.05
replicates2 <- replicate(15000, SituationB(0.05))
SitBSim2 <- mean(replicates2)

#p = alpha = 0.01
replicates3 <- replicate(15000, SituationB(0.01))
SitBSim3 <- mean(replicates3)
