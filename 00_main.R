## #############################################################
## Research article 'Quantifying Social Complexity'           ##
## by Julia Fischer, Max S Farnworth, Holger Sennhenn-Reulen, ##
## and Kurt Hammerschmidt                                     ##
## Simulation R code                                          ##
## April 19, 2017                                             ##
## #############################################################
library("plyr")
source("01_sim_function.R")
source("01_sim1.R")
source("01_sim2.R")
source("01_sim3.R")
source("01_sim4.R")

seed <- 1604
aux1 <- do_it_all1(seed = seed)
dat1 <- aux1$dat

seed <- 1604
aux2 <- do_it_all2(seed = seed)
dat2 <- aux2$dat

seed <- 1604
aux3 <- do_it_all3(seed = seed)
dat3 <- aux3$dat

seed <- 1604
aux4 <- do_it_all4(seed = seed)
dat4 <- aux4$dat

source("02_plot.R")

write.csv(dat1, file = "dat1_new.csv", row.names = FALSE)
write.csv(dat2, file = "dat2_new.csv", row.names = FALSE)
write.csv(dat3, file = "dat3_new.csv", row.names = FALSE)
write.csv(dat4, file = "dat4_new.csv", row.names = FALSE)