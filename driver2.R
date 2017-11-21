# I confirm that the attached work is my own, except where clearly indicated in the text.

source("as3_v2.R")

library("ggplot2")

#density plots of distributions
x.norm <- seq(-4, 4, length=100)
plot(dnorm(x.norm), type = "l", ylab = "Density", xlab = "x", main = "Normal(0,1) Density")
x.pois <- seq(1, 30, length=30)
plot(dpois(x = x.pois, lambda = 10), type = "l", ylab = "Density", xlab = "x", main = "Poisson(10) Density")
x.gamma <- seq(0, 40, length(40))
plot(dgamma(x = x.gamma, 2, 0.5), type ="l", ylab = "Density", xlab = "x", main = "Gamma(2,0.5) Density")

#------------------------------------------------------------------------------


set.seed(82398)

#BIG sims
norm.perc.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                           n.v = c(99, 499, 999), B = 999, n = 999, 
                           method = "percentile", 
                           dist = "normal", alpha = 0.025,
                           mean = 0 , sd = 1)

norm.bca.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                          n.v = c(99, 499, 999), B = 999, n = 999,
                          method = "bca", 
                          dist = "normal", alpha = 0.025,
                          mean = 0 , sd = 1)

norm.smooth.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                             n.v = c(99, 499, 999), B = 999, n = 999,
                             method = "smooth", 
                             dist = "normal", alpha = 0.025,
                             mean = 0 , sd = 1)

norm.t.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                        n.v = c(99, 499, 999), B = 999, n = 999,
                        dist = "normal", alpha = 0.025, method ="t",
                        mean = 0 , sd = 1)

pois.perc.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                           n.v = c(99, 499, 999), B = 999, n = 999,
                           method = "percentile", 
                           dist = "poisson", alpha = 0.025,
                           lambda = 10)

pois.bca.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                          n.v = c(99, 499, 999), B = 999, n = 999,
                          method = "bca", 
                          dist = "poisson", alpha = 0.025,
                          lambda = 10)


pois.smooth.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                             n.v = c(99, 499, 999), B = 999, n = 999,
                             method = "smooth", 
                             dist = "poisson", alpha = 0.025,
                             lambda = 10)

pois.t.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                        n.v = c(99, 499, 999), B = 999, n = 999,
                        method = "t", 
                        dist = "poisson", alpha = 0.025,
                        lambda = 10)

gamma.perc.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                            n.v = c(99, 499, 999), B = 999, n = 999,
                            method = "percentile", 
                            dist = "gamma", alpha = 0.025,
                            shape = 2, rate = 1/2)

gamma.bca.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                           n.v = c(99, 499, 999), B = 999, n = 999,
                           method = "bca", 
                           dist = "gamma", alpha = 0.025,
                           shape = 2, rate = 1/2)

gamma.smooth.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                              n.v = c(99, 499, 999), B = 999, n = 999,
                              method = "smooth", 
                              dist = "gamma", alpha = 0.025,
                              shape = 2, rate = 1/2)

gamma.t.sim <- do.sim.np(sims = 1000, B.v = c(99, 499, 999), 
                         n.v = c(99, 499, 999), B = 999, n = 999,
                         method = "t", 
                         dist = "gamma", alpha = 0.025,
                         shape = 2, rate = 1/2)
#------------------------------------------------------------------------------



set.seed(7729)


#small sims
norm.perc.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                           n.v = seq(from=49,to=199,by=50), B = 999, n = 999, 
                           method = "percentile", 
                           dist = "normal", alpha = 0.025,
                           mean = 0 , sd = 1)

norm.bca.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                          n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                          method = "bca", 
                          dist = "normal", alpha = 0.025,
                          mean = 0 , sd = 1)

norm.smooth.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                             n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                             method = "smooth", 
                             dist = "normal", alpha = 0.025,
                             mean = 0 , sd = 1)

norm.t.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                        n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                        dist = "normal", alpha = 0.025, method ="t",
                        mean = 0 , sd = 1)

pois.perc.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                           n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                           method = "percentile", 
                           dist = "poisson", alpha = 0.025,
                           lambda = 10)

pois.bca.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                          n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                          method = "bca", 
                          dist = "poisson", alpha = 0.025,
                          lambda = 10)


pois.smooth.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                             n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                             method = "smooth", 
                             dist = "poisson", alpha = 0.025,
                             lambda = 10)

pois.t.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                        n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                        method = "t", 
                        dist = "poisson", alpha = 0.025,
                        lambda = 10)

gamma.perc.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                            n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                            method = "percentile", 
                            dist = "gamma", alpha = 0.025,
                            shape = 2, rate = 1/2)

gamma.bca.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                           n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                           method = "bca", 
                           dist = "gamma", alpha = 0.025,
                           shape = 2, rate = 1/2)

gamma.smooth.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                              n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                              method = "smooth", 
                              dist = "gamma", alpha = 0.025,
                              shape = 2, rate = 1/2)

gamma.t.sim.small <- do.sim.np(sims = 1000, B.v = seq(from=49,to=199,by=50), 
                         n.v = seq(from=49,to=199,by=50), B = 999, n = 999,
                         method = "t", 
                         dist = "gamma", alpha = 0.025,
                         shape = 2, rate = 1/2)

#------------------------------------------------------------------------------


#getting coverage for small sims
cov.norm.perc.small <- coverage(true_mean = 0, norm.perc.sim.small)
cov.norm.bca.small <- coverage(true_mean = 0, norm.bca.sim.small)
cov.norm.smooth.small <- coverage(true_mean = 0, norm.smooth.sim.small)
cov.norm.t.small <- coverage(true_mean = 0, norm.t.sim.small)

cov.pois.perc.small <- coverage(true_mean = 10, pois.perc.sim.small)
cov.pois.bca.small <- coverage(true_mean = 10, pois.bca.sim.small)
cov.pois.smooth.small <- coverage(true_mean = 10, pois.smooth.sim.small)
cov.pois.t.small <- coverage(true_mean = 10, pois.t.sim.small)

cov.gamma.perc.small <- coverage(true_mean = 4, gamma.perc.sim.small)
cov.gamma.bca.small <- coverage(true_mean = 4, gamma.bca.sim.small)
cov.gamma.smooth.small <- coverage(true_mean = 4, gamma.smooth.sim.small)
cov.gamma.t.small <- coverage(true_mean = 4, gamma.t.sim.small)

cov.gamma <- list(cov.gamma.perc.small, cov.gamma.bca.small,cov.gamma.smooth.small, 
                  cov.gamma.t.small)

cov.norm <- list(cov.norm.perc.small, cov.norm.bca.small, cov.norm.smooth.small, 
                 cov.norm.t.small)

cov.pois <- list(cov.pois.perc.small, cov.pois.bca.small, cov.pois.smooth.small, 
                 cov.pois.t.small)

cov.perc <- list(cov.norm.perc.small, cov.pois.perc.small, cov.gamma.perc.small)

cov.bca <- list(cov.norm.bca.small, cov.pois.bca.small, cov.gamma.bca.small)

cov.t <- list(cov.norm.t.small, cov.pois.t.small, cov.gamma.t.small)

cov.smooth <- list(cov.norm.smooth.small, cov.pois.smooth.small, cov.gamma.smooth.small)

#------------------------------------------------------------------------------


#getting data frames of small sims
df.gamma <- format.data(cov.gamma)
df.norm <- format.data(cov.norm)
df.pois <- format.data(cov.pois)
df.perc <- format.data(cov.perc)
df.bca <- format.data(cov.bca)
df.t <- format.data(cov.t)
df.smooth <- format.data(cov.smooth)

#plotting mutiple methods for each distribution for small sims
p.gamma.n <- ggplot(data = df.gamma, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("gamma simulation")
p.gamma.B <- ggplot(data = df.gamma, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("gamma simulation")
p.norm.n <- ggplot(data = df.norm, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("normal simulation")
p.norm.B <- ggplot(data = df.norm, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("normal simulation")
p.pois.n <- ggplot(data = df.pois, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("poisson simulation")
p.pois.B <- ggplot(data = df.pois, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("normal simulation")

# plotting multiple distributions for small sims
p.perc.n <- ggplot(data = df.perc, aes(x = n, y = coverage.n, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("percentile simulations")
p.perc.B <- ggplot(data = df.perc, aes(x = B, y = coverage.B, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("percentile simulations")
p.bca.n <- ggplot(data = df.bca, aes(x = n, y = coverage.n, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("bca simulations")
p.bca.B <- ggplot(data = df.bca, aes(x = B, y = coverage.B, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("bca simulations")

#------------------------------------------------------------------------------

#getting pdfs of plots
pdf("gam.B.pdf", h = 4, w = 8)
p.gamma.B
dev.off()

pdf("gam.n.pdf", h = 4, w = 8)
p.gamma.n
dev.off()

pdf("pois.n.pdf", h = 4, w = 8)
p.pois.n
dev.off()

pdf("pois.B.pdf", h = 4, w = 8)
p.pois.B
dev.off()

pdf("norm.n.pdf", h = 4, w = 8)
p.norm.n
dev.off()

pdf("norm.B.pdf", h = 4, w = 8)
p.norm.B
dev.off()

pdf("bca_B.pdf", h = 4, w=8)
p.bca.B
dev.off()

pdf("bca_n.pdf", h = 4, w=8)
p.bca.n
dev.off()

pdf("perc_n.pdf", h = 4, w=8)
p.perc.n
dev.off()

pdf("perc_B.pdf", h = 4, w=8)
p.bca.B
dev.off()

#------------------------------------------------------------------------------

# coverage of big sims
cov.norm.perc <- coverage(true_mean = 0, norm.perc.sim)
cov.norm.bca <- coverage(true_mean = 0, norm.bca.sim)
cov.norm.smooth <- coverage(true_mean = 0, norm.smooth.sim)
cov.norm.t <- coverage(true_mean = 0, norm.t.sim)

cov.pois.perc <- coverage(true_mean = 10, pois.perc.sim)
cov.pois.bca <- coverage(true_mean = 10, pois.bca.sim)
cov.pois.smooth <- coverage(true_mean = 10, pois.smooth.sim)
cov.pois.t <- coverage(true_mean = 10, pois.t.sim)

cov.gamma.perc <- coverage(true_mean = 4, gamma.perc.sim)
cov.gamma.bca <- coverage(true_mean = 4, gamma.bca.sim)
cov.gamma.smooth <- coverage(true_mean = 4, gamma.smooth.sim)
cov.gamma.t <- coverage(true_mean = 4, gamma.t.sim)

cov.gamma.big <- list(cov.gamma.perc, cov.gamma.bca,cov.gamma.smooth, 
                  cov.gamma.t)

cov.norm.big <- list(cov.norm.perc, cov.norm.bca, cov.norm.smooth, 
                 cov.norm.t)

cov.pois.big <- list(cov.pois.perc, cov.pois.bca, cov.pois.smooth, 
                 cov.pois.t)

cov.perc.big <- list(cov.norm.perc, cov.pois.perc, cov.gamma.perc)

cov.bca.big <- list(cov.norm.bca, cov.pois.bca, cov.gamma.bca)

cov.t.big <- list(cov.norm.t, cov.pois.t, cov.gamma.t)

cov.smooth.big <- list(cov.norm.smooth, cov.pois.smooth, cov.gamma.smooth)

#--------------------------------------------------------------------------------------------
#data frames of big sims
df.gamma1 <- format.data(cov.gamma.big)
df.norm1 <- format.data(cov.norm.big)
df.pois1 <- format.data(cov.pois.big)
df.perc1 <- format.data(cov.perc.big)
df.bca1 <- format.data(cov.bca.big)
df.t1 <- format.data(cov.t.big)
df.smooth1 <- format.data(cov.smooth.big)

#----------------------------------------------------------------------------------------------
#plots of multiple methods for one dist - big sims
p.gamma.n1 <- ggplot(data = df.gamma1, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("gamma simulation")
p.gamma.B1 <- ggplot(data = df.gamma1, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("gamma simulation")
p.norm.n1 <- ggplot(data = df.norm1, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("normal simulation")
p.norm.B1 <- ggplot(data = df.norm1, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("normal simulation")
p.pois.n1 <- ggplot(data = df.pois1, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("poisson simulation")
p.pois.B1 <- ggplot(data = df.pois1, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.9,1)) +
  ggtitle("poisson simulation")
#----------------------------------------------------------------------------------------------
#plots of multiple dists for one method - big sims
p.perc.n1 <- ggplot(data = df.perc1, aes(x = n, y = coverage.n, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("percentile simulations")
p.perc.B1 <- ggplot(data = df.perc1, aes(x = B, y = coverage.B, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("percentile simulations")
p.bca.n1 <- ggplot(data = df.bca1, aes(x = n, y = coverage.n, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("bca simulations")
p.bca.B1 <- ggplot(data = df.bca1, aes(x = B, y = coverage.B, dist =dist, group = dist)) +
  geom_point(aes(colour = dist)) +
  geom_line(aes(colour = dist)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("bca simulations")
#------------------------------------------------------------------------------------
#saving pdfs of plots
pdf("gamma.B.pdf", h = 4, w = 8)
p.gamma.B1
dev.off()

pdf("gamma.n.pdf", h = 4, w = 8)
p.gamma.n1
dev.off()

pdf("poisson.n.pdf", h = 4, w = 8)
p.pois.n1
dev.off()

pdf("poisson.B.pdf", h = 4, w = 8)
p.pois.B1
dev.off()

pdf("normal.n.pdf", h = 4, w = 8)
p.norm.n1
dev.off()

pdf("normal.B.pdf", h = 4, w = 8)
p.norm.B1
dev.off()

pdf("bcaB.pdf", h = 4, w=8)
p.bca.B1
dev.off()

pdf("bcan.pdf", h = 4, w=8)
p.bca.n1
dev.off()

pdf("percn.pdf", h = 4, w=8)
p.perc.n1
dev.off()

pdf("percB.pdf", h = 4, w=8)
p.perc.B1
dev.off()

#------------------------------------------------------------------------------------------

set.seed(04286)
#messing with really small B and n
norm.perc.sim.small2 <- do.sim.np(sims = 1000, B.v = seq(from=9,to=99,by=10), 
                                 n.v = seq(from=9,to=99,by=10), B = 999, n = 999, 
                                 method = "percentile", 
                                 dist = "normal", alpha = 0.025,
                                 mean = 0 , sd = 1)

norm.bca.sim.small2 <- do.sim.np(sims = 1000, B.v = seq(from=9,to=99,by=10), 
                                n.v = seq(from=9,to=99,by=10), B = 999, n = 999,
                                method = "bca", 
                                dist = "normal", alpha = 0.025,
                                mean = 0 , sd = 1)

cover.bca <- coverage(0, norm.bca.sim.small2)
cover.perc <- coverage(0, norm.perc.sim.small2)
covs <- list(cover.bca, cover.perc)
dataf <- format.data(covs)
norm.B <- ggplot(data = dataf, aes(x = B, y = coverage.B, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.5,1)) +
  ggtitle("normal simulations")
norm.n <- ggplot(data = dataf, aes(x = n, y = coverage.n, method = method, group = method)) +
  geom_point(aes(colour = method)) +
  geom_line(aes(colour = method)) +
  coord_cartesian(ylim = c(0.8,1)) +
  ggtitle("normal simulations")
