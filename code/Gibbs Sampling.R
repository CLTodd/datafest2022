bivariate_gibbs = function(m, x0,
                           mu1, mu2,
                           sigma1, sigma2,
                           rho) {

  results = matrix(0, nrow=m, ncol=2)
  results[1, ] = x0
  for (i in 1:(m-1)) {
    # update first element of current state
    results[i+1, 1] = rnorm(
      1,
      mu1 + rho * sigma1 / sigma2 * (results[i, 2] - mu2),
      sqrt((1 - rho**2) * sigma1**2)
    )
    # update second element of current state
    results[i+1, 2] = rnorm(
      1,
      mu2 + rho * sigma2 / sigma1 * (results[i+1, 1] - mu1),
      sqrt((1 - rho**2) * sigma2**2)
    )
  }
  # return results
  data.frame(results)
}



library(HistData)
# calculate Galton parameters
g_mu1 = mean(s5$weeks == 0)
g_mu2 = mean(s5$weeks == 24)
g_s1 = sd(s5$weeks == 0)
g_s2 = sd(s5$weeks == 24)
g_rho = cor(s5$weeks == 0, s5$weeks == 24)
#set.seed(440)
gibbs_res = bivariate_gibbs(10000, c(g_mu1, g_mu2),
                            g_mu1, g_mu2,
                            g_s1, g_s2,
                            g_rho)
names(gibbs_res) = c("ZeroWeeks", "Twenty4Weeks")
ggplot(data=gibbs_res, mapping=aes(x=ZeroWeeks,
                                   y=Twenty4Weeks)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Gibbs sampling histogram")


library(MASS)

sim_naive_res = data.frame(
  mvrnorm(10000,
          c(1, 1),
          matrix(c(2, -1, -1, 2), nrow=2))
)


naive_mc_res = data.frame(mvrnorm(10000,
                                  colMeans(s5),
                                  cov(s5)))
names(naive_mc_res) = c("ZeroWeeks", "Twenty4Weeks")

ggplot(data=naive_mc_res, mapping=aes(x=ZeroWeeks,
                                      y=Twenty4Weeks)) +
  geom_bin2d() +
  geom_density_2d(color="white") +
  ggtitle("Naive MC sampling histogram")
