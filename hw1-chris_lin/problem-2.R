library(tidyverse)

# (4) sample
n <- 100
mean_value <- 0
sd_value <- 1
z1 <- rnorm(n, mean_value, sd_value)
z2 <- rnorm(n, mean_value, sd_value)
x1 <- z1 + 0.5 * z2
x2 <- z1 - 0.5 * z2
temp <- rep(1, n)
X <- data.frame(identity = temp, x1 = x1, x2 = x2)
corr <- cor(X$x1, X$x2)

# x*1 versus x*2
fig1 <- ggplot(X, aes(x = x1, y = x2)) + 
  geom_point(color = "orange", show.legend = FALSE) +
  labs(x = 'x*1', y = 'x*2') +
  ggtitle(paste("Correlation =", corr)) +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('./figures-and-tables/2-4-variable.png', plot = fig1)

# regression
beta <- matrix(c(0, 1, 2), nrow = 3, ncol = 1)
X <- as.matrix(X)
m <- 250
beta_table <- data.frame()
for (i in 1:m){
  y <- X %*% beta + rnorm(n, mean_value, sd_value)
  beta_ <- t(solve(t(X)%*%X)%*%t(X)%*%y)
  beta_table <- rbind(beta_table, beta_)
}
colnames(beta_table) <- c("beta0", "beta1", "beta2")
corr <- cor(beta_table$beta1, beta_table$beta2)
#plot
fig2 <- ggplot(beta_table, aes(x = beta1, y = beta2)) + 
  geom_point(color = "purple", show.legend = FALSE) +
  labs(x = expression(paste(beta[1])), y = expression(paste(beta[2]))) +
  geom_point(aes(x = beta[2], y = beta[3]), color = 'red') +
  ggtitle(paste("Correlation =", corr)) +
  theme(plot.title = element_text(hjust = 0.5)) 
ggsave('./figures-and-tables/2-4-regression.png', plot = fig2)

library('cowplot')
fig <- plot_grid(fig1, fig2, ncol = 2)
ggsave('./figures-and-tables/cowplot.png', plot = fig)
