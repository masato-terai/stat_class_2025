
# 伝えたいこと
## 事前分布を特に設定しない場合、頻度統計の結果もベイズ統計の結果も近似する

### 参考文献 Gelman et al. Regression and Other Stories
#

dat <- read.csv("../stat_class_2025/sample_data/pokemon_data.csv")

re_f <- lm(dat$HP ~ dat$高さ) # least-squares

library(rstanarm)
set.seed(123)

prior_samples <- rnorm(4000, mean = 0, sd = 66)

re_b <- stan_glm(dat$HP ~ dat$高さ, seed = 123, refresh = 0) #priors weakly informative by default

re_b <- stan_glm(dat$HP ~ dat$高さ, seed = 123, refresh = 0, prior = NULL) #priors weakly informative by default

prior_summary(re_b)

summary(re_b)

posterior_samples <- as.matrix(re_b, pars = "dat$高さ")# "am" の事後分布サンプルを取得

df_prior <- data.frame(value = prior_samples, type = "Prior")
df_posterior <- data.frame(value = posterior_samples, type = "Posterior")

colnames(df_prior) <- c("value", "type")
colnames(df_posterior) <- c("value", "type")

df_combined <- rbind(df_prior, df_posterior)

ggplot(df_combined, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior vs Posterior Distribution",
       x = "Coefficient Value",
       y = "Density") +
  xlim(min(df_posterior$value) -10, max(df_posterior$value) +10)

# より狭い事前分布をおく：事前分布の影響が大きくなる（狭いと）
prior_samples2 <- rnorm(4000, mean = 0, sd = 0.2)  # 事前分布からサンプリング

re_b_i <- stan_glm(dat$HP ~ dat$高さ, prior = normal(0, 0.2), seed = 123, refresh = 0) # 事前分布設定

posterior_samples2 <- as.matrix(re_b_i, pars = "dat$高さ")# "am" の事後分布サンプルを取得

df_prior2 <- data.frame(value = prior_samples2, type = "Prior")
df_posterior2 <- data.frame(value = posterior_samples2, type = "Posterior")

colnames(df_posterior2) <- c("value", "type")

df_combined2 <- rbind(df_prior2, df_posterior2)

ggplot(df_combined2, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Prior vs Posterior Distribution",
       x = "Coefficient Value",
       y = "Density")


summary(re_f)
summary(re_b, digits = 4)

# Informative priorを敷くと、係数の値が小さくなった
