# 必要なパッケージを読み込む
library(ggplot2)
library(gganimate)
library(gifski)

# x軸の範囲
x <- seq(-4, 6, length.out = 200)

# 帰無仮説と対立仮説の分布
null_dist <- dnorm(x, mean = 0, sd = 1)   # 帰無仮説（H0）
alt_dist  <- dnorm(x, mean = 2, sd = 1)   # 対立仮説（H1）

# α（有意水準）の変化
alpha_values <- seq(0.01, 0.2, length.out = 10)

# アニメーション用データフレームを作成
df_list <- lapply(alpha_values, function(alpha) {
  crit_value <- qnorm(1 - alpha, mean = 0, sd = 1)  # 棄却域の閾値
  beta <- pnorm(crit_value, mean = 2, sd = 1)       # βの計算（第二種の誤り）
  
  data.frame(
    x = rep(x, 2),
    density = c(null_dist, alt_dist),
    group = rep(c("H0", "H1"), each = length(x)),
    alpha = alpha,
    beta = beta,
    crit_value = crit_value
  )
})

df <- do.call(rbind, df_list)

# アニメーションの作成
p <- ggplot(df, aes(x = x, y = density, color = group, group = group, linetype = gropu)) +
  geom_line(size = 1.2) +
  geom_area(data = df[df$x > df$crit_value, ], aes(fill = "Type I Error (α)"), alpha = 0.4) +
  geom_area(data = df[df$x < df$crit_value & df$group == "H1", ], aes(fill = "Type II Error (β)"), alpha = 0.4) +
  geom_vline(aes(xintercept = crit_value), linetype = "dashed") +
  scale_color_manual(values = c("H0" = "black", "H1" = "blue")) +
  scale_fill_manual(values = c("Type I Error (α)" = "red", "Type II Error (β)" = "blue")) +
  labs(title = "関係: α（赤）が大きくなると、β（青）が小さくなる",
       subtitle = "α = {frame_time}",
       x = "統計量",
       y = "密度") +
  theme_minimal() +
  transition_time(alpha) +
  ease_aes('linear')

# GIFとして保存
anim_save("alpha_vs_beta.gif", animation = p, renderer = gifski_renderer())

ggplot(df, aes(x = x, y = density, color = group, group = group, linetype = group)) +
  geom_line(size = 1.2) +
  #geom_area(data = df[df$x > df$crit_value, ], aes(fill = "Type I Error (α)"), alpha = 0.4) +
 # geom_area(data = df[df$x < df$crit_value & df$group == "H1", ], aes(fill = "Type II Error (β)"), alpha = 0.4) +
  geom_vline(aes(xintercept = crit_value), linetype = "dashed") +
  scale_color_manual(values = c("H0" = "black", "H1" = "blue")) +
  scale_fill_manual(values = c("Type I Error (α)" = "red", "Type II Error (β)" = "blue")) +
  labs(title = "関係: α（赤）が大きくなると、β（青）が小さくなる",
       subtitle = "α = {frame_time}",
       x = "統計量",
       y = "密度") +
  theme_minimal() +
  geom_ribbon(data = subset(df, x < mean(density) - 1.96 & group == "H0"),  
              aes(ymin = 0, ymax = density), 
              fill = "skyblue", alpha = 0.7) +
  geom_ribbon(data = subset(df, x > mean(density) + 1.96 & group == "H0"),  
              aes(ymin = 0, ymax = density), 
              fill = "skyblue", alpha = 0.7)
              
