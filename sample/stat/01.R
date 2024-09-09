theme_set( theme_bw(base_family = "HiraKakuProN-W3")) 
library(ggplot2)

# 男性の身長データ
set.seed(123)

# 男性の身長データ（平均172cm、標準偏差7cm）
male_height <- rnorm(50, mean = 172, sd = 7)

# データフレームに変換
df <- data.frame(PersonID = 1:50, Height = round(male_height, 0))
df$Height

# 最大値と最小値の取得
max_value <- max(df$Height)
min_value <- min(df$Height)

# 範囲の計算
r <- max_value - min_value

# データの個数を取得
n <- length(df$PersonID)

# スタージェスの公式で階級数を計算
k <- ceiling(1 + log2(n))

# 階級幅の計算
class_width <- ceiling(r / k)

# 階級用の値を作成（最小値から階級幅ごとの数列を生成）
class_vec <- seq(from = min_value, to = max_value + class_width, by = class_width)

# 階級値の計算（right=FALSEだと左側の境界を含み、右側は含まない）
cut_data <- cut(df$Height, breaks = class_vec, right = FALSE, include.lowest = TRUE)

# 度数分布表を作成
freq_table <- table(cut_data)

# 累積度数の計算
cumsum(freq_table)

# 相対度数の計算
relative_freq <- prop.table(freq_table)

# 累積相対度数を計算
cum_relative_freq <- cumsum(relative_freq)

# 度数分布表と累積相対度数の確認
freq_table
cum_relative_freq

# 累積相対度数をデータフレームに変換
cum_freq_df <- data.frame(class = names(cum_relative_freq), 
                          cum_relative_freq = as.numeric(cum_relative_freq))

# ヒストグラム
ggplot(df, mapping = aes(x = Height)) +
  geom_histogram(bins = 14) +
  labs(x = "身長", y = "度数", title = "ヒストグラム")

# 累積相対度数折線
ggplot(cum_freq_df, aes(x = class, y = cum_relative_freq, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "身長", y = "累積相対度数", 
       title = "累積相対度数折線グラフ")
