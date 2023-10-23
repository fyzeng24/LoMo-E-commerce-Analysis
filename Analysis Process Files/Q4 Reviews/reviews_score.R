# 加载所需的包
library(readr)

# 读取数据文件
data <- read_csv("Analysis Data.csv")

# 创建列联表（contingency table）
contingency_table <- table(data$review_score, data$is_reviewed)

# 执行卡方检验
chi_square_result <- chisq.test(contingency_table, p = 0.01)
chi_square_result

# 使用dplyr包的count函数创建透视表
library(dplyr)

pivot_table <- data %>%
  count(review_score, is_reviewed)
pivot_table$is_reviewed_n = pivot_table$n
pivot_table <- select(pivot_table, -n)

pivot_table

# 绘制柱状图
library(ggplot2)

colors <- c("1" = "#FF7302", "0" = "#475A8D")

plot = ggplot(pivot_table, aes(x = review_score, y = is_reviewed_n, fill = factor(is_reviewed))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Review Score", y = "Review Vloume") +
  ggtitle("review_score and is_reviews") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_fill_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) 

# 计算is_reviewed占比
pivot_table$percentage <- pivot_table$is_reviewed_n / tapply(pivot_table$is_reviewed_n, pivot_table$review_score, sum)[as.character(pivot_table$review_score)] * 100
plot + geom_text(aes(label = paste0(round(pivot_table$percentage, 1), "%")), position = position_stack(vjust = 1), color = "white")

write.csv(pivot_table, file = "reviews_score.csv", row.names = FALSE)

