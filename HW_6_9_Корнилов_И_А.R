library(dplyr)
parf <- read.csv("C:/advacedA/parfume.csv")
parf <- parf %>% filter(vote <= 1)
votes <- c(sum(parf$vote), length(parf$vote) - sum(parf$vote))
barplot (votes, xlab = "хочет девушка купить или нет", ylab = "кол-во", main = "Сколько девушек хотят купить парф.воду", col = c("blue", "red"), legend.text = c("да", "нет"))

test <- parf %>% filter(group == "test")
control <- parf %>% filter(group == "control")

# Формклируем нулевую гипотезу
# Н0 - нет различия в тестовой и контрольной группе (девушкам все равно какой ролик показывается (цвет или ЧБ), от этого кол-во покупок не меняется)
# Н1 - есть значимая разница в тестовой и контрольной группе (покупают в тестовой группе (чб) больше)

# Используем бутстреп для проверки гипотезы (итераций 1000 сид 123 и центрирование)
N <- 1000
differences <- rep(NA, N)
set.seed(123)
for (i in 1:N){
  s1 <- sample(control$vote, replace = TRUE)
  s2 <- sample(test$vote, replace = TRUE)
  p1 <- sum(s1)/length(s1)
  p2 <- sum(s2)/length(s2)
  p_diff <- p2 - p1
  differences[i] <- p_diff
}
differences_cent <- differences - mean(differences)
diff_df$dif_cent <- differences_cent 
ggplot(data = diff_df, aes(x = dif_cent)) + geom_histogram(fill = "lightblue", color = "navy") + geom_vline(xintercept = p_diff_1, color = "red", lty = 2)
sum(differences_cent > p_diff_1)

# 0 < 0.05 H0 reject
# Есть значимая разница по бустрепу - есть значимая разница - выбираем ролик ЧБ

votes_control <- c(sum(control$vote), length(control$vote))
votes_test <- c(sum(test$vote), length(test$vote))


prop.test(votes_control, votes_test, alternative = "less")
# гипотезу о равенстве долей можем отвергнуть pvalue почти ноль
# Есть значимая разница по стат тесту  - есть значимая разница - выбираем ролик ЧБ

