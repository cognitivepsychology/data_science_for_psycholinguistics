# 문제 1.
# 정답: 3번. purrr 패키지는 함수를 사용하여 반복적 업무를 효과적으로 수행하는 데 사용.

# 문제 2.
# crimtab 데이터셋은 정제된 데이터가 아님. 정제된 데이터가 되려면, 각 열이 하나의 변수를 나타내고 각 행이 하나의 관찰값을 나타내야 함. 그러나 해당 데이터셋은 각 열이 어떤 변수를 나타내는지 알 수 없음. 

# 문제 3.
library(dslabs)
data(murders)
murders %>%
  mutate(pop_mil = population/10^6) # 1000000도 OK.

# 문제 4.
murders %>%
  filter(region %in% c("West", "Northeast"))

# 문제 5.

# 답안 1:
airquality %>%
  select(-c(Solar.R, Wind, Temp)) 

# 답안 2:
airquality %>%
  select(-Solar.R, -Wind, -Temp)

# 답안 3:
airquality %>%
  select(Ozone, Month, Day) # 이 경우 10점 만점에서 5점.


# 문제 6.
airquality %>%
  select(Ozone, Temp, Month, Day) %>%
  filter(Ozone >= 50)

# 문제 7.

# 답안 1:
Orange %>%
  filter(Tree %in% c(1:3)) %>%
  summarise(age_min = min(age), age_max = max(age),
            cir_min = min(circumference), cir_max = max(circumference))

# 답안 2:
Orange %>%
  filter(Tree %in% c(1,2,3)) %>%
  summarise(age_min_max = quantile(age,c(0,1)),
            cir_min_max = quantile(circumference,c(0,1)))

# 문제 8.
Orange %>%
  group_by(Tree) %>%
  summarise(age_mean = mean(age, na.rm = T),
            age_sd = sd(age, na.rm = T),
            cir_mean = mean(circumference, na.rm = T),
            cir_sd = sd(circumference, na.rm = T))

# 문제 9.
stars %>%
  arrange(type, desc(magnitude, temp)) %>%
  head(30)

# 문제 10.

# 답안 1:
iris %>%
  mutate(Petal.Length_level = case_when(Petal.Length <= 2 ~ "short",
                                        between(Petal.Length, 2, 4) ~ "middle",
                                        T ~ "long"))

# 답안 2:
iris %>%
  mutate(Petal.Length_level = case_when(Petal.Length <= 2 ~ "short",
                                        Petal.Length > 2 & Petal.Length <= 4 ~ "middle",
                                        Petal.Length > 4 ~ "long"))

