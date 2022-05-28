# 1. 아래의 지시문을 차례대로 시행해주세요.

# (1) data("PlantGrowth")를 실행해주세요.
# (2) pastecs 패키지를 불러들여주세요.
# (3) by 함수를 사용하여 group이라는 집단별수에 따라 3개 층위별 weight의 기술통계량을 계산해주세요. 이때 stat.desc 함수를 사용해주세요. 
# 코드와 결과를 함께 제시해주세요.

library(pastecs)
by(data = PlantGrowth$weight, INDICES = PlantGrowth$group, FUN = stat.desc, basic = F, norm = T)

# 2. 아래의 지시문을 차례대로 시행해주세요.

# (1) 1번에 사용한 데이터셋을 그대로 사용해주세요.
# (2) psych 패키지를 불러들여주세요.
# (3) describeBy 함수를 사용하여 group이라는 집단별수에 따라 3개 층위별 weight의 기술통계량을 계산해주세요. 
# (4) 이때 round 함수를 사용하여 소수점 이하 3자리까지만 표시되도록 해주세요.
# 코드와 결과를 함께 제시해주세요.

library(psych)
describeBy(PlantGrowth$weight, PlantGrowth$group, mat = T, digits = 3) 
# 제가 문제의 의도를 제대로 전달하지 못한 듯합니다.  matrix 형태로 결과가 제시될 경우, digits 논항으로 rounding이 가능합니다. help(describeBy)의 각 논항 설명을 참고 바랍니다.

# 3. 아래의 지시문을 차례대로 시행해주세요.

# (1) skewed_data.2.rda를 로딩해주세요.
# (2) 자료변형 전 관찰값 score에 대한 히스토그램을 그려주세요.
# (3) 관찰값 score를 실수 e를 밑으로 하는 자연 로그 값으로 변환한 뒤 히스토그램을 그려주세요.
# 코드와 결과를 함께 제시해주세요.

load("skewed_data.2.rda")

skewed_data.2.log <- skewed_data.2 %>%
  mutate(score_log = log(score))

skewed_data.2 %>% 
  ggplot(aes(x = score)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(adjust = 2, lwd = 2) +
  facet_wrap(~ gender)

skewed_data.2.log %>% 
  ggplot(aes(x = score_log)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.2) +
  geom_density(adjust = 2, lwd = 2) +
  facet_wrap(~ gender)


# 4. 아래의 지시문을 차례대로 시행해주세요.

# (1) skewed_data.1.rda를 로딩해주세요.
# (2) 자료변형 전 관찰값 score에 대한 히스토그램을 그려주세요.
# (3) 가장 큰 관찰값에서 개별 관찰값을 뺀 뒤 히스토그램을 그려주세요.
# 코드와 결과를 함께 제시해주세요.

load("skewed_data.1.rda")

# 일자별 최댓값 구하기.

skewed_data.1.max <- skewed_data.1 %>%
  group_by(gender) %>%
  summarise(max = max(score, na.rm =T)) %>%
  pull()

# 가장 큰 관찰값에서 개별 관찰값 빼기.

skewed_data.1.maxsub <- skewed_data.1 %>%
  mutate(score_maxsub = case_when(gender == "female" ~ score - skewed_data.1.max[1],
                                  gender == "male" ~ score - skewed_data.1.max[2],
                                  T ~ score
                                          )
         )  

# 변형 전 히스토그램

skewed_data.1 %>% 
  ggplot(aes(x = score)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(adjust = 2, lwd = 2) +
  facet_wrap(~ gender)

# 변형 후 히스토그램

skewed_data.1.maxsub %>% 
  ggplot(aes(x = score_maxsub)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(adjust = 2, lwd = 2) +
  facet_wrap(~ gender)

# 5. 아래의 지시문을 차례대로 시행해주세요.

# (1) treatment.rda를 로딩해주세요.
# (2) treatment 데이터셋을 바탕으로 contingency table을 만들어주세요.
# (3) 해당 contingency table을 토대로 카이제곱검정을 시행해주세요.
# 코드와 결과를 함께 제시해주세요.

load("treatment.rda")
treatment.table <- table(treatment$treatment, treatment$improvement)
chisq.test(treatment.table) 


# 6. 아래의 지시문을 차례대로 시행해주세요.

# (1) data("mtcars")를 실행한 뒤 tibble 형식으로 변환해주세요.
# (2) 1, 3, 4, 5, 6, 7번 열을 서브세팅해주세요.
# (3) GGally 패키지를 불러들여 mtcars 데이터셋에 대한 산점도, 분포도, 상관계수를 구해주세요.
# (4) correlation 패키지를 불러들여 mtcars 데이터셋에 대한 상관계수 Pearson R를 산출해주세요.
# 코드와 결과를 함께 제시해주세요.

data("mtcars")
mtcars_tib <- mtcars %>%
  as_tibble() %>%
  dplyr::select(1, 3, 4, 5, 6, 7)

library(GGally)
ggpairs(mtcars_tib)

library(correlation)
mtcars_tib %>%
  correlation(digits = 3) # digits 논항 생략 가능.

# 7. 아래의 지시문을 차례대로 시행해주세요.

# (1) data("mpg")를 실행해주세요.
# (2) 5, 9번 열을 서브세팅해주세요.
# (3) GGally 패키지를 불러들여 mpg 데이터셋에 대한 산점도, 분포도, 상관계수를 구해주세요.
# (4) correlation 패키지를 불러들여 mpg 데이터셋에 대한 상관계수 spearman의 rho를 산출해주세요.
# 코드와 결과를 함께 제시해주세요.

data("mpg")
mpg_tib <- mpg %>%
  as_tibble() %>%
  dplyr::select(5, 9)

library(GGally)
ggpairs(mpg_tib)

library(correlation)
mpg_tib %>%
  correlation(digits = 3, method = "spearman") # digits 논항 생략 가능.

# 8. 아래의 지시문을 차례대로 시행해주세요.

# (1) data("mpg")를 실행해주세요.
# (2) 5, 9번 열을 서브세팅해주세요.
# (3) GGally 패키지를 불러들여 mpg 데이터셋에 대한 산점도, 분포도, 상관계수를 구해주세요.
# (4) psych 패키지를 불러들여 mpg 데이터셋에 대한 상관계수 Kendall의 tau를 산출해주세요.
# 코드와 결과를 함께 제시해주세요.

data("mpg")
mpg_tib <- mpg %>%
  as_tibble() %>%
  dplyr::select(5, 9)

library(GGally)
ggpairs(mpg_tib)

library(psych)
mpg_tib %>%
  corr.test(method = "kendall")

# 9. 아래의 지시문을 차례대로 시행해주세요.

# (1) zombi.rda를 로딩해주세요.
# (2) lm 일반선형모형 함수를 바탕으로 Immobiity 결과변수를 Taser 예측변수로 예측해주세요.
# (3) 해당 lm 모형을 zombie_lm이라는 변수명에 할당해주세요.
# (4) broom 패키지를 사용하여 모형의 적합도와 모수정보를 제시해주세요.
# 코드와 결과를 함께 제시해주세요.

load("zombie.rda")
zombie_lm <- lm(Immobility ~ Taser, data = zombie, na.action = na.exclude)

library(broom)
glance(zombie_lm)
tidy(zombie_lm)

# 10. 아래의 지시문을 차례대로 시행해주세요.

# (1) zombi 데이터셋에 대해 lm 일반선형모형 함수를 바탕으로 Immobiity 결과변수를 rTMS와 Taser 예측변수로 예측해주세요.
# (2) 해당 lm 모형을 zombie_full_lm이라는 변수명에 할당해주세요.
# (3) broom 패키지를 사용하여 모형의 적합도와 모수정보를 제시해주세요.
# (4) anova 함수를 사용하여 zombie_lm 모형과 zombie_full_lm 모형의 적합도를 비교해주세요. 
# (5) 예측변수 추가 후 모형의 적합도에 어떤 변화가 있었는지 간단히 설명해주세요.
# 코드와 결과를 함께 제시해주세요.

zombie_full_lm <- lm(Immobility ~ rTMS + Taser, data = zombie, na.action = na.exclude)
glance(zombie_full_lm)
tidy(zombie_full_lm)
anova(zombie_lm, zombie_full_lm) # 해석: rTMS 예측변수 추가 후 모형 적합도가 통계적으로 유의미하게 상승.