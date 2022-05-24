## 1. 아래의 지시문을 차례대로 시행해주세요.

# (1) mask_tib.rda 파일을 로딩해주세요.
# (2) t-test 함수를 사용하여 독립표본 t-검정을 시행해주세요. 독립변수는 Mask, 종속변수는 Popularity입니다.
# (3) broom 패키지의 tidy 함수로 결과를 요약해주세요.
# (4) effectsize 패키지로 Cohen의 d 효과 크기를 산출해주세요.
# (5) 검정결과 및 효과 크기에 대해 간단하게 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

load("mask_tib.rda")
library(effectsize)
mask_tib_ttest <- t.test(Popularity ~ Mask, data = mask_tib, na.action = na.exclude)
tidy(mask_tib_ttest) 
cohens_d(Popularity ~ Mask, data = mask_tib)

## 2. 아래의 지시문을 차례대로 시행해주세요.

# (1) mask_tib_rm.rda 파일을 로딩해주세요.
# (2) pivot_longer 함수를 사용하여 human 열과 animal 열을 Mask 열(독립변수)과 Popularity 열(종속변수)로 변형해주세요. 그리고 해당 티블을 mask_tib_rm.1 변수에 할당해주세요.
# (3) t-test 함수를 사용하여 종속표본 t-검정을 시행해주세요. 독립변수는 Mask, 종속변수는 Popularity입니다.
# (4) broom 패키지의 tidy 함수로 결과를 요약해주세요.
# (5) effectsize 패키지로 Hedges의 g 효과 크기를 산출해주세요.
# (6) 검정결과 및 효과 크기에 대해 간단하게 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

load("mask_tib_rm.rda")
mask_tib_rm.1 <- mask_tib_rm %>%
  pivot_longer(human:animal, names_to = "Mask", values_to = "Popularity")

mask_tib_rm.1.ttest <- t.test(Popularity ~ Mask, data = mask_tib, na.action = na.exclude, paired = T)
tidy(mask_tib_rm.1.ttest)
hedges_g(Popularity ~ Mask, data = mask_tib_rm.1, paired = T)

## 3. 아래의 지시문을 차례대로 시행해주세요.

# (1) mask_tib.rda 파일을 로딩해주세요.
# (2) WRS2 패키지의 yuen 함수를 사용하여 robust 독립표본 t-검정을 시행해주세요. 
# (3) 독립변수는 Mask, 종속변수는 Popularity입니다. 부트스트래핑 횟수는 1000회, 양극단 절단치는 20%, 신뢰구간은 대칭적으로 부트스트래핑해주세요.
# (4) 검정결과 및 효과 크기에 대해 간단하게 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

library(WRS2)
yuen(Popularity ~ Mask, data = mask_tib, nboot = 1000, side = T)

## 4. 아래의 지시문을 차례대로 시행해주세요.

# (1) mask_tib_rm 데이터셋에 대해 WRS2 패키지의 yuend 함수를 사용하여 종속표본 t-검정을 시행해주세요. 
# (2) 이때 human mask과 animal mask의 popularity 효과를 비교해주세요.
# (3) 검정결과 및 효과 크기에 대해 간단하게 해석해주세요. 부트스트래핑 횟수는 1000회로, 양극단 절단치는 20%로 설정해주세요.
# 코드와 결과를 함께 제시해주세요.

library(WRS2)
yuend(mask_tib_rm$human, mask_tib_rm$animal, nboot = 1000, side = T)

## 5. 아래의 지시문을 차례대로 시행해주세요(2점).

# (1) memory_tib.rda 파일을 로딩해주세요.
# (2) 데이터셋 설명: 36명의 참여자를 대상으로 5분 간 10가지 정보가 담긴 비디오를 보여주었음. 통제집단(N = 12)은 어떤 처치도 하지 않은 집단, erase 집단(N = 12)은 머릿속 ID 칩에 전자파를 보낸 집단, replace 집단(N =  12)은 원래 정보와 헛갈리는 정보를 머릿속 ID 칩에 보낸 집단임. 결과변수는 10가지 정보 중 몇 가지를 기억해냈는가임. 연구가설은 "전자파나 헛갈리는 정보를 받은 집단이 통제집단보다 회상률이 낮을 것이다"임. 
# A. ID: 참여자 ID.
# B. Group: 참여자가 할당된 칩의 유형.
# C. Recall: 10가지 정보 중 참여자가 기억해낸 정보의 수.
# D. Erase_Dummy: Erase 조건과 통제조건을 비교하는 더미 변수.
# E. Replace_Dummy: Replace 조건과과 통제조건을 비교하는 더미 변수.
# (3) lm 함수를 사용하여 Erase_Dummy와 Replace_Dummy가 예측변수, Recall이 결과변수인 일반선형모형을 생성한 뒤, memory_lm이라는 객체에 할당해주세요.
# (4) broom 패키지의 glance와 tidy 함수를 사용하여 모형의 적합도 지수와 모수를 측정해주세요.
# (5) ggfortify 패키지의 autoplot 함수를 사용하여 1-6번 플롯을 모두 생성해주세요. 그리고 플롯들을 토대로 해당 모형이 통계적 가정을 충족하는지 여부를 판단해주세요.
# 코드와 결과를 함께 제시해주세요.

load("memory_tib.rda")
memory_lm <- lm(Recall ~ Erase_Dummy + Replace_Dummy, data = memory_tib, na.action = na.exclude)
glance(memory_lm)
tidy(memory_lm)

library(ggfortify)
autoplot(memory_lm, which = 1:6) + 
  theme_minimal() 

## 6. 아래의 지시문을 차례대로 시행해주세요.

# (1) anova와 lm 함수를 사용하여 memory_tib의 Recall 변수를 Group 변수로 예측한 뒤, 해당 모형을 memory_anova라는 객체에 할당해주세요.
# (2) broom 패키지의 glance와 tidy 함수를 사용하여 모형의 적합도 지수와 모수를 측정해주세요.
# (3) parameters 패키지의 model_parameters 함수를 사용하여 부분 omega 제곱 효과 크기를 산출해주세요.
# 코드와 결과를 함께 제시해주세요.

memory_anova <- lm(Recall ~ Group, data = memory_tib, na.action = na.exclude) %>%
  anova()
glance(memory_anova)
tidy(memory_anova )

library(parameters)
memory_anova %>%
  model_parameters(., omega_squared = "partial")

## 7. 아래의 지시문을 차례대로 시행해주세요. (단답식으로 할 것)

# (1) memory_tib 데이터셋에 대해 아래의 두 가지 대조(contrast) 가중치(weight) 코딩을 설정해주세요. 
# (2) 가중치의 합은 반드시 0이 되도록 설정해주세요. 가중치의 합이 0이 되기만 하면, 어떤 수치든 상관없습니다.
# A. Contrast_1: 기억방해 관련 조건들과 통제조건을 비교하는 대조 코드 사용 변수.
# B. Contrast_2: Erase 조건과 Replace 조건을 비교하는 대조 코드 사용 변수.
# 코드와 결과를 함께 제시해주세요.

memory_contrasts <-  tibble(Group = c("Control", "Erase", "Replace"), 
                            Contrast_1 = c(-2/3, 1/3, 1/3), 
                            Contrast_2 = c(0, -1/2, 1/2)
                            )
memory_contrasts


## 8. 아래의 지시문을 차례대로 시행해주세요.
# (1) memory_tib 데이터셋에 대해 hypr 패키지의 hypr 함수를 사용하여 Group 변수에 바탕을 둔 대조를 설정한 뒤 trtC 객체에 할당해주세요.
# (2) cmat 함수를 사용하여 해당 대조 매트릭스를 확인해주세요.
# (3) hypr 패키지의 contr.hypothesis 함수를 사용하여 대조를 설정해주세요.
# (4) R의 contrasts 함수에 예측변수를 투입한 뒤, (3)에서 설정된 대조를 할당해주세요.
# (5) lm 함수를 사용하여 Group 변수로 Recall 변수를 예측하는 일반선형모형을 생성한 뒤, memory_lm_contrast 객체에 할당해주세요.
# (6) broom 패키지의 glance와 tidy 함수를 사용하여 모형의 적합도 지수와 모수를 측정해주세요.
# (7) 모형의 적합도 지수와 모수에 대해 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

library(hypr)
trtC <- hypr((Erase + Replace)/2 ~ Control , Replace ~ Erase)
cmat(trtC)
contr.hypothesis(trtC)
contrasts(memory_tib$Group) <- contr.hypothesis(trtC) 

library(broom)
memory_lm_contrast <- lm(Recall ~ Group, data = memory_tib, na.action = na.exclude)
glance(memory_lm_contrast)
tidy(memory_lm_contrast)

## 9. 아래의 지시문을 차례대로 시행해주세요. 

# (1) modelbased 패키지의 estimate_contrasts 함수를 사용하여 memory_lm_contrast 선형모형 객체에 대한 사후검정을 시행해주세요. adjustment 관련 논항은 holm으로 설정해주세요. 
# (2) emmeans 패키지의 emmeans 함수를 사용하여 memory_lm_contrast 선형모형 객체에 대한 사후검정을 시행해주세요. adjustment 관련 논항은 bonferroni로 설정해주세요.
# (3) multcomp 패키지의 glht 함수를 사용하여 memory_lm_contrast 선형모형 객체에 대한 사후검정을 시행해주세요. adjustment 관련 논항은 bonferroni로 설정해주세요. 결과요약은 summary 함수를 이용해주세요.
# (4) (1), (2), (3) 사후검정 결과를 간단히 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

library(modelbased)
estimate_contrasts(memory_lm_contrast, contrast = "Group", adjust = "holm")

library(emmeans)
emmeans(memory_lm_contrast, list(pairwise ~ Group), adjust = "bonferroni")

library(multcomp)
memory_lm_contrast_glht <- glht(memory_lm_contrast, linfct = mcp(Group = "Tukey"), test = adjusted("bonferroni")) # 저는 해당 코드 실행 시 문제가 없었는데, 코드 실행이 불가능했다고 하시는 분들이 계시네요. 저도 그 이유를 알아보겠습니다.
summary(memory_lm_contrast_glht) 

## 10. 아래의 지시문을 차례로 시행해주세요.

# (1) WRS2 패키지의 t1way 함수로 memory_tib 데이터셋의 Recall 변수를 Group 변수로 예측하는 robust ANOVA를 시행한 뒤, lincon 함수를 사용하여 사후검정을 시행해주세요. 부트스트래핑 횟수는 100회로, 양극단 절단치는 0.2로 설정해주세요.
# (2) WRS2 패키지의 t1waybt 함수로 memory_tib 데이터셋의 Recall 변수를 Group 변수로 예측하는 robust ANOVA를 시행해주세요. 그런 뒤 mcppb20 함수를 사용하여 사후검정을 시행해주세요. 부트스트래핑 횟수는 둘 다 1000회로, 양극단 절단치는 20%로 설정해주세요.
# (3) (1)과 (2) 모형 적합도 분석 및 사후검정 결과를 간단히 해석해주세요.
# 코드와 결과를 함께 제시해주세요.

library(WRS2)
t1way(Recall ~ Group, data = memory_tib, tr = 0.2, nboot = 100)
lincon(Recall ~ Group, data = memory_tib, tr = 0.2)

t1waybt(Recall ~ Group, data = memory_tib, tr = 0.2, nboot = 1000)
mcppb20(Recall ~ Group, data = memory_tib, tr = 0.2, nboot = 1000)
