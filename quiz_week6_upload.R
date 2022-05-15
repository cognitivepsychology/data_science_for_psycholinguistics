
# 1. 5주차 수업자료를 참조하여 mtcars.tb 데이터셋을 바탕으로 첨부한 플롯과 가능한 한 유사한 플롯을 생성해주세요. 그러나 플롯의 theme은 여러분의 취향대로 바꿀 수 있습니다. ggthemes 패키지가 제공하는 theme 중에서 골라주세요. 단, 플롯의 제목과 범례 서체의 크기 및 종류는 첨부한 ggthemes 매뉴얼을 참조하여 수정해주셔야 합니다.
# mtcars 데이터셋: 1973-74년 자동차 모델 32종의 연비와 각종 퍼포먼스에 관한 데이터셋.
# 변수: disp = 엔진 배기량, mpg = 연비, car_model= 자동차 모델명, gear = 전진 기어(forward gear)의 수.
# 코드와 결과를 함께 제시해주세요.

library(tidyverse)
library(ggthemes)
library(ggrepel)

Sys.setlocale("LC_COLLATE", "ko_KR.UTF-8")
windowsFonts(NanumGothic = windowsFont("나눔 고딕"))

load("mtcars.tb.rda")

mtcars_tb_plot <- mtcars.tb %>% 
  ggplot(aes(disp, mpg, label = car_model)) +   
  geom_point(aes(col=gear), size = 3) +
  geom_text_repel(max.overlaps = 20) + # ggrepel 패키지 함수 적용.
  scale_x_log10() +
  scale_y_log10() +
  xlab("엔진 배기량(로그 척도)") + 
  ylab("연비(로그 척도)") +
  ggtitle("1973-74년 자동차 모델의 엔진 배기량 대비 연비 퍼포먼스\n(출처: 1974년 Motor Trend US 잡지)")+ 
  scale_color_discrete(name = "전진 기어의 수") +
  theme_economist(base_size = 8,
                  base_family = "NanumGothic"
                  ) # ggthemes 패키지 함수 적용.
mtcars_tb_plot

png(filename = "mtcars_tb_plot.png", width = 2000, height = 1300, type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
mtcars_tb_plot
dev.off()


# 2. norm_data_week6_quiz 데이터셋에 대해 히스토그램과 밀도 플롯을 겹쳐서 그려주세요. binwidth = 5 이내로 설정해주세요. 첨부한 플롯을 참조해주세요.
# 코드와 결과를 함께 제시해주세요.

load("norm_data_week6_quiz.rda")
norm_data_week6_quiz.plot <- norm_data_week6_quiz %>%
  ggplot(aes(foot_size)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5) +
  geom_density(adjust = 2, lwd = 2, color = "blue") 
norm_data_week6_quiz.plot

png(filename = "norm_data_week6_quiz.plot.png", width = 1000, height = 1000, type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
norm_data_week6_quiz.plot
dev.off()


# 3. norm_data_week6_quiz 데이터셋에 대해 qqplotr 패키지를 사용하여 QQ-플롯을 그려주세요.
# 코드와 결과를 함께 제시해주세요.

library(qqplotr)
load("norm_data_week6_quiz.rda")

norm_data_week6_quiz.plot.1 <- norm_data_week6_quiz %>%
  ggplot(aes(sample = foot_size))  +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point()
norm_data_week6_quiz.plot.1

png(filename = "norm_data_week6_quiz.plot.1.png", width = 1000, height = 1000, type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
norm_data_week6_quiz.plot.1
dev.off()

# 4. norm_data_week6_quiz.1 데이터셋은 남성과 여성 각 100명의 발 크기에 대한 정보를 담고 있습니다. 해당 데이터셋에 대해 남녀의 발 크기를 비교하는 boxplot 형태의 퀵 플롯을 그려주세요. 이때 norm_data_week6_quiz.1에 대해 pivot_longer 함수를 사용하여 gender 열과 foot_size 열을 별도로 만들어주세요.
# 코드와 결과를 함께 제시해주세요.

load("norm_data_week6_quiz.1.rda")

norm_data_week6_quiz.1.plot <- norm_data_week6_quiz.1 %>% 
  pivot_longer(c(female, male), names_to = "gender", values_to = "foot_size") %>%
  arrange(gender) %>%
  qplot(gender, foot_size, data = ., geom = "boxplot")
norm_data_week6_quiz.1.plot

png(filename = "norm_data_week6_quiz.1.plot.png", width = 1000, height = 1000, type = "cairo", 
    antialias = "subpixel", family = "NanumGothic", res = 220)
norm_data_week6_quiz.1.plot
dev.off()
