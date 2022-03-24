library(tidyverse)

# 문제 1.
6/3 + 11%/%3 # 5

# 문제 2.
c(6, 7, 8, 9) >= 6.5 # FALSE  TRUE  TRUE  TRUE

# 문제 3.
x <-  "5.23"
is.numeric(x) # FALSE

# 문제 4.
"s" == "S" # FALSE

# 문제 5.
x <- "2.5"
as.numeric(x) # 숫자 2.5로 변환.

# 문제 6.
my_vector <- c(1, 2, "삼", "사") # "1"  "2"  "삼" "사" <== 문자로 바뀜. 벡터는 같은 자료형의 객체로만 구성되기 때문.

# 문제 7.
my_mat <- matrix(c("a", "b", "c", "d", "e", "f", "g", "h"), # 매트릭스 구성요소
            nrow = 4,            # 행의 수
            ncol = 2,            # 열의 수
            byrow = T)           # 행의 방향으로 매트릭스를 채울 것인가? 그렇다면 T, 아니면 F

# 문제 8.
ID <- c("ID_1", "ID_2", "ID_3", "ID_4")
major <- c("국문과", "영문과", "중문과", "심리학과")
score <- c(80, 50, 60, 90)
my_tibble <- tibble(ID, major, score)

# 문제 9.
nrow(iris) # 150
ncol(iris) # 5

# 문제 10.
colnames(iris) <- c("꽃받침_길이", "꽃받침_너비", "꽃잎_길이", "꽃잎_너비", "꽃의_종류")
iris_korean <- iris



