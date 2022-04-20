library(tidyverse) 
library(dslabs)
options(tibble.width = Inf)

# 문제 1.
# 괄호에 들어갈 용어들이 알맞게 짝지어진 것을 고르세요.

# 각 변수는 고유의 (열)을, 각 관찰값은 고유의 (행)을, 개별값은 고유의 (셀)을 가진다.
# 1. 열, 셀, 행
# 2. 행, 열, 셀
# 3. 셀, 행, 열
# 4. 열, 행, 셀 => 정답.

# 문제 2.

# 1. 본 수업용 깃허브에서 "wide_data.rda" 파일을 프로젝트 폴더로 다운받은 뒤 로딩해주세요.
# 2. wide_data 데이터는 `1960`부터 `2015` 열까지 총 57개 열로 구성되어 있습니다. 새로운 열을 생성하여 "year"라는 변수에 기존 열 이름을 넣어주세요.
# 3. 새로운 열을 생성하여 "fertility"라는 변수에 관찰값을 넣어주세요.
# 코드와 결과를 함께 제시해주세요.

load("wide_data.rda")
wide_data %>%
  pivot_longer(`1960`:`2015`, names_to = "year", values_to = "fertility")

wide_data %>% 
  pivot_longer(-country, names_to="year", values_to="fertility")

# 문제 3.

# 1. relig_income 데이터를 콘솔에 띄워 확인해보세요. tidyr 패키지에 포함되어 있으므로 문제 2처럼 파일을 직접 로딩할 필요는 없습니다. 
# 2. 첫 번째 열 religion을 제외한 나머지 모든 열 이름을 "income"이라는 변수에 넣어주세요.
# 3. 새로운 열을 또 하나 생성하여 "frequency"라는 변수에 관찰값을 넣어주세요.
# 코드와 결과를 함께 제시해주세요.

relig_income %>%
  pivot_longer(`<$10k`:`Don't know/refused`, names_to = "income", values_to = "frequency")

relig_income %>%
  pivot_longer(-religion, names_to = "income", values_to = "frequency")

# 문제 4.

# 1. Orange 데이터를 콘솔에 띄워 확인해보세요. 
# 2. Tree 열에 포함된 개별값을 변수로 만들어주세요.
# 3. circumference 열로부터 개별값을 취해주세요.
# 코드와 결과를 함께 제시해주세요.

Orange %>%
  pivot_wider(names_from = "Tree", values_from = "circumference")

# 문제 5.

# 1. data(starwars)를 실행한 뒤 starwars 데이터를 콘솔에 띄워 확인해보세요. 
# 2. gender가 NA가 아닌 경우만 필터링해 남겨주세요. 
# 3. gender 열에 포함된 개별값을 변수로 만들어주세요.
# 4. height, mass, birth_year 열로부터 개별값을 취해주세요.
# 코드와 결과를 함께 제시해주세요.

data(starwars)
starwars %>%
  filter(is.na(gender) == F) %>% # filter(!is.na(gender)) 또한 OK.
  pivot_wider(names_from = "gender", values_from = c("height", "mass", "birth_year"))

# 문제 6.

# 1. 본 수업용 깃허브에서 "dat_quiz_w5.rda" 파일을 프로젝트 폴더로 다운받은 뒤 로딩해주세요.
# 2. name 변수를 "_"를 기준으로 "year"와 "name" 변수로 분리해주세요.
# 3. 정제된 최종 데이터를 new_dat_quiz_w5라는 변수명에 할당해주세요.
# 코드와 결과를 함께 제시해주세요.

load("dat_quiz_w5.rda")
new_dat_quiz_w5 <- dat_quiz_w5 %>% 
  separate(name, c("year", "name"), sep = "_")

# 문제 7.

# 1. 본 수업용 깃허브에서 "dat_quiz_w5.1.rda" 파일을 프로젝트 폴더로 다운받은 뒤 로딩해주세요.
# 2. name1과 name2 변수를 "-"를 매개로 합친 뒤 name이라는 변수에 넣어주세요.
# 3. 정제된 최종 데이터를 new_dat_quiz_w5.1이라는 변수명에 할당해주세요.
# 코드와 결과를 함께 제시해주세요.

load("dat_quiz_w5.1.rda")
new_dat_quiz_w5.1 <- dat_quiz_w5.1 %>%
  unite(name, name1, name2, sep = "-")
  
# 문제 8.

# 1. billboard 데이터를 콘솔에 띄워 확인해보세요. 
# 2. 첫 번째 열부터 일곱 번째 열까지만 서브세팅해주세요. 
# 3. wk1부터 wk4까지 네 열 이름을 "week"라는 변수에 넣어주세요.
# 4. 새로운 열을 또 하나 생성하여 "rank"라는 변수에 관찰값을 넣어주세요.
# 5. 명시적 결측값을 제외해주세요.
# 코드와 결과를 함께 제시해주세요.

billboard[, 1:7] %>%
  pivot_longer(wk1:wk4, names_to = "week", values_to = "rank",
               values_drop_na = T)
  
# 문제 9.

# 1. 본 수업용 깃허브에서 "tab_1.rda", "tab_2.rda", "tab_3.rda" 파일을 프로젝트 폴더로 다운받은 뒤 로딩해주세요.
# 2. left_join 함수를 사용하여 state 열을 매개로 tab_1과 tab_2 데이터를 연결해주세요.
# 3. right_join 함수를 사용하여 state 열을 매개로 tab_1과 tab_2 데이터를 연결해주세요.
# 4. inner_join 함수를 사용하여 state 및 state_name 열을 매개로 tab_1과 tab_3 데이터를 연결해주세요.
# 5. full_join 함수를 사용하여 state 및 state_name 열을 매개로 tab_1과 tab_3 데이터를 연결해주세요.
# 코드와 결과를 함께 제시해주세요.

load("tab_1.rda")
load("tab_2.rda")
load("tab_3.rda")

tab_1 %>% 
  left_join(tab_2, by = "state")

tab_1 %>%
  right_join(tab_2, by = "state")

tab_1 %>%
  inner_join(tab_3, by = c("state" = "state_name"))

tab_1 %>%
  full_join(tab_3, by = c("state" = "state_name"))

# 문제 10.

# 1. nycflights13 패키지를 작업공간으로 불러오세요.
# 2. semi_join 함수를 사용하여 "dest" 및 "faa" 열을 매개로 flights와 airports 데이터를 연결해주세요.
# 3. anti_join 함수를 사용하여 "faa" 및 "dest" 열을 매개로 airports와 filghts 데이터를 연결해주세요.
# 코드와 결과를 함께 제시해주세요.

library(nycflights13)

flights %>%
  semi_join(airports, by = c("dest" = "faa"))
  
airports %>%
  anti_join(flights, by = c("faa" = "dest"))

