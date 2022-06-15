### Code 1 : 중복되는 KSIC과 그렇지 않은 KSIC 구분 후 csv file 제작
### Writer : 김동현
### Date : 2020.04.08


# 라이브러리 호출
library(kableExtra)
library(tidyverse)
library(readxl)

# 작업공간 설정
setwd("C:/Users/mazy4/Desktop")
getwd()

산업분류 <- read_xlsx("신재생에너지 산업 특수분류체계 연계 KSIC(9차-10차)_20200323.xlsx", skip = 1)
names(산업분류) <- c("대","중","소","세","세세_9차","KSIC코드_9차","세세","KSIC코드")

# NA 제거
산업분류 <- 산업분류[-dim(산업분류)[1], ]
산업분류 <- 산업분류[, -c(9, 10)]

# 각 NA에 이름 할당
for(i in 1:(dim(산업분류)[1])){
  if(is.na(산업분류$대[i])){
    산업분류$대[i] <- 산업분류$대[i-1]}
  if(is.na(산업분류$중[i])){
    산업분류$중[i] <- 산업분류$중[i-1]}
  if(is.na(산업분류$세세_9차[i])){
    산업분류$세세_9차[i] <- 산업분류$세세_9차[i-1]}
  if(is.na(산업분류$KSIC코드_9차[i])){
    산업분류$KSIC코드_9차[i] <- 산업분류$KSIC코드_9차[i-1]}
}

# 소분류, 세분류 제거
산업분류 <- 산업분류[, -c(3, 4)]

# 산업분류 데이터 확인

# 같은 중분류 내에 KSIC이 몇 번 중복되는가?
중분류별_KSIC_count <- 산업분류 %>% group_by(., 대, 중, KSIC코드) %>% count()

# 중복X, 중복O 구분하여 데이터 제작
중분류별_KSIC_count_수정 <- 중분류별_KSIC_count[!(중분류별_KSIC_count$n > 1), ]
중분류별_KSIC_count_중복 <- 중분류별_KSIC_count[중분류별_KSIC_count$n > 1, ]

# 중분류별_KSIC_count_수정, 중분류별_KSIC_count_중복 확인

# 중분류별_KSIC_count_수정 -> Excel 파일 제작
write.csv(중분류별_KSIC_count_수정, file = "중분류별_KSIC_count_중복없음.csv")
write.csv(중분류별_KSIC_count_중복, file = "중분류별_KSIC_count_중복.csv")
