### Code : 중복 KSIC에 대한 비중을 종사자 수에 부여하고자 하는 코드
### Writer : 김동현

library(knitr)
library(kableExtra)
library(tidyverse)
library(readxl)
library(ggplot2)
library(doBy)

## 산업분류표 만들기
산업분류 <- read_xlsx("C:/Users/mazy4/Desktop/소분류별_상위기업제외/data/신재생에너지 산업 특수분류체계 연계 KSIC(9차-10차)_E3_20200323(del).xlsx", skip = 1)
산업분류 <- 산업분류[, -c(9, 10)] # 비고 항 제거
산업분류 <- 산업분류[-nrow(산업분류), ] # 마지막 행 제거
names(산업분류) <- c("대","중","소","세","세세_9차","KSIC코드_9차","세세","KSIC코드")

## NA 부분 모두 밀어서 이름 부여
for(i in 1:(dim(산업분류)[1])){
  if(is.na(산업분류$대[i])){
    산업분류$대[i] <- 산업분류$대[i-1]}
  if(is.na(산업분류$중[i])){
    산업분류$중[i] <- 산업분류$중[i-1]}
  if(is.na(산업분류$소[i])){
    산업분류$소[i] <- 산업분류$소[i-1]}
  if(is.na(산업분류$세[i])){
    산업분류$세[i] <- 산업분류$세[i-1]}
  if(is.na(산업분류$세세_9차[i])){
    산업분류$세세_9차[i] <- 산업분류$세세_9차[i-1]}
  if(is.na(산업분류$KSIC코드_9차[i])){
    산업분류$KSIC코드_9차[i] <- 산업분류$KSIC코드_9차[i-1]}
}

write_file_name <- "산업분류표_대기업포함.csv"
write_csv(산업분류,paste("./",write_file_name,sep=""))

## 산업분류 -> 소분류까지만 살리고 세분류, 세세_9차 지워버림
산업분류 <- 산업분류[, -c(4:6)]

## 같은 소분류 내에 있는 KSIC은 모두 1개로 처리
소분류별_KSIC_count <- NULL
for(i in unique(산업분류$소)) {
  소분류별_KSIC_count <- rbind(소분류별_KSIC_count, unique(산업분류[산업분류$소 == i, ]))
}

######################################################################################

## 같은 KSIC이 몇 개의 서로 다른 소분류에 중복되는지 확인
KSIC_count_비중 <- 소분류별_KSIC_count %>% group_by(., KSIC코드) %>% count
KSIC중복 <- KSIC_count_비중[KSIC_count_비중$n > 1, ]
중복_KSIC_data <- 산업분류[산업분류$KSIC코드 %in% KSIC중복$KSIC코드, ]
중복_KSIC_data <- unique(orderBy(~ KSIC코드, 중복_KSIC_data))

## 매출액 표본 싹 다 끌어오기
조사결과 <- read_xlsx("C:/Users/mazy4/Desktop/소분류별_상위기업제외/data/완료_2019년도 신재생에너지 전후방산업_본조사 data_0228(745)_17년도 실적 매칭 + 제조 상위기업.xlsx",sheet=2,skip=3)
proc_조사결과 <- 조사결과[,c(2,4,7,8,14)]
names(proc_조사결과) <- c("상위업체", "업체명", "KSIC코드", "종사자규모", "총_종사자수_2018")

## 중복_KSIC_data와 proc_조사결과 data merge -> 각 KSIC에 해당되는 매출액 확인 가능
중복_KSIC_data_종사자 <- 중복_KSIC_data %>% merge(., proc_조사결과, all.x = T)

##################################################################################################

## by 이재용 - 중복된 KSIC에 대한 종사자 표본 합 표1 제작
중복_KSIC_data_종사자_표1 <- data.frame()
j <- 1
for(i in unique(중복_KSIC_data_종사자$KSIC코드)) {
  temp <- subset(중복_KSIC_data_종사자, 중복_KSIC_data_종사자$KSIC코드 == i)
  temp1 <- unique(temp$소)
  
  for(a in temp1) {
    중복_KSIC_data_종사자_표1[j, "KSIC코드"] <- i
    중복_KSIC_data_종사자_표1[j, "대분류"] <- temp[temp$소 == a,"대"][1]
    if(length(unique(temp[temp$소 == a,"대"])) > 2){
      print("Discover the error!")
    }
    중복_KSIC_data_종사자_표1[j, "중분류"] <- temp[temp$소 == a,"중"][1]
    if(length(unique(temp[temp$소 == a,"중"])) > 2){
      print("Discover the error!")
    }
    중복_KSIC_data_종사자_표1[j, "소분류"] <- a
    중복_KSIC_data_종사자_표1[j, "표본수"] <- sum(temp$소 == a)
    중복_KSIC_data_종사자_표1[j, "종사자수"] <- sum(temp$총_종사자수_2018[temp$소 == a],na.rm = T)
    j <- j + 1
  }
}

## 임시 변수 제거
rm(temp)
rm(temp1)
rm(a)
rm(i)
rm(j)

########################################################################################################################

## 에너지원 비중 파일 불러들이기
에너지원_비중 <- read_xlsx("C:/Users/mazy4/Desktop/소분류별_상위기업제외/data/2018 신재생에너지 신규 보급 비중.xlsx")[1:11, 6:8]
colnames(에너지원_비중) <- c("보급용량전체", "용량", "비중")

## 같은 KSIC이 서로 다른 소분류에 중복되는 것들만 비중 구해주기(매출액, 종사자 수에 반영하기 위함)
# 1. 보급종류, 비중 부여
중복_KSIC_data_종사자_표1$보급종류 <- NA
중복_KSIC_data_종사자_표1$비중 <- 0
for(i in 1:dim(중복_KSIC_data_종사자_표1)[1]) {
  if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "태양") {
    if(substr(중복_KSIC_data_종사자_표1[i, 4], 7, 8) != "온수") {
      중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[2, 1]
      중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[2, 3]
    } else {
      중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[3, 1]
      중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[3, 3]
    }
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "풍력") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[4, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[4, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "수력") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[5, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[5, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "해양") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[6, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[6, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "바이") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[7, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[7, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "폐기") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[8, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[8, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "연료") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[9, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[9, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "석탄") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[10, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[10, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "지열") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[11, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[11, 3]
  } else if(substr(중복_KSIC_data_종사자_표1[i, 4], 1, 2) == "수열") {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- 에너지원_비중[12, 1]
    중복_KSIC_data_종사자_표1[i, "비중"] <- 에너지원_비중[12, 3]
  } else {
    중복_KSIC_data_종사자_표1[i, "보급종류"] <- NA
    중복_KSIC_data_종사자_표1[i, "비중"] <- 0
  }
}

# 2. 각 KSIC 별로 비중 재계산
KSIC_비중합 <- NULL
비중합_반복부여횟수 <- KSIC중복$n
for(i in 1:length(unique(중복_KSIC_data_종사자_표1$KSIC코드))) {
  KSIC_비중합 <- c(KSIC_비중합, rep(sum(중복_KSIC_data_종사자_표1[중복_KSIC_data_종사자_표1$KSIC코드 == unique(중복_KSIC_data_종사자_표1$KSIC코드)[i], "비중"]), 비중합_반복부여횟수[i]))
}

중복_KSIC_data_종사자_표1$비중합 <- KSIC_비중합
중복_KSIC_data_종사자_표1$부여비중 <- 중복_KSIC_data_종사자_표1$비중 / 중복_KSIC_data_종사자_표1$비중합

View(중복_KSIC_data_종사자_표1)

## 매출액에 비중 곱하기
중복_KSIC_data_종사자_표1$종사자수_수정 <- 중복_KSIC_data_종사자_표1$종사자수 * 중복_KSIC_data_종사자_표1$부여비중

## 파일 내보내기
write_file_name <- "중복_KSIC_data_종사자_표1.csv"
write_csv(중복_KSIC_data_종사자_표1, paste("./", write_file_name, sep=""))
