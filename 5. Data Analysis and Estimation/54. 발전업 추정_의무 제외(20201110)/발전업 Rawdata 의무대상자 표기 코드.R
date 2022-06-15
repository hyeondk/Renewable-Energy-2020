### Code : 발전업 rawdata 의무대상자 여부 표기
### Writer : Donghyun Kim
### Date : 2020.11.10.

## 1. Set Working Directory
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop")
setwd(foldname)

## 2. 파일 호출하기
library(readxl)
raw1 <- read_xlsx("발전업2020(표본추출).xlsx", sheet = 1)
raw1 <- raw1[, -c(1, ncol(raw1))]

raw2 <- read_xlsx("발전업2020(표본추출).xlsx", sheet = 2, skip = 3)

## 3. 데이터 결합
temp <- merge(raw1, raw2, all.x = T)
View(temp)

## 4. 파일 저장
library(writexl)
write_xlsx(temp, "발전업2020(표본추출)_수정.xlsx")
