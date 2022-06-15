### Code : 신재생에너지 사업 영위 리스트 KSIC, 종사자 규모별 데이터 형태 파악
### Encoding Option : UTF-8
### Writer : Donghyeon Kim
### Date : 2020.08.12
### Place : Seoul Bigdata Center

## 1. 분석시 사용할 파일을 Upload하기
# R을 서버 상에서 이용해야 하므로 작업공간 설정 불가함.


## 2. 파일 호출하기
library(readxl)
mydata <- read_xlsx("(접수번호995) 반입자료(신재생에너지 사업 영위 기업 리스트)_200729.xlsx", sheet = 1) # tibble


## 3. KSIC 오름차순으로 정렬하기
# KSIC(한국표준산업분류) : BR_SNB_CD
idx <- order(mydata$BR_SNB_CD) # index
mydata_s <- mydata[idx, ] # sorting by using index


## 4-1. KSIC 2, 3, 4자리 데이터셋만 추출
mydata_f1 <- subset(mydata_s, nchar(mydata_s$BR_SNB_CD) < 5)

## 4-2. KSIC 5자리 데이터셋만 추출
mydata_f2 <- subset(mydata_s, !(nchar(mydata_s$BR_SNB_CD) < 5))


## 5. KSIC, 종사자 수 별로 사업체 수 확인 및 데이터 저장
# Case 1(KSIC이 5자리가 아닌 경우)와 Case 2(KSIC이 5자리인 경우)로 구분하여 반복작업 시행
# 종사자수(남녀총합) : BR_EMP_MF_T

result_case1 <- data.frame(matrix(NA, ncol = 7))
result_case2 <- data.frame(matrix(NA, ncol = 7))

names(result_case1) <- c("KSIC", "총사업체수", "5인미만", "10인미만", "50인미만", "50인이상", "종사자규모별합")
names(result_case2) <- c("KSIC", "총사업체수", "5인미만", "10인미만", "50인미만", "50인이상", "종사자규모별합")

for(c in 1:2) {
  if(c == 1) {
    for(i in 1:length(unique(mydata_f1$BR_SNB_CD))) {
      # 해당 KSIC 데이터셋만 추출
      temp <- subset(mydata_f1, mydata_f1$BR_SNB_CD == unique(mydata_f1$BR_SNB_CD)[i])
      
      # 종사자 규모별로 구분
      comp_under5 <- subset(temp, temp$BR_EMP_MF_T < 5) # 5인미만
      comp_under10 <- subset(temp, temp$BR_EMP_MF_T >= 5 & temp$BR_EMP_MF_T < 10) # 10인미만
      comp_under50 <- subset(temp, temp$BR_EMP_MF_T >= 10 & temp$BR_EMP_MF_T < 50) # 50인미만
      comp_above50 <- subset(temp, temp$BR_EMP_MF_T >= 50) # 50인이상
      
      # 데이터 저장
      result_case1[i, 1] <- unique(mydata_f1$BR_SNB_CD)[i]
      result_case1[i, 2] <- nrow(temp)
      result_case1[i, 3] <- nrow(comp_under5)
      result_case1[i, 4] <- nrow(comp_under10)
      result_case1[i, 5] <- nrow(comp_under50)
      result_case1[i, 6] <- nrow(comp_above50)
      result_case1[i, 7] <- sum(result_case1[i, 3:6])
      
      # 수월한 반복작업을 위한 변수명 제거
      rm(list = c("temp", "comp_under5", "comp_under10", "comp_under50", "comp_above50"))
    }
  } else {
    for(j in 1:length(unique(mydata_f2$BR_SNB_CD))) {
      # 해당 KSIC 데이터셋만 추출
      temp <- subset(mydata_f2, mydata_f2$BR_SNB_CD == unique(mydata_f2$BR_SNB_CD)[j])
      
      # 종사자 규모별로 구분
      comp_under5 <- subset(temp, temp$BR_EMP_MF_T < 5) # 5인미만
      comp_under10 <- subset(temp, temp$BR_EMP_MF_T >= 5 & temp$BR_EMP_MF_T < 10) # 10인미만
      comp_under50 <- subset(temp, temp$BR_EMP_MF_T >= 10 & temp$BR_EMP_MF_T < 50) # 50인미만
      comp_above50 <- subset(temp, temp$BR_EMP_MF_T >= 50) # 50인이상
      
      # 데이터 저장
      result_case2[j, 1] <- unique(mydata_f2$BR_SNB_CD)[j]
      result_case2[j, 2] <- nrow(temp)
      result_case2[j, 3] <- nrow(comp_under5)
      result_case2[j, 4] <- nrow(comp_under10)
      result_case2[j, 5] <- nrow(comp_under50)
      result_case2[j, 6] <- nrow(comp_above50)
      result_case2[j, 7] <- sum(result_case2[j, 3:6])
      
      # 수월한 반복작업을 위한 변수명 제거
      rm(list = c("temp", "comp_under5", "comp_under10", "comp_under50", "comp_above50"))
    }
  }
}



## 6. 엑셀 파일로 저장(확장자 : xlsx)
# openxlsx package 사용 불가능하므로 writexl package 사용함.
library(writexl)
write_xlsx(list("KSIC(2~4자리)" = result_case1, "KSIC(5자리)" = result_case2), "KSIC, 종사자 규모별 데이터 형태.xlsx")
