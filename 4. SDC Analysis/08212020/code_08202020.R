### Code : 대분류, 에너지원, KSIC, 종사자 규모별 사업체수
### Encoding Option : UTF-8
### Writer : Donghyeon Kim
### Date : 2020.08.20

## 1. 파일 호출하기
library(readxl)
mydata <- read_xlsx("(접수번호995) 반입자료(신재생에너지 사업 영위 기업 리스트)_200729.xlsx", sheet = 1) # tibble

## 2. 대분류, 에너지원 변수 설정
dae <- unique(mydata$구분1) # 대분류
energy <- unique(mydata$구분2) # 에너지원

## 3. 사업체수 구하기(2번의 변수 활용, 종사자 규모 구분)
result_data <- NULL # 결과물을 저장하기 위한 변수

for(i in 1:length(dae)) {
  for(j in 1:length(energy)) {
    temp <- subset(mydata, mydata$구분1 == dae[i] & mydata$구분2 == energy[j]) # 대분류, 에너지원 필터링
    
    ksic <- unique(temp$BR_SNB_CD) # 필터링한 데이터의 ksic(중복 제거)
    
    # 종사자 규모별 구분
    for(k in 1:length(ksic)) {
      comp1 <- subset(temp, temp$BR_SNB_CD == ksic[k] & temp$BR_EMP_MF_T < 5) # 5인미만
      comp2 <- subset(temp, temp$BR_SNB_CD == ksic[k] & temp$BR_EMP_MF_T >= 5 & temp$BR_EMP_MF_T < 10) # 10인미만
      comp3 <- subset(temp, temp$BR_SNB_CD == ksic[k] & temp$BR_EMP_MF_T >= 10 & temp$BR_EMP_MF_T < 50) # 50인미만
      comp4 <- subset(temp, temp$BR_SNB_CD == ksic[k] & temp$BR_EMP_MF_T >= 50) # 50인이상
      
      # 데이터 저장
      imsi_result <- data.frame(matrix(NA, ncol = 8)) # 임시로 결과물을 저장할 변수
      names(imsi_result) <- c("대분류", "에너지원", "KSIC", "총사업체수", "5인미만", "10인미만", "50인미만", "50인이상")
      
      imsi_result[k, 1] <- dae[i]
      imsi_result[k, 2] <- energy[j]
      imsi_result[k, 3] <- ksic[k]
      imsi_result[k, 4] <- sum(c(nrow(comp1), nrow(comp2), nrow(comp3), nrow(comp4)))
      imsi_result[k, 5] <- nrow(comp1)
      imsi_result[k, 6] <- nrow(comp2)
      imsi_result[k, 7] <- nrow(comp3)
      imsi_result[k, 8] <- nrow(comp4)
      
      # 데이터 프레임 결합
      result_data <- rbind(result_data, imsi_result)
      
      # 원활한 반복 작업을 위한 변수 제거
      rm(list = c("comp1", "comp2", "comp3", "comp4", "imsi_result"))
    }
    
    # 원활한 반복 작업을 위한 변수 제거
    rm(list = c("temp", "ksic"))
  }
  
  # NA 제거
  result_data <- result_data[!is.na(result_data$대분류), ]
}

## 4. 엑셀 파일 쓰기(xlsx)
# 대분류별로 시트 따로 작성함.

# 대분류 분류(제조업, 건설업, 공급업, 서비스업)
d1 <- subset(result_data, result_data$대분류 == dae[1])
d2 <- subset(result_data, result_data$대분류 == dae[2])
d3 <- subset(result_data, result_data$대분류 == dae[3])
d4 <- subset(result_data, result_data$대분류 == dae[4])

# 엑셀 파일 작성하기
library(writexl)
write_xlsx(list("제조업" = d1, "건설업" = d2, "공급업" = d3, "서비스업" = d4), "대분류, 에너지원, KSIC, 종사자 규모별 사업체수.xlsx")
