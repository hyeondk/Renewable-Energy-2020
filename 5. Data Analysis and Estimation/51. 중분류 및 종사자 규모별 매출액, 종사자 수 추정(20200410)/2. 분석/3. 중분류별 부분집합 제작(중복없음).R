### Code 3-1 : 중분류별 부분집합 제작(중복없음)
### Writer : 김동현
### Date : 2020.04.08

# 1. 적격률 추정
# 2. 산업 사업체 수(모집단) 추정
# 3. 매출액 추정
# 4. 총 매출액 추정

### 중복되지 않는 KSIC 파일 호출
setwd("C:/Users/mazy4/Desktop")
getwd()

중분류별_KSIC_count_중복없음 <- read.csv("중분류별_KSIC_count_중복없음2.csv")

# 중분류 종류 확인
unique(중분류별_KSIC_count_중복없음$중)

# 중분류별로 부분집합 만들기
중분류별_부분집합 <- list()

for(i in unique(중분류별_KSIC_count_중복없음$중)) {
  중분류별_부분집합[[i]] <- subset(중분류별_KSIC_count_중복없음, 중분류별_KSIC_count_중복없음$중 == i)
}

# Excel 파일 제작
for(i in 1:length(unique(중분류별_KSIC_count_중복없음$중))) {
  write.csv(중분류별_부분집합[[i]], file = paste0("중분류별_부분집합", i, ".csv"))
}
