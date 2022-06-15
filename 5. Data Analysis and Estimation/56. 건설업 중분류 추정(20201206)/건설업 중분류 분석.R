### Code : 건설업 중분류별 요약통계량 및 Plot 작성
### Writer : Donghyeon Kim
### Date : 2020.12.06

## User-Defined Function ##
construct <- function(filename) {
  
  # 1. 패키지 설치
  if(!require(readxl)) {
    install.packages("readxl")
  }
  library(readxl)
  
  if(!require(dplyr)) {
    install.packages("dplyr")
  }
  library(dplyr)
  
  if(!require(ggplot2)) {
    install.packages("ggplot2")
  }
  library(ggplot2)
  
  # 2. 1번째 파일 호출(중분류 내 KSIC 추출 목적)
  dat1 <- read_xlsx(filename, sheet = 5, skip = 1) # 중분류 내에 있는 KSIC 뽑기
  dat1 <- dat1[1:16, ]
  
  # 3. 중분류별 KSIC 추출
  con1 <- as.numeric(dat1$KSIC코드[1:8])
  con2 <- as.numeric(dat1$KSIC코드[9:15])
  con3 <- as.numeric(dat1$KSIC코드[16])
  
  # 4. 2번째 파일 호출(중분류 내 KSIC에 관련된 데이터만 추출하여 매출액, 종사자 분석 목적)
  dat2 <- read_xlsx(filename, sheet = 4, skip = 3)
  dat2 <- dat2 %>% filter(업종 == "건설업")
  
  # 5. 중분류 내 KSIC 관련된 데이터만 추출
    # (1) con1 : 신재생에너지 발전 설비 건설업
    f_data1 <- dat2 %>% filter(표준산업분류코드 %in% con1)
  
    # (2) con2 : 신재생에너지 증기, 냉온수 및 공기조절 설비 건설업
    f_data2 <- dat2 %>% filter(표준산업분류코드 %in% con2)
    
    # (3) con3 : 신재생에너지 연료 제조 설비 건설업
    f_data3 <- dat2 %>% filter(표준산업분류코드 %in% con3)
    
  # 6. 결과물 1, 2 : 각 중분류별 요약통계량 계산(매출액, 종사자수)
  stat_summary <- function(myfile) {
    r_mean <- mean(myfile)
    r_var <- var(myfile)
    r_sd <- sd(myfile)
    r_min <- min(myfile)
    r_q1 <- quantile(myfile)[2]
    r_q2 <- quantile(myfile)[3]
    r_q3 <- quantile(myfile)[4]
    r_max <- max(myfile)
    
    result <- c(r_mean, r_var, r_sd, r_min, r_q1, r_q2, r_q3, r_max)
    names(result) <- c("평균", "분산", "표준편차", "최소값", "Q1", "Q2", "Q3", "최대값")
    
    return(result)
  }
  
    # (1) 매출액
    con1_result1 <<- stat_summary(f_data1$`2018년 신재생에너지 매출액(백만원)`)
    con2_result1 <<- stat_summary(f_data2$`2018년 신재생에너지 매출액(백만원)`)
    con3_result1 <<- stat_summary(f_data3$`2018년 신재생에너지 매출액(백만원)`)
    
    # (2) 종사자 수
    con1_result2 <<- stat_summary(f_data1$`2018년 신재생에너지산업 종사자 수`)
    con2_result2 <<- stat_summary(f_data2$`2018년 신재생에너지산업 종사자 수`)
    con3_result2 <<- stat_summary(f_data3$`2018년 신재생에너지산업 종사자 수`)
    
  # 7. 결과물 3 : Dot Plot
    # (1) 매출액
    con1_result3 <<- ggplot(f_data1, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_dotplot(method = "histodot", binwidth = 700) + ggtitle("발전 설비 건설업_매출액 Dot Plot")
    con2_result3 <<- ggplot(f_data2, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_dotplot(method = "histodot", binwidth = 730) + ggtitle("증기, 냉온수 및 공기조절 설비 건설업_매출액 Dot Plot")
    con3_result3 <<- ggplot(f_data3, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_dotplot(method = "histodot", binwidth = 1700) + ggtitle("연료 제조 설비 건설업_매출액 Dot Plot")
  
    # (2) 종사자 수
    con1_result4 <<- ggplot(f_data1, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_dotplot(method = "histodot", binwidth = 1.1) + ggtitle("발전 설비 건설업_종사자수 Dot Plot")
    con2_result4 <<- ggplot(f_data2, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_dotplot(method = "histodot", binwidth = 1.1) + ggtitle("증기, 냉온수 및 공기조절 설비 건설업_종사자수 Dot Plot")
    con3_result4 <<- ggplot(f_data3, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_dotplot(method = "histodot", binwidth = 1.1) + ggtitle("연료 제조 설비 건설업_종사자수 Dot Plot")
    
  # 8. 결과물 4 : Histogram
    # (1) 매출액
    con1_result5 <<- ggplot(f_data1, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_histogram(binwidth = 700) + ggtitle("발전 설비 건설업_매출액 Histogram")
    con2_result5 <<- ggplot(f_data2, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_histogram(binwidth = 730) + ggtitle("증기, 냉온수 및 공기조절 설비 건설업_매출액 Histogram")
    con3_result5 <<- ggplot(f_data3, aes(x = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_histogram(binwidth = 1700) + ggtitle("연료 제조 설비 건설업_매출액 Histogram")
    
    # (2) 종사자 수
    con1_result6 <<- ggplot(f_data1, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_histogram(binwidth = 1.1) + ggtitle("발전 설비 건설업_종사자수 Histogram")
    con2_result6 <<- ggplot(f_data2, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_histogram(binwidth = 1.1) + ggtitle("증기, 냉온수 및 공기조절 설비 건설업_종사자수 Histogram")
    con3_result6 <<- ggplot(f_data3, aes(x = `2018년 신재생에너지산업 종사자 수`)) +
      geom_histogram(binwidth = 1.1) + ggtitle("연료 제조 설비 건설업_종사자수 Histogram")
    
  # 9. 결과물 5 : 종사자수 vs 매출액 Scatter Plot
    con1_result7 <<- ggplot(f_data1, aes(x = `2018년 신재생에너지산업 종사자 수`, y = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_point(size = 2, colour = "red") + ggtitle("발전 설비 건설업_종사자수 대비 매출액 Scatter Plot")
    con2_result7 <<- ggplot(f_data2, aes(x = `2018년 신재생에너지산업 종사자 수`, y = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_point(size = 2, colour = "blue") + ggtitle("증기, 냉온수 및 공기조절 설비 건설업_종사자수 대비 매출액 Scatter Plot")
    con3_result7 <<- ggplot(f_data3, aes(x = `2018년 신재생에너지산업 종사자 수`, y = `2018년 신재생에너지 매출액(백만원)`)) +
      geom_point(size = 2, colour = "hot pink") + ggtitle("연료 제조 설비 건설_종사자수 대비 매출액 Scatter Plot")
    
  # 10. 결과물 6 : 종사자수 vs 매출액 상관계수
    con1_result8 <<- cor(f_data1$`2018년 신재생에너지산업 종사자 수`, f_data1$`2018년 신재생에너지 매출액(백만원)`)
    con2_result8 <<- cor(f_data2$`2018년 신재생에너지산업 종사자 수`, f_data2$`2018년 신재생에너지 매출액(백만원)`)
    con3_result8 <<- cor(f_data3$`2018년 신재생에너지산업 종사자 수`, f_data3$`2018년 신재생에너지 매출액(백만원)`)
    
  # 11. Sample 개수
    con1_result9 <<- nrow(f_data1)
    con2_result9 <<- nrow(f_data2)
    con3_result9 <<- nrow(f_data3)
    
  ## 결과물 출력
    # con1(발전 설비 건설업) : con1_result1 ~ con1_result9
    # con2(증기, 냉온수 및 공기조절 설비 건설업) : con2_result1 ~ con2_result9
    # con3(연료 제조 설비 건설업) : con3_result1 ~ con3_result9
}


## Function Execution ##
# 1. 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop")
setwd(foldname)

# 2. 파일명 설정
filename <- "완료_2019년도 신재생에너지 전후방산업_본조사 data_(최종)_건설업 중분류 구분.xlsx"

# 3. 함수 실행
construct(filename)


## Result Check ##
# 1. 중분류 1 : 발전 설비 건설업
# (1) 매출액 요약통계량
con1_result1

# (2) 종사자수 요약통계량
con1_result2

# (3) 매출액 Dot Plot
con1_result3

# (4) 종사자수 Dot Plot
con1_result4

# (5) 매출액 Histogram
con1_result5

# (6) 종사자수 Histogram
con1_result6

# (7) 종사자수 vs 매출액 Scatter Plot
con1_result7

# (8) 종사자수 vs 매출액 상관계수
con1_result8

# (9) Sample 개수
con1_result9

#########################################

# 2. 중분류 2 : 증기, 냉온수 및 공기조절 설비 건설업
# (1) 매출액 요약통계량
con2_result1

# (2) 종사자수 요약통계량
con2_result2

# (3) 매출액 Dot Plot
con2_result3

# (4) 종사자수 Dot Plot
con2_result4

# (5) 매출액 Histogram
con2_result5

# (6) 종사자수 Histogram
con2_result6

# (7) 종사자수 vs 매출액 Scatter Plot
con2_result7

# (8) 종사자수 vs 매출액 상관계수
con2_result8

# (9) Sample 개수
con2_result9

#########################################

# 3. 중분류 3 : 연료 제조 설비 건설업
# (1) 매출액 요약통계량
con3_result1

# (2) 종사자수 요약통계량
con3_result2

# (3) 매출액 Dot Plot
con3_result3

# (4) 종사자수 Dot Plot
con3_result4

# (5) 매출액 Histogram
con3_result5

# (6) 종사자수 Histogram
con3_result6

# (7) 종사자수 vs 매출액 Scatter Plot
con3_result7

# (8) 종사자수 vs 매출액 상관계수
con3_result8

# (9) Sample 개수
con3_result9
