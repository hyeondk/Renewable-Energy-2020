### Code : 발전업 실제 발전량 데이터 - 모집단 정보
### Writer : Donghyun Kim
### Date : 2021.01.17

## Module 1 : 모집단 정보 수집 ##
my_summary1 <- function(filename) {
  
  # 1. 패키지 설치
  if(!require(readxl)) {
    install.packages("readxl")
  }
  
  if(!require(dplyr)) {
    install.packages("dplyr")
  }
  
  if(!require(ggplot2)) {
    install.packages("ggplot2")
  }
  
  # 2. 데이터 호출(모집단 정보)
  dat1 <- read_xlsx(filename, sheet = 2, skip = 3)
  dat1 <- dat1[, 1:10]
  
    # (1) 변수 이름 재설정
    names(dat1)[1] <- "사업구분"
    names(dat1)[3] <- "설비용량"
    names(dat1)[4] <- "설비코드"
    names(dat1)[7] <- "전_혼소_구분_2019년_기준"
    names(dat1)[8] <- "REC_발급_발전량_2019년_기준"
    
    # (2) 의무대상자, 표본조사 부분 : NA -> 0으로 수정
    dat1$의무대상자 <- replace(dat1$의무대상자, is.na(dat1$의무대상자), 0)
    dat1$표본조사 <- replace(dat1$표본조사, is.na(dat1$표본조사), 0)
  
  # 3. 각 에너지원별 모집단 정보
    # (1) 요약통계량 함수
    my_stat_summary <- function(myfile) {
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
    
    # (2) 에너지원 구분
    dat1_energy <- unique(dat1$에너지원)
    
      # (2-1) 에너지원1 : LFG
      f_data1 <- dat1 %>% filter(에너지원 == dat1_energy[1])

      # (2-2) 에너지원2 : 바이오가스
      f_data2 <- dat1 %>% filter(에너지원 == dat1_energy[2])
      
      # (2-3) 에너지원3 : 바이오매스
      f_data3 <- dat1 %>% filter(에너지원 == dat1_energy[3])
      
      # (2-4) 에너지원4 : 폐기물
      f_data4 <- dat1 %>% filter(에너지원 == dat1_energy[4])
      
      # (2-5) 에너지원5 : 바이오
      f_data5 <- dat1 %>% filter(에너지원 == dat1_energy[5])
    
    # (3) 결과 볼 수 있는 변수
    result1 <<- vector(mode = "list", length = 3) # LFG
    result2 <<- vector(mode = "list", length = 3) # 바이오가스
    result3 <<- vector(mode = "list", length = 3) # 바이오매스
    result4 <<- vector(mode = "list", length = 3) # 폐기물
    result5 <<- vector(mode = "list", length = 3) # 바이오
    
    names(result1) <<- c("모집단_사업체_수", "설비용량_요약통계량", "REC_발전량_요약통계량")
    names(result2) <<- c("모집단_사업체_수", "설비용량_요약통계량", "REC_발전량_요약통계량")
    names(result3) <<- c("모집단_사업체_수", "설비용량_요약통계량", "REC_발전량_요약통계량")
    names(result4) <<- c("모집단_사업체_수", "설비용량_요약통계량", "REC_발전량_요약통계량")
    names(result5) <<- c("모집단_사업체_수", "설비용량_요약통계량", "REC_발전량_요약통계량")
    
    # (4) 에너지원 별 사업체 수 / 설비 용량 요약통계량 / REC 발전량 요약통계량
      # (4-1) 에너지원 별 사업체 수
      result1[[1]] <<- nrow(f_data1)
      result2[[1]] <<- nrow(f_data2)
      result3[[1]] <<- nrow(f_data3)
      result4[[1]] <<- nrow(f_data4)
      result5[[1]] <<- nrow(f_data5)
      
      # (4-2) 설비 용량 요약통계량
      result1[[2]] <<- my_stat_summary(f_data1$설비용량)
      result2[[2]] <<- my_stat_summary(f_data2$설비용량)
      result3[[2]] <<- my_stat_summary(f_data3$설비용량)
      result4[[2]] <<- my_stat_summary(f_data4$설비용량)
      result5[[2]] <<- my_stat_summary(f_data5$설비용량)
      
      # (4-3) REC 발전량 요약통계량
      result1[[3]] <<- my_stat_summary(f_data1$REC_발급_발전량_2019년_기준)
      result2[[3]] <<- my_stat_summary(f_data2$REC_발급_발전량_2019년_기준)
      result3[[3]] <<- my_stat_summary(f_data3$REC_발급_발전량_2019년_기준)
      result4[[3]] <<- my_stat_summary(f_data4$REC_발급_발전량_2019년_기준)
      result5[[3]] <<- my_stat_summary(f_data5$REC_발급_발전량_2019년_기준)
}

## Function Execution ##
# 1. 디렉토리 설정
dr <- "C:/"

# 2. 폴더이름 설정
foldname <- paste0(dr, "Users/mazy4/Desktop")

# 3. 작업공간 설정
setwd(foldname)

# 4. 파일이름 설정
filename <- "(E3) 발전업 data_바이오폐기물 발전량 추가.xlsx"

my_summary1(filename)
