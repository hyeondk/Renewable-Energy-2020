### Code : 발전업 연도 필터링 및 현황 작성 모듈(1MW 구분O, 그림X)
### Writer : Donghyeon Kim
### Date : 2020.07.16

### 제외할 연도 필터링 & 현황 작성 모듈(1MW 구분)
powerfilt2 <- function(filename, colnum, rmyear, outfile) {
  library(readxl) # package for read_xlsx function
  mydata <- read_xlsx(filename, sheet = 1)[, colnum]
  names(mydata)[1] <- "ID"
  names(mydata)[4] <- "설비용량[kW]"
  
  # apply rmyear(제외할 연도 적용)
  mydata_f <- subset(mydata, mydata$사업년도 != rmyear)
  
  ## 현황 작성
  # 에너지원 추출(unique)
  en_source <- unique(mydata_f$에너지원)
  
  # 결과 저장할 데이터 프레임 생성
  result <- data.frame(matrix(NA, nrow = length(en_source) + 5, ncol = 11))
  names(result) <- c("에너지원", "용량", "모집단수", "비율", "용량_평균", "용량_표준편차",
                     "용량_최소", "용량_Q1", "용량_Q2", "용량_Q3", "용량_최대")
  
  # 데이터 프레임 1 ~ 2열 : 에너지원, 용량 입력
  result[, 1] <- c(rep(en_source[1:4], each = 2), en_source[5:length(en_source)], "총합")
  result[, 2] <- c(rep(c("1MW 이상", "1MW 미만"), 4), rep(NA, 7))
  
  # 데이터 프레임 3 ~ 10열 : 모집단수, 비율, 용량 기초통계량 입력
  pop <- NULL # 모집단수(3열)
  ratio <- NULL # 비율(4열)
  cap_mean <- NULL # 용량 평균(5열)
  cap_sd <- NULL # 용량 표준편차(6열)
  cap_min <- NULL # 용량 최소값(7열)
  cap_q1 <- NULL # 용량 Q1(8열)
  cap_q2 <- NULL # 용량 Q2(9열)
  cap_q3 <- NULL # 용량 Q3(10열)
  cap_max <- NULL # 용량 최대값(11열)
  
  for(i in 1:length(en_source)) {
    temp <- subset(mydata_f, mydata_f$에너지원 == en_source[i]) # 에너지원 부분집합(subset)
    if(temp$에너지원 == "태양광") {
      temp1 <- subset(temp, temp$`설비용량[kW]` >= 1000)
      temp2 <- subset(temp, temp$`설비용량[kW]` < 1000)
      
      pop[1] <- nrow(temp1); pop[2] <- nrow(temp2)
      ratio[1] <- pop[1]/nrow(mydata_f); ratio[2] <- pop[2]/nrow(mydata_f)
      cap_mean <- c(cap_mean, mean(temp1$`설비용량[kW]`), mean(temp2$`설비용량[kW]`))
      cap_sd <- c(cap_sd, sd(temp1$`설비용량[kW]`), sd(temp2$`설비용량[kW]`))
      cap_min <- c(cap_min, fivenum(temp1$`설비용량[kW]`)[1], fivenum(temp2$`설비용량[kW]`)[1])
      cap_q1 <- c(cap_q1, fivenum(temp1$`설비용량[kW]`)[2], fivenum(temp2$`설비용량[kW]`)[2])
      cap_q2 <- c(cap_q2, fivenum(temp1$`설비용량[kW]`)[3], fivenum(temp2$`설비용량[kW]`)[3])
      cap_q3 <- c(cap_q3, fivenum(temp1$`설비용량[kW]`)[4], fivenum(temp2$`설비용량[kW]`)[4])
      cap_max <- c(cap_max, fivenum(temp1$`설비용량[kW]`)[5], fivenum(temp2$`설비용량[kW]`)[5])
      rm(temp1); rm(temp2); rm(temp)
    } else if(temp$에너지원 == "바이오") {
      temp1 <- subset(temp, temp$`설비용량[kW]` >= 1000)
      temp2 <- subset(temp, temp$`설비용량[kW]` < 1000)
      
      pop[3] <- nrow(temp1); pop[4] <- nrow(temp2)
      ratio[3] <- pop[3]/nrow(mydata_f); ratio[4] <- pop[4]/nrow(mydata_f)
      cap_mean <- c(cap_mean, mean(temp1$`설비용량[kW]`), mean(temp2$`설비용량[kW]`))
      cap_sd <- c(cap_sd, sd(temp1$`설비용량[kW]`), sd(temp2$`설비용량[kW]`))
      cap_min <- c(cap_min, fivenum(temp1$`설비용량[kW]`)[1], fivenum(temp2$`설비용량[kW]`)[1])
      cap_q1 <- c(cap_q1, fivenum(temp1$`설비용량[kW]`)[2], fivenum(temp2$`설비용량[kW]`)[2])
      cap_q2 <- c(cap_q2, fivenum(temp1$`설비용량[kW]`)[3], fivenum(temp2$`설비용량[kW]`)[3])
      cap_q3 <- c(cap_q3, fivenum(temp1$`설비용량[kW]`)[4], fivenum(temp2$`설비용량[kW]`)[4])
      cap_max <- c(cap_max, fivenum(temp1$`설비용량[kW]`)[5], fivenum(temp2$`설비용량[kW]`)[5])
      rm(temp1); rm(temp2); rm(temp)
    } else if(temp$에너지원 == "수력") {
      temp1 <- subset(temp, temp$`설비용량[kW]` >= 1000)
      temp2 <- subset(temp, temp$`설비용량[kW]` < 1000)
      
      pop[5] <- nrow(temp1); pop[6] <- nrow(temp2)
      ratio[5] <- pop[5]/nrow(mydata_f); ratio[6] <- pop[6]/nrow(mydata_f)
      cap_mean <- c(cap_mean, mean(temp1$`설비용량[kW]`), mean(temp2$`설비용량[kW]`))
      cap_sd <- c(cap_sd, sd(temp1$`설비용량[kW]`), sd(temp2$`설비용량[kW]`))
      cap_min <- c(cap_min, fivenum(temp1$`설비용량[kW]`)[1], fivenum(temp2$`설비용량[kW]`)[1])
      cap_q1 <- c(cap_q1, fivenum(temp1$`설비용량[kW]`)[2], fivenum(temp2$`설비용량[kW]`)[2])
      cap_q2 <- c(cap_q2, fivenum(temp1$`설비용량[kW]`)[3], fivenum(temp2$`설비용량[kW]`)[3])
      cap_q3 <- c(cap_q3, fivenum(temp1$`설비용량[kW]`)[4], fivenum(temp2$`설비용량[kW]`)[4])
      cap_max <- c(cap_max, fivenum(temp1$`설비용량[kW]`)[5], fivenum(temp2$`설비용량[kW]`)[5])
      rm(temp1); rm(temp2); rm(temp)
    } else if(temp$에너지원 == "풍력") {
      temp1 <- subset(temp, temp$`설비용량[kW]` >= 1000)
      temp2 <- subset(temp, temp$`설비용량[kW]` < 1000)
      
      pop[7] <- nrow(temp1); pop[8] <- nrow(temp2)
      ratio[7] <- pop[7]/nrow(mydata_f); ratio[8] <- pop[8]/nrow(mydata_f)
      cap_mean <- c(cap_mean, mean(temp1$`설비용량[kW]`), mean(temp2$`설비용량[kW]`))
      cap_sd <- c(cap_sd, sd(temp1$`설비용량[kW]`), sd(temp2$`설비용량[kW]`))
      cap_min <- c(cap_min, fivenum(temp1$`설비용량[kW]`)[1], fivenum(temp2$`설비용량[kW]`)[1])
      cap_q1 <- c(cap_q1, fivenum(temp1$`설비용량[kW]`)[2], fivenum(temp2$`설비용량[kW]`)[2])
      cap_q2 <- c(cap_q2, fivenum(temp1$`설비용량[kW]`)[3], fivenum(temp2$`설비용량[kW]`)[3])
      cap_q3 <- c(cap_q3, fivenum(temp1$`설비용량[kW]`)[4], fivenum(temp2$`설비용량[kW]`)[4])
      cap_max <- c(cap_max, fivenum(temp1$`설비용량[kW]`)[5], fivenum(temp2$`설비용량[kW]`)[5])
    } else {
      pop <- c(pop, nrow(temp))
      ratio <- c(ratio, pop[i]/nrow(mydata_f))
      cap_mean <- c(cap_mean, mean(temp$`설비용량[kW]`))
      cap_sd <- c(cap_sd, sd(temp$`설비용량[kW]`))
      cap_min <- c(cap_min, fivenum(temp$`설비용량[kW]`)[1])
      cap_q1 <- c(cap_q1, fivenum(temp$`설비용량[kW]`)[2])
      cap_q2 <- c(cap_q2, fivenum(temp$`설비용량[kW]`)[3])
      cap_q3 <- c(cap_q3, fivenum(temp$`설비용량[kW]`)[4])
      cap_max <- c(cap_max, fivenum(temp$`설비용량[kW]`)[5])
    }
  }  
  # result에 결과 입력
  result[, 3] <- c(pop, sum(pop))
  result[, 4] <- c(ratio*100, sum(ratio*100))
  result[, 5] <- c(cap_mean, NA)
  result[, 6] <- c(cap_sd, NA)
  result[, 7] <- c(cap_min, NA)
  result[, 8] <- c(cap_q1, NA)
  result[, 9] <- c(cap_q2, NA)
  result[, 10] <- c(cap_q3, NA)
  result[, 11] <- c(cap_max, NA)
  
  # result를 엑셀 파일로 저장
  library(openxlsx)
  workbook <- loadWorkbook(outfile)
  addWorksheet(workbook, sheetName = paste0("발전업 현황", "(", rmyear, " ", "제외, 1MW 구분)"))
  writeData(workbook, sheet = paste0("발전업 현황", "(", rmyear, " ", "제외, 1MW 구분)"), result)
  saveWorkbook(workbook, outfile, overwrite = T)
}

# Function execution
# (1) filename - 불러올 파일명 / 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop")
setwd(foldname) # 작업공간 설정

filename <- paste0(foldname, "/", "발전업2020(표본추출).xlsx")

# (2) colnum - 파일에서 읽어들일 열
colnum <- 1:13

# (3) rmyear - 제외할 연도
rmyear <- 2019

# (4) outfile - 만들고자 하는 파일명
outfile <- "발전업 표본추출 2020.xlsx"

powerfilt2(filename, colnum, rmyear, outfile)
