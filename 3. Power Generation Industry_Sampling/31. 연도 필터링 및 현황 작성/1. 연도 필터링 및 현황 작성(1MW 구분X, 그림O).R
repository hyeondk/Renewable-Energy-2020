### Code : 발전업 연도 필터링 및 현황 작성 모듈(1MW 구분X, 그림O)
### Writer : Donghyeon Kim
### Date : 2020.07.16

### 제외할 연도 필터링 & 현황 작성 모듈
powerfilt <- function(filename, colnum, rmyear, outfile, sheetname) {
  library(readxl) # package for read_xlsx function
  mydata <- read_xlsx(filename, sheet = 1)[, colnum]
  names(mydata)[1] <- "ID"
  names(mydata)[4] <- "설비용량[kW]"
  
  # apply rmyear(제외할 연도 적용)
  mydata_f <- subset(mydata, mydata$사업년도 != rmyear)
  
  # Write Excel file(위에서 필터링한 데이터을 엑셀로 저장)
  library(openxlsx)
  write.xlsx(mydata_f, file = outfile, sheetName = sheetname)
  
  ## 현황 작성 / 히스토그램 및 박스플롯 그리기
  # 에너지원 추출(unique)
  en_source <- unique(mydata_f$에너지원)
  
  # 결과 저장할 데이터 프레임 생성
  result <- data.frame(matrix(NA, nrow = length(en_source) + 1, ncol = 10))
  names(result) <- c("에너지원", "모집단수", "비율", "용량_평균", "용량_표준편차",
                     "용량_최소", "용량_Q1", "용량_Q2", "용량_Q3", "용량_최대")
  
  # 데이터 프레임 1열 : 에너지원 입력
  result[, 1] <- c(en_source, "총합")
  
  # 데이터 프레임 2 ~ 10열 : 모집단수, 비율, 용량 기초통계량 입력
  pop <- NULL # 모집단수(2열)
  ratio <- NULL # 비율(3열)
  cap_mean <- NULL # 용량 평균(4열)
  cap_sd <- NULL # 용량 표준편차(5열)
  cap_min <- NULL # 용량 최소값(6열)
  cap_q1 <- NULL # 용량 Q1(7열)
  cap_q2 <- NULL # 용량 Q2(8열)
  cap_q3 <- NULL # 용량 Q3(9열)
  cap_max <- NULL # 용량 최대값(10열)
  
  for(i in 1:length(en_source)) {
    temp <- subset(mydata_f, mydata_f$에너지원 == en_source[i]) # 에너지원 부분집합(subset)
    pop <- c(pop, nrow(temp))
    ratio <- c(ratio, pop[i]/nrow(mydata_f))
    cap_mean <- c(cap_mean, mean(temp$`설비용량[kW]`))
    cap_sd <- c(cap_sd, sd(temp$`설비용량[kW]`))
    cap_min <- c(cap_min, fivenum(temp$`설비용량[kW]`)[1])
    cap_q1 <- c(cap_q1, fivenum(temp$`설비용량[kW]`)[2])
    cap_q2 <- c(cap_q2, fivenum(temp$`설비용량[kW]`)[3])
    cap_q3 <- c(cap_q3, fivenum(temp$`설비용량[kW]`)[4])
    cap_max <- c(cap_max, fivenum(temp$`설비용량[kW]`)[5])
    
    png(filename = paste0(en_source[i], " ", "발전량 분포도", ".png"),
        width = 2000, height = 1500)
    nf <- layout(mat = matrix(c(1, 2), 2, 1, byrow = T), height = c(3, 1))
    par(mar = c(3.1, 3.1, 1.1, 2.1))
    hist(temp$`설비용량[kW]`, xlim = c(0, max(temp$`설비용량[kW]`)), col = "pink",
         main = paste0(en_source[i], " ", "발전량 분포도"))
    rug(temp$`설비용량[kW]`)
    boxplot(temp$`설비용량[kW]`, horizontal = T, outline = T, ylim = c(0, max(temp$`설비용량[kW]`)),
                                                                   frame = F, col = "green1", width = 10)
    dev.off()
    rm(temp)
  }
  
  # result에 결과 입력
  result[, 2] <- c(pop, sum(pop))
  result[, 3] <- c(ratio*100, sum(ratio*100))
  result[, 4] <- c(cap_mean, NA)
  result[, 5] <- c(cap_sd, NA)
  result[, 6] <- c(cap_min, NA)
  result[, 7] <- c(cap_q1, NA)
  result[, 8] <- c(cap_q2, NA)
  result[, 9] <- c(cap_q3, NA)
  result[, 10] <- c(cap_max, NA)
  
  # result를 엑셀 파일로 저장
  library(openxlsx)
  workbook <- loadWorkbook(outfile)
  addWorksheet(workbook, sheetName = paste0("발전업 현황", "(", rmyear, " ", "제외)"))
  writeData(workbook, sheet = paste0("발전업 현황", "(", rmyear, " ", "제외)"), result)
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

# (5) sheetname - 만들고자 파일에서의 파일 시트명
sheetname <- "발전업 2019년 제외한 자료"

powerfilt(filename, colnum, rmyear, outfile, sheetname)
