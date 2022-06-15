### Code : 발전업 샘플링
### Writer : Donghyeon Kim
### Date : 2020.07.18

### 원하는 에너지원을 입력하여 해당 에너지원에 대해서만 샘플링 해주는 코드
samplingpower <- function(filename, colnum, rmyear, eng_source, m, k) {
  
  ## 1. 제외할 연도 필터링
  library(readxl) # package for read_xlsx function
  mydata <- read_xlsx(filename, sheet = 1)[, colnum]
  names(mydata)[1] <- "ID"
  names(mydata)[4] <- "설비용량[kW]"
  
  # apply rmyear(제외할 연도 적용)
  mydata_f <- subset(mydata, mydata$사업년도 != rmyear & mydata$에너지원 == eng_source)
  sort_CAP <- order(mydata_f$`설비용량[kW]`) # index
  mydata_sf <- mydata_f[sort_CAP, ] # index에 따른 정렬
  
  ## 2. 데이터가 50개 이상이면 표본조사 / 아닌 경우 전수조사
  if(nrow(mydata_sf) > 50) {
    
    ## 2-1. (1) 주표본 샘플링 - 계통추출법
    N <- nrow(mydata_sf) # 모집단 수
    Nm <- as.integer(N/m) # 모집단 수 / 샘플링하고자 하는 개수
    
    library(purrr) # package for rdunif function
    x    <- rdunif(1, 1, Nm) # discrete uniform distribution
    rsfunc <- function(m, Nm, x) {
      rsid <- (1:m)*Nm+x
      remaind <- (m+1)*Nm+x
      if(remaind < N) {
        rsid <- c(rsid, remaind)
      }
      return(rsid)
    }
    rsid_result <- rsfunc(m, Nm, x)
    
    mydata_sample <- mydata_sf[rsid_result, ]
    
    # 엑셀 파일로 저장
    library(openxlsx)
    if(file.exists("발전업 샘플링.xlsx") == FALSE) {
      write.xlsx(mydata_sample, file = "발전업 샘플링.xlsx", sheetName = paste0(eng_source, " ", "주표본"))
    } else {
      workbook <- loadWorkbook("발전업 샘플링.xlsx")
      addWorksheet(workbook, sheetName = paste0(eng_source, " ", "주표본"))
      writeData(workbook, sheet = paste0(eng_source, " ", "주표본"), mydata_sample)
      saveWorkbook(workbook, "발전업 샘플링.xlsx", overwrite = T)
      rm(workbook)
    }
    
    ## 2-1. (2) 대체 표본 샘플링
    for(i in 1:k) {
      subx <- x + ((-1)^i)*round(i/2 + 0.1)
      subx_result <- rsfunc(m, Nm, subx)
      mydata_subsample <- mydata_sf[subx_result, ]
      
      library(openxlsx)
      workbook <- loadWorkbook("발전업 샘플링.xlsx")
      addWorksheet(workbook, sheetName = paste0(eng_source, " ", "대체표본", i))
      writeData(workbook, sheet = paste0(eng_source, " ", "대체표본", i), mydata_subsample)
      saveWorkbook(workbook, "발전업 샘플링.xlsx", overwrite = T)
      
      rm(subx); rm(subx_result); rm(mydata_subsample)
    }
  } else {
    
    ## 2-2. 모집단 전부 가져오기
    library(openxlsx)
    workbook <- loadWorkbook("발전업 샘플링.xlsx")
    addWorksheet(workbook, sheetName = paste0(eng_source, " ", "전체(전수조사)"))
    writeData(workbook, sheet = paste0(eng_source, " ", "전체(전수조사)"), mydata_sf)
    saveWorkbook(workbook, "발전업 샘플링.xlsx", overwrite = T)
    rm(workbook)
  }
}

## Function execution
# 작업공간 설정
dr <- "D:/"
foldname <- paste0(dr, "2. Projects/2020_Project/1. 신재생에너지/6. 표본설계및추출/62. 발전업")
setwd(foldname)

# 폴더 생성 이후 다시 작업 공간 설정
makefold <- "발전업 샘플링(2020)"
dir.create(makefold) # 폴더 생성
newfoldname <- paste0(foldname, "/", makefold)
setwd(newfoldname)

# (1) filename - 불러올 파일 이름 설정
filename <- paste0(foldname, "/", "발전업2020(표본추출).xlsx")

# (2) colnum - 불러올 파일에서 읽어들일 열
colnum <- 1:13

# (3) rmyear - 제외할 연도
rmyear <- 2019

# (4) eng_source - 추출하고자 하는 에너지원
eng_source <- "바이오매스"

# (5) m - 샘플링하고자 하는 개수(전수조사인 경우, 해당 변수는 사용되지 않음)
m <- 35

# (6) k - 대체 표본 수(전수조사인 경우, 해당 변수는 사용되지 않음)
k <- 1

samplingpower(filename, colnum, rmyear, eng_source, m, k)
