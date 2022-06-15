### Code : 발전업 샘플링(의무대상자)
### Writer : Donghyeon Kim
### Date : 2020.07.19

### 의무대상자만 추출하여 모듈 3에서 제작한 '발전업 샘플링.xlsx' 파일에 시트 추가
dutytarget <- function(filename, colnum, rmyear) {
  
  ## 1. 제외할 연도, 의무대상자 필터링
  library(readxl) # package for read_xlsx function
  mydata <- read_xlsx(filename, sheet = 2, skip = 3)[, colnum]
  names(mydata)[3] <- "설비용량[kW]"
  
  # apply rmyear(제외할 연도 적용)
  mydata_f <- subset(mydata, mydata$사업년도 != rmyear & mydata$`의무대상자 여부` == "1")
  
  ## 2. 필터링한 자료를 엑셀 파일로 저장
  library(openxlsx)
  if(file.exists("발전업 샘플링.xlsx") == FALSE) {
    write.xlsx(mydata_sample, file = "발전업 샘플링.xlsx", sheetName = paste0("발전소 정보_RPS의무대상자(", rmyear, " ", "제외)"))
  } else {
    workbook <- loadWorkbook("발전업 샘플링.xlsx")
    addWorksheet(workbook, sheetName = paste0("발전소 정보_RPS의무대상자(", rmyear, " ", "제외)"))
    writeData(workbook, sheet = paste0("발전소 정보_RPS의무대상자(", rmyear, " ", "제외)"), mydata_f)
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

# (2) colnum - 파일에서 읽어들일 열
colnum <- 1:13

# (3) rmyear - 제외할 연도
rmyear <- 2019

dutytarget(filename, colnum, rmyear)
