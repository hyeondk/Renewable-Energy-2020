### 모듈 1. 신재생에너지 대분류별 분류
### Writer : Donghyeon Kim
### Date : 2020.08.04(최종 수정일)

## 사용자 함수(모듈)
classify <- function(filename, sheetname) {
  
  # 1. 파일 불러오기
  library(readxl) # package for read_xlsx function
  mydata <- read_xlsx(filename, sheet = sheetname, skip = 3)
  
  # 2. 산업별로 분류 및 엑셀 파일로 저장
  div_num <- c("1", "2", "3", "4")
  data_name <- c("제조업", "건설업", "공급업", "서비스업")
  
  mydata_f <- subset(mydata, substr(mydata$`신재생에너지 산업 분류 코드`, 1, 1) == div_num[1])
  
  library(openxlsx)
  write.xlsx(mydata_f, file = "대분류별 분류 자료.xlsx", sheetName = data_name[1])
  rm(mydata_f)
  
  for(i in 1:3) {
    mydata_f <- subset(mydata, substr(mydata$`신재생에너지 산업 분류 코드`, 1, 1) == div_num[i+1])
    workbook <- loadWorkbook("대분류별 분류 자료.xlsx")
    addWorksheet(workbook, sheetName = data_name[i+1])
    writeData(workbook, sheet = data_name[i+1], mydata_f)
    saveWorkbook(workbook, "대분류별 분류 자료.xlsx", overwrite = T)
    rm(workbook); rm(mydata_f)
  }
}

## Function execution
# 작업공간 설정
dr <- "C:/"
foldname <- paste0(dr, "Users/mazy4/Desktop")
setwd(foldname)

# 폴더 생성 이후 다시 작업 공간 설정
makefold <- "result"
dir.create(makefold) # 폴더 생성
newfoldname <- paste0(foldname, "/", makefold)
setwd(newfoldname)


# (1) filename - 불러올 파일 이름 설정
filename <- paste0(foldname, "/data/", "특수분류 추정 방법론3 재추정 데이터_수정2_200701.xlsx")

# (2) sheetname - 불러올 파일에서 사용할 시트 이름 설정
sheetname <- "데이터1"

classify(filename, sheetname)
