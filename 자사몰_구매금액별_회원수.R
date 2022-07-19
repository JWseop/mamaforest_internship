# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()

# 불러올 데이터 파일명 '' 사이에 입력 / 위에 작성한 폴더안에 존재하는 파일이어야 함
input_file <- 'Full_data.csv'
output_file <- '자사몰_2022_04-06_구매금액별_회원수.csv'
df <- read.csv(input_file, header=T)
class(df)


# 전처리 - 주문자ID 결측치 분리
df$주문자ID[df$주문자ID == ""] <- NA
df_ID_na <- df[is.na(df$주문자ID),]
df <- df[!is.na(df$주문자ID),]

# 데이터프레임에서 각각 필요한 변수만 추출 및 추가
df <- df[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]
df[,"주문금액대(만원)"] <- NA
df_ID_na <- df_ID_na[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]

# 전처리 - 총 결제금액 결측치 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
df <- df[!is.na(df$총.결제금액),]

# 
n <- length(df$주문번호)

# 주문번호 중복 삭제
for(i in 1:n){
  #temp = 같은 주문번호들의 배 열 index 값 저장
  temp<-grep(paste("^",df$주문번호[i],"$", sep=""), df$주문번호)
  # 중복 존재 시
  if(length(temp)>1){
    df <- df[-temp[c(-1)],]
    n <- n-length(temp)+1
  }
  if(i==n) {break}
}

df$주문일시<-as.Date(df$주문일시, '%Y.%m.%d')


# 총 주문자 수
all_ID_num <- length(unique(df$주문자ID))

############################# Result #############################
# 회원별 월간 총 결제금액 연산
n <- length(df$주문자ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(paste("^",df$주문자ID[i],"$", sep=""), df$주문자ID)
  # 중복 존재 시
  if(length(temp)>1){
    # 결제금액 합치기
    df$총.결제금액[i] <- sum(df$총.결제금액[temp])
    
    # df 중복 삭제
    df <- df[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  else if(length(temp)==1){
    df$주문주기[i]<-0
  }
  # 등급 분류
  if(df$총.결제금액[i]<10000){
    df$`주문금액대(만원)`[i] <- 0
  }
  if(df$총.결제금액[i]>=10000){
    j <- df$총.결제금액[i]%/%10000
    df$`주문금액대(만원)`[i] <- j
  }
  if(i==n) {break}
}

# 소수점 버린 평균값
MEAN_payment <- as.integer(mean(df$총.결제금액))

# 결과 저장 및 파일 추출
RESULT <- data.frame(df$주문자ID, df$총.결제금액, df$`주문금액대(만원)`)
names(RESULT) <- c("주문자ID","총 결제금액","주문금액대(만원)")

# 주문번호 기준 재정렬(주문시간 순)
RESULT<-RESULT[order(RESULT$"총 결제금액",decreasing=TRUE),]

# 결제구간 별 구분
result_table<-as.data.frame(table(RESULT$`주문금액대(만원)`))
names(result_table) <- c("주문금액대(만원)","회원 수")
result_table$`주문금액대(만원)` <- paste(as.character(result_table$`주문금액대(만원)`),"만원대", sep = "")
result_table$`회원 수` <- paste(as.character(result_table$`회원 수`),"명", sep = "")

# 파일 생성
dir_name <- paste(dir_name,'/',output_file,sep="")
write.csv(RESULT,file=dir_name,row.names = FALSE,fileEncoding="cp949")
dir_name <- gsub('.csv',"",dir_name)
dir_name <- paste(dir_name,'_결과.csv',sep="")
write.table(result_table,file=dir_name,sep = ",",row.names = FALSE,fileEncoding="cp949")
