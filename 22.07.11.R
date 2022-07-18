# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()
# 불러올 데이터 파일명 '' 사이에 입력
file_name <- 'Full_data.csv'
df <- read.csv(file_name, header=T)
class(df)
df <- df[,c("주문번호","주문자ID","주문자.가입일","주문일시","총.결제금액")]

# 전처리 - 주문자 가입일 결측치 제거
df$주문자.가입일[df$주문자.가입일 == ""] <- NA
df <- df[!is.na(df$주문자.가입일),]
# 전처리 - 총 결제금액 결측치 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
head(df$총.결제금액)
df <- df[!is.na(df$총.결제금액),]

#필요벡터 생성
temp <- integer()
level <- character()
n <- length(df$주문번호)

# 주문번호 중복 삭제
for(i in 1:n){
  #temp = 같은 주문번호들의 배열 index 값 저장
  temp<-grep(df$주문번호[i], df$주문번호)
  
  # 중복 존재 시
  if(length(temp)>1){
    df <- df[-temp[c(-1)],]
    n <- n-length(temp)+1
  }
  
  if(i==n) {break}
}

# 총 주문자 수
all_customer_num <- length(unique(df$주문자ID))

## 월별 가입자 저장
# 4월 신규가입자
df_April<-df[grepl("2022-04", df$주문자.가입일),]
Apr_new_customer <- length(unique(df_April$주문자ID))
# 5월 신규가입자
df_May<-df[grepl("2022-05", df$주문자.가입일),]
May_new_customer <- length(unique(df_May$주문자ID))
# 6월 신규가입자
df_June<-df[grepl("2022-06", df$주문자.가입일),]
June_new_customer <- length(unique(df_June$주문자ID))





## 4월 result
# 회원별 월간 총 결제금액 연산
n <- length(df_April$주문자ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(df_April$주문자ID[i], df_April$주문자ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_April$총.결제금액[i] <- sum(df_April$총.결제금액[temp])
    df_April <- df_April[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  
  # 등급 분류
  if(df_April$총.결제금액[i]<10000){
    level[i] <- 0
  }
  if(df_April$총.결제금액[i]>=10000){
    j <- df_April$총.결제금액[i]%/%10000
    level[i] <- j
  }
  if(i==n) {break}
}
# 소수점 버린 평균값
April_mean_payment <- as.integer(mean(df_April$총.결제금액))

# 결과 저장 및 파일 추출
RESULT_April <- data.frame(df_April$주문일시,df_April$주문자ID, df_April$총.결제금액, level)
names(RESULT_April) <- c("Order_date","ID","total_Payment","level")
file_name <- gsub(".csv","",file_name)
file_name <- paste(file_name,"_Apr.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT_April,file=dir_name,row.names = FALSE)




## 5월 result
# 회원별 월간 총 결제금액 연산
n <- length(df_May$주문자ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(df_May$주문자ID[i], df_May$주문자ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_May$총.결제금액[i] <- sum(df_May$총.결제금액[temp])
    df_May <- df_May[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  
  # 등급 분류
  if(df_May$총.결제금액[i]<10000){
    level[i] <- 0
  }
  if(df_May$총.결제금액[i]>=10000){
    j <- df_May$총.결제금액[i]%/%10000
    level[i] <- j
  }
  if(i==n) {break}
}
# 소수점 버린 평균값
May_mean_payment <- as.integer(mean(df_May$총.결제금액))

# 결과 저장 및 파일 추출
RESULT_May <- data.frame(df_May$주문일시,df_May$주문자ID, df_May$총.결제금액, level)
names(RESULT_May) <- c("Order_date","ID","total_Payment","level")
dir_name <- getwd()
file_name <- gsub("_Apr.csv","",file_name)
file_name <- paste(file_name,"_May.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT_May,file=dir_name,row.names = FALSE)





## 6월 result
# 회원별 월간 총 결제금액 연산
n <- length(df_June$주문자ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(df_June$주문자ID[i], df_June$주문자ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_June$총.결제금액[i] <- sum(df_June$총.결제금액[temp])
    df_June <- df_June[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  
  # 등급 분류
  if(df_June$총.결제금액[i]<10000){
    level[i] <- 0
  }
  if(df_June$총.결제금액[i]>=10000){
    j <- df_June$총.결제금액[i]%/%10000
    level[i] <- j
  }
  if(i==n) {break}
}
# 소수점 버린 평균값
June_mean_payment <- as.integer(mean(df_June$총.결제금액))
a<-data.frame(table(level))
# 결과 저장 및 파일 추출
RESULT_June <- data.frame(df_June$주문일시,df_June$주문자ID, df_June$총.결제금액, level)
names(RESULT_June) <- c("Order_date","ID","total_Payment","level")
dir_name <- getwd()
file_name <- gsub("_May.csv","",file_name)
file_name <- paste(file_name,"_June.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT_June,file=dir_name,row.names = FALSE)
