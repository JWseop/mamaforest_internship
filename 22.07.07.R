# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()
# 불러올 데이터 파일명 '' 사이에 입력
file_name <- '0630-0706.csv'
df <- read.csv(file_name, header=T)
class(df)
names(df) <- c('주문번호','회원등급', '주문자ID', '주문상품명',	'상품옵션',	'주문상품명(옵션포함)',	'수량','상품구매금액', '총배송비', '사용한적립금액', '총결제금액', '총실결제금액',
               '결제업체', '매출경로', '주문 상태', '네이버포인트', '네이버캐시')
head(df)

# # 특정 열(df$열이름)에서 제거 하고싶은 단어(""사이)가 포함된 열 삭제
df_fix<-df
# df_fix<-df_fix[!grepl("솔솔홈", df$order_object),]
# head(df_fix)

# 주문번호 중복 제거
customer_ID <- df_fix[,3]
total_value <- df_fix[,12]

# 상품구매금액에서 ','문자 제거(숫자화)
total_value <- gsub(",", "", total_value)
total_value <- as.numeric(total_value)

# 주문개수 n
n=length(customer_ID)
n
#필요벡터 생성
result <- numeric()
temp <- integer()
level <- character()

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(customer_ID[i], customer_ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    customer_ID <- customer_ID[-temp[c(-1)]]
    total_value <- total_value[-temp[c(-1)]]
    n<-n-length(temp)+1
  }
  
  # 출력에 사용할 벡터에 값 저장
  result[i] <- total_value[i]
  
  # 등급 분류
  if(result[i]<10000){
    level[i] <- 0
  }
  if(result[i]>=10000&&result[i]<30000){
    level[i] <- 1
  }
  if(result[i]>=30000&&result[i]<50000){
    level[i] <- 2
  }
  if(result[i]>=50000&&result[i]<70000){
    level[i] <- 3
  }
  if(result[i]>=70000&&result[i]<100000){
    level[i] <- 4
  }
  if(result[i]>=100000&&result[i]<110000){
    level[i] <- 5
  }
  if(is.na(level[i])){
    j <- (result[i]-100000)%/%10000
    level[i] <- 5+j
  }
  if(i==n) {break}
}

RESULT <- cbind(customer_ID, result, level)

file_name <- gsub(".csv","",file_name)
file_name <- paste(file_name,"_result.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT,file=dir_name,row.names = FALSE)


