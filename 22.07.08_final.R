# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()
# 불러올 데이터 파일명 '' 사이에 입력
file_name <- 'mamaforest_2204-06.csv'
df <- read.csv(file_name, header=T)
class(df)
names(df) <- c('order_No','ID_level','ID','ID_sign_date','order_product_name',
               'quantity','product_purchase_amount','delivery_fee','payment_amount','payment_company',
               'sales_path','order_status')
head(df)

# 전처리 - ID 결측치 제거
df$ID[df$ID == ""] <- NA
df <- df[!is.na(df$ID),]
head(df)
# 전처리 - 총 결제금액 결측치 제거
df<-df[!grepl(" - ", df$payment_amount),]

df_fix<-df
df_fix
# 주문번호 중복 제거
order_No <- df_fix[,1]
ID <- df_fix[,3]
object_value <- df_fix[,9]

# 상품구매금액에서 ','문자 제거(숫자화)
object_value=gsub(",", "", object_value)
object_value <- as.numeric(object_value)
object_value
# 주문개수 n
n <- length(order_No)
n
#필요벡터 생성
result <- numeric()
temp <- integer()
level <- character()

# 주문번호, ID 하나로 합치기
ID <- paste(order_No,ID)

# 주문번호 중복 삭제
for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(ID[i], ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    ID <- ID[-temp[c(-1)]]
    object_value <- object_value[-temp[c(-1)]]
    n<-n-length(temp)+1
  }
  if(i==n) {break}
}

# 문자열에서 ID만 남기기
ID<-substr(ID, 17,50)

# ID 중복 시 가격 합치기
for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(ID[i], ID)
  
  # 중복 존재 시
  if(length(temp)>1){
    ID <- ID[-temp[c(-1)]]
    result[i] <- sum(object_value[temp])
    object_value <- object_value[-temp[c(-1)]]
    n<-n-length(temp)+1
  }
  # 중복 없을 시
  if(length(temp)==1){
    result[i] <- object_value[temp]
  }
  
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

# 결과 저장 및 파일 추출
RESULT <- cbind(ID, result, level)

file_name <- gsub(".csv","",file_name)
file_name <- paste(file_name,"_result.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT,file=dir_name,row.names = FALSE)

