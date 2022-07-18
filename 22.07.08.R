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

# 전처리 - ID 결측치 제거
df$ID[df$ID == ""] <- NA
df
df <- df[!is.na(df$ID),]
head(df)
# 전처리 - 총 결제금액 결측치 제거
df<-df[!grepl(" - ", df$payment_amount),]

#필요벡터 생성
temp <- integer()
level <- character()
n<-length(df$ID)


# 상품구매금액에서 ','문자 제거(숫자화)
df$payment_amount <- gsub(",", "", df$payment_amount)
df$payment_amount <- as.numeric(df$payment_amount)
df$payment_amount
# 주문개수 n
for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(df$ID[i], df$ID)
  # 중복 존재 시
  if(length(temp)>1){
    for(j in temp){
      if(df$order_No[i]!=df$order_No[j]){
        df$payment_amount[i] <- df$payment_amount[i] + df$payment_amount[j]
      }
    }
  }
  tm<-temp[c(1)]
  df <- df[-tm,]
  n<-length(df$ID)
  
  # 등급 분류
  if(df$payment_amount[i]<10000){
    level[i] <- 0
  }
  if(df$payment_amount[i]>=10000&&df$payment_amount[i]<30000){
    level[i] <- 1
  }
  if(df$payment_amount[i]>=30000&&df$payment_amount[i]<50000){
    level[i] <- 2
  }
  if(df$payment_amount[i]>=50000&&df$payment_amount[i]<70000){
    level[i] <- 3
  }
  if(df$payment_amount[i]>=70000&&df$payment_amount[i]<100000){
    level[i] <- 4
  }
  if(df$payment_amount[i]>=100000&&df$payment_amount[i]<110000){
    level[i] <- 5
  }
  if(is.na(level[i])){
    j <- (df$payment_amount[i]-100000)%/%10000
    level[i] <- 5+j
  }
  if(i==n) {break}
}

RESULT <- cbind(df$ID, df$payment_amount, level)

file_name <- gsub(".csv","",file_name)
file_name <- paste(file_name,"_result.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT,file=dir_name,row.names = FALSE)


