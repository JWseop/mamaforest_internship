# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()
# 불러올 데이터 파일명 '' 사이에 입력
file_name <- 'mamaforest_2204-06.csv'
df <- read.csv(file_name, header=T)
class(df)
names(df) <- c('order_No','ID_level','ID','ID_sign_date','order_object',
                               'order_num','object_value','delivery_fee','purchased','결제업체','매출경로','주문상태')
head(df)

# # 특정 열(df$열이름)에서 제거 하고싶은 단어(""사이)가 포함된 열 삭제
df_fix<-df
# df_fix<-df_fix[!grepl("솔솔홈", df$order_object),]
# head(df_fix)

# 주문번호 중복 제거
order_No <- df_fix[,1]
object_value <- df_fix[,7]

# 상품구매금액에서 ','문자 제거(숫자화)
object_value=gsub(",", "", object_value)
object_value <- as.numeric(object_value)

# 주문개수 n
n <- length(order_No)
n
#필요벡터 생성
result <- numeric()
temp <- integer()
level <- character()

for(i in 1:n){
  #temp = 같은 주문번호의 index 벡터
  temp<-grep(order_No[i], order_No)
  
  # 중복 존재 시
  if(length(temp)>1){
    order_No <- order_No[-temp[c(-1)]]
    result[i] <- sum(object_value[temp])
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
RESULT <- cbind(order_No, result, level)

# 결과 파일 저장
file_name <- gsub(".csv","",file_name)
file_name <- paste(file_name,"_result.csv",sep="")
dir_name <- paste(dir_name,'/',file_name,sep="")

write.csv(RESULT,file=dir_name,row.names = FALSE)

