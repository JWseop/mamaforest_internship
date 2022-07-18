# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()

# 불러올 데이터 파일명 '' 사이에 입력 / 위에 작성한 폴더안에 존재하는 파일이어야 함
input_file1 <- 'Full_data.csv'
output_file1 <- 'output.csv'
output_file2 <- 'Members_Rate_output.csv'
df <- read.csv(input_file, header=T)
#df_out <- read.csv(output_file1, header=T)
class(df)


# 전처리 - 주문자ID 결측치 분리
df$주문자ID[df$주문자ID == ""] <- NA
df_ID_na <- df[is.na(df$주문자ID),]
df <- df[!is.na(df$주문자ID),]

# 데이터프레임에서 각각 필요한 변수만 추출 및 추가
df <- df[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]
df[,"주문주기"] <- NA
df[,"주문금액대(만원)"] <- NA
df_ID_na <- df_ID_na[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]

# 전처리 - 총 결제금액 결측치 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
head(df$총.결제금액)
df <- df[!is.na(df$총.결제금액),]

#################################################
# 주문상품명 기준 주문수량 회원 비회원 구분
# 에러시 아래 'install.packages("dplyr")' 코드 주석 해제 후 다운 받고 아래 실행
# install.packages("dplyr")
library(dplyr)

# 주문상품별 회원 주문 수 데이터 생성
temp_table1<-table(df$주문상품명)
df_sold_num1 <- as.data.frame(temp_table1[c(order(temp_table1))])
names(df_sold_num1) <- c("product","Members_number")

# 주문상품별 비회원 주문 수 데이터 생성
temp_table2<-table(df_ID_na$주문상품명)
df_sold_num2 <- as.data.frame(temp_table2[c(order(temp_table2))])
names(df_sold_num2) <- c("product","Nonmembers_number")
# 주문상품별 회원/비회원의 주문 수 데이터 추가
df_sold_num1 <- full_join(df_sold_num1,df_sold_num2,by="product")

# 전처리 - 회원/비회원 수NA값 0으로 치환
df_sold_num1$Members_number[is.na(df_sold_num1$Members_number)] <- 0
df_sold_num1$Nonmembers_number[is.na(df_sold_num1$Nonmembers_number)] <- 0
# 제품별 총 주문수(total_order_num) 추가
df_sold_num1 <- mutate(df_sold_num1,
                       total_order_num=Members_number+Nonmembers_number)
# 총 주문수 기준 데이터프레임 재정렬
df_sold_num1 <- relocate(df_sold_num1,c(product,total_order_num,Members_number,Nonmembers_number))
df_sold_num1 <- df_sold_num1[order(df_sold_num1$total_order_num,decreasing=TRUE),]

# 총 주문수 대비 회원 비중
df_sold_num1 <- mutate(df_sold_num1,
                       Members_order_ratio=(Members_number/total_order_num)*100)
df_sold_num1$Members_order_ratio<-round(df_sold_num1$Members_order_ratio,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_sold_num1$Members_order_ratio <- as.character(df_sold_num1$Members_order_ratio)
df_sold_num1$Members_order_ratio <- paste(df_sold_num1$Members_order_ratio,"%", sep = "")

# 총 주문수 대비 비회원 비중
df_sold_num1 <- mutate(df_sold_num1,
                       Nonmembers_order_ratio=(Nonmembers_number/total_order_num)*100)
df_sold_num1$Nonmembers_order_ratio<-round(df_sold_num1$Nonmembers_order_ratio,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_sold_num1$Nonmembers_order_ratio <- as.character(df_sold_num1$Nonmembers_order_ratio)
df_sold_num1$Nonmembers_order_ratio <- paste(df_sold_num1$Nonmembers_order_ratio,"%", sep = "")

# 주문상품별 회원의 총주문수량 데이터 추가
df_quantity1<-aggregate(rep(1, nrow(df)), by = list(x = df$주문상품명, y = df$수량), sum)
names(df_quantity1) <- c("product","Order_quantity","num_of_Order")
df_quantity1 <- mutate(df_quantity1,Mem_order_quantity=Order_quantity*num_of_Order)
df_quantity1 <- df_quantity1[,c(1,4)]

#필요벡터 생성
temp <- integer()
n <- length(unique(df_quantity1$product))

# 수량 다른 동일제품 합치기
temp_quantity <- df_quantity1$product
df_quantity1$product<-gsub("[[:punct:]]", "", df_quantity1$product)
for(i in 1:n){
  #temp = 같은 제품들의 배열 index 값 저장
  temp<-grep(paste("^",df_quantity1$product[i],"$", sep=""), df_quantity1$product)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_quantity1$Mem_order_quantity[i] <- sum(df_quantity1$Mem_order_quantity[temp])
    df_quantity1 <- df_quantity1[-temp[c(-1)],]
  }
  if(i==n) {break}
}
df_quantity1$product <- unique(temp_quantity)

# 주문상품별 비회원의 총주문수량 데이터 추가
df_quantity2<-aggregate(rep(1, nrow(df_ID_na)), by = list(x = df_ID_na$주문상품명, y = df_ID_na$수량), sum)
names(df_quantity2) <- c("product","Order_quantity","num_of_Order")
df_quantity2 <- mutate(df_quantity2,Nonm_order_quantity=Order_quantity*num_of_Order)
df_quantity2 <- df_quantity2[,c(1,4)]

#필요벡터 생성
temp <- integer()

n <- length(unique(df_quantity2$product))

# 수량 다른 동일제품 합치기
temp_quantity <- df_quantity2$product
df_quantity2$product<-gsub("[[:punct:]]", "", df_quantity2$product)
for(i in 1:n){
  #temp = 같은 제품들의 배열 index 값 저장
  temp<-grep(paste("^",df_quantity2$product[i],"$", sep=""), df_quantity2$product)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_quantity2$Nonm_order_quantity[i] <- sum(df_quantity2$Nonm_order_quantity[temp])
    df_quantity2 <- df_quantity2[-temp[c(-1)],]
  }
  if(i==n) {break}
}
df_quantity2$product <- unique(temp_quantity)


# 합치기
df_quantity1 <- full_join(df_quantity1,df_quantity2,by="product")


# 전처리 - 회원/비회원 수NA값 0으로 치환
df_quantity1$Mem_order_quantity[is.na(df_quantity1$Mem_order_quantity)] <- 0
df_quantity1$Nonm_order_quantity[is.na(df_quantity1$Nonm_order_quantity)] <- 0
# 제품별 총 주문수(total_order_quantity) 추가
df_quantity1 <- mutate(df_quantity1,
                       total_order_quantity=Mem_order_quantity+Nonm_order_quantity)
# 총 주문수 기준 데이터프레임 재정렬
df_quantity1 <- relocate(df_quantity1,c(product,total_order_quantity,Mem_order_quantity,Nonm_order_quantity))
df_quantity1 <- df_quantity1[order(df_quantity1$total_order_quantity,decreasing=TRUE),]

# 총 주문수 대비 회원 비중
df_quantity1 <- mutate(df_quantity1,
                       Members_order_ratio=(Mem_order_quantity/total_order_quantity)*100)
df_quantity1$Members_order_ratio<-round(df_quantity1$Members_order_ratio,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_quantity1$Members_order_ratio <- as.character(df_quantity1$Members_order_ratio)
df_quantity1$Members_order_ratio <- paste(df_quantity1$Members_order_ratio,"%", sep = "")

# 총 주문수 대비 비회원 비중
df_quantity1 <- mutate(df_quantity1,
                       Nonm_order_ratio=(Nonm_order_quantity/total_order_quantity)*100)
df_quantity1$Nonm_order_ratio<-round(df_quantity1$Nonm_order_ratio,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_quantity1$Nonm_order_ratio <- as.character(df_quantity1$Nonm_order_ratio)
df_quantity1$Nonm_order_ratio <- paste(df_quantity1$Nonm_order_ratio,"%", sep = "")

df_product_result <- full_join(df_sold_num1,df_quantity1,by="product")

# 결과 파일 출력
dir_name <- paste(dir_name,'/',output_file2,sep="")
write.csv(df_product_result,file=dir_name,row.names = FALSE)
#############################################################




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

## Result
# 회원별 월간 총 결제금액 연산
n <- length(df$주문자ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(paste("^",df$주문자ID[i],"$", sep=""), df$주문자ID)
  # 중복 존재 시
  if(length(temp)>1){
    # 결제금액 합치기
    df$총.결제금액[i] <- sum(df$총.결제금액[temp])
    
    # df 주문주기 평균
    df$주문주기[i] <- mean(as.numeric(diff(df$주문일시[temp])))
    
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
RESULT <- data.frame(df$주문번호, df$주문일시, df$주문주기, df$주문자ID, df$총.결제금액, df$`주문금액대(만원)`)
names(RESULT) <- c("Order_Number","Order_Date","mean_Order_Term","ID","total_Payment","level")

# 기존 output 파일에 RESULT 데이터 추가
# RESULT <- rbind(df_out,RESULT)

# 주문번호 기준 재정렬(주문시간 순)
RESULT<-RESULT[order(RESULT$Order_Number),]

# 파일 생성
dir_name <- gsub(output_file2,"",dir_name)
dir_name <- paste(dir_name,output_file1,sep="")
write.csv(RESULT,file=dir_name,row.names = FALSE)
