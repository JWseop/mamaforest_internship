# 2022/07/19 정우섭
# 4-6월 간 회원 데이터 등, cafe24를 통해 다운받은 파일을 입력하면,
#해당 기간동안 주문상품별로 '주문 수'와 '주문수량'의 회원/비회원 '인원 수'와 그 비율을 결과 파일로 출력해준다.

# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()

# 불러올 데이터 파일명은 line6에 입력한 폴더안에 존재하는 파일이어야 함
# 새로 입력할 파일을 line12의 '' 사이에 입력
# 기존에 존재하던 결과파일명을 line13의 '' 사이에 입력
input_file <- 'input_data_sample.csv'
origin_result_file <- '상품별_회원_비회원_비율.csv'
df <- read.csv(input_file, header=T)
df_product_result_origin <- read.csv(origin_result_file, header=T,fileEncoding = "cp949")
class(df)

# 기존 결과값 전처리 - 주문수, 주문수량 데이터 분류
df_origin_num <- df_product_result_origin[,c(1:4)]
df_origin_quantity <- df_product_result_origin[,c(1,7:9)]

# 전처리 - 주문자ID 결측치 분리
df$주문자ID[df$주문자ID == ""] <- NA
df_ID_na <- df[is.na(df$주문자ID),]
df <- df[!is.na(df$주문자ID),]

# 데이터프레임에서 각각 필요한 변수만 추출 및 추가
df <- df[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]
df[,"주문주기"] <- NA
df[,"주문횟수"] <- NA
df_ID_na <- df_ID_na[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]

# 전처리 - 총 결제금액 결측치 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
head(df$총.결제금액)
df <- df[!is.na(df$총.결제금액),]

################################### 상품기준_Report ###################################
# 주문상품명 기준 주문수량 회원 비회원 구분
# 에러시 아래 'install.packages("dplyr")' 코드 주석 해제 후 다운 받고 아래 실행
# install.packages("dplyr")
library(dplyr)

# 주문상품별 회원 주문 수 데이터 생성
temp_table1<-table(df$주문상품명)
df_sold_num1 <- as.data.frame(temp_table1[c(order(temp_table1))])
names(df_sold_num1) <- c("주문상품","회원_주문_수")

# 주문상품별 비회원 주문 수 데이터 생성
temp_table2<-table(df_ID_na$주문상품명)
df_sold_num2 <- as.data.frame(temp_table2[c(order(temp_table2))])
names(df_sold_num2) <- c("주문상품","비회원_주문_수")

# 주문상품별 회원/비회원의 주문 수 데이터 추가
df_sold_num1 <- full_join(df_sold_num1,df_sold_num2,by="주문상품")

# 전처리 - 회원/비회원 수NA값 0으로 치환
df_sold_num1$회원_주문_수[is.na(df_sold_num1$회원_주문_수)] <- 0
df_sold_num1$비회원_주문_수[is.na(df_sold_num1$비회원_주문_수)] <- 0
# 제품별 총 주문수(총_주문_수) 추가
df_sold_num1 <- mutate(df_sold_num1,
                       총_주문_수=회원_주문_수+비회원_주문_수)
# 총 주문수 기준 데이터프레임 재정렬
df_sold_num1 <- relocate(df_sold_num1,c(주문상품,총_주문_수,회원_주문_수,비회원_주문_수))
for(i in 1:length(df_sold_num1$주문상품)){
  for(j in 1:length(df_origin_num$주문상품)){
    if(df_origin_num$주문상품[i]==df_sold_num1$주문상품[j]){
      df_sold_num1$총_주문_수[j]    <-df_sold_num1$총_주문_수[j]    +df_origin_num$총_주문_수[i]
      df_sold_num1$회원_주문_수[j]  <-df_sold_num1$회원_주문_수[j]  +df_origin_num$회원_주문_수[i]
      df_sold_num1$비회원_주문_수[j]<-df_sold_num1$비회원_주문_수[j]+df_origin_num$비회원_주문_수[i]
    }
  }
}
df_sold_num1 <- df_sold_num1[order(df_sold_num1$총_주문_수,decreasing=TRUE),]

# 총 주문수 대비 회원 비중
df_sold_num1 <- mutate(df_sold_num1,
                       회원_주문_비율=(회원_주문_수/총_주문_수)*100)
df_sold_num1$회원_주문_비율<-round(df_sold_num1$회원_주문_비율,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_sold_num1$회원_주문_비율 <- as.character(df_sold_num1$회원_주문_비율)
df_sold_num1$회원_주문_비율 <- paste(df_sold_num1$회원_주문_비율,"%", sep = "")

# 총 주문수 대비 비회원 비중
df_sold_num1 <- mutate(df_sold_num1,
                       비회원_주문_비율=(비회원_주문_수/총_주문_수)*100)
df_sold_num1$비회원_주문_비율<-round(df_sold_num1$비회원_주문_비율,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_sold_num1$비회원_주문_비율 <- as.character(df_sold_num1$비회원_주문_비율)
df_sold_num1$비회원_주문_비율 <- paste(df_sold_num1$비회원_주문_비율,"%", sep = "")

# 주문상품별 회원의 총주문수량 데이터 추가
df_quantity1<-aggregate(rep(1, nrow(df)), by = list(x = df$주문상품명, y = df$수량), sum)
names(df_quantity1) <- c("주문상품","Order_quantity","num_of_Order")
df_quantity1 <- mutate(df_quantity1,회원_주문수량=Order_quantity*num_of_Order)
df_quantity1 <- df_quantity1[,c(1,4)]

#필요벡터 생성
temp <- integer()
n <- length(unique(df_quantity1$주문상품))

# 수량 다른 동일제품 합치기
temp_quantity <- df_quantity1$주문상품
df_quantity1$주문상품<-gsub("[[:punct:]]", "", df_quantity1$주문상품)
for(i in 1:n){
  #temp = 같은 제품들의 배열 index 값 저장
  temp<-grep(paste("^",df_quantity1$주문상품[i],"$", sep=""), df_quantity1$주문상품)
  
  # 중복 존재 시
  if(length(temp)>1){
    df_quantity1$회원_주문수량[i] <- sum(df_quantity1$회원_주문수량[temp])
    df_quantity1 <- df_quantity1[-temp[c(-1)],]
  }
  if(i==n) {break}
}
df_quantity1$주문상품 <- unique(temp_quantity)

# 주문상품별 비회원의 총주문수량 데이터 추가
df_quantity2<-aggregate(rep(1, nrow(df_ID_na)), by = list(x = df_ID_na$주문상품명, y = df_ID_na$수량), sum)
names(df_quantity2) <- c("주문상품","Order_quantity","num_of_Order")
df_quantity2 <- mutate(df_quantity2,비회원_주문수량=Order_quantity*num_of_Order)
df_quantity2 <- df_quantity2[,c(1,4)]

#필요벡터 생성
temp <- integer()

n <- length(unique(df_quantity2$주문상품))

# 수량 다른 동일제품 합치기
temp_quantity <- df_quantity2$주문상품
df_quantity2$주문상품<-gsub("[[:punct:]]", "", df_quantity2$주문상품)
for(i in 1:n){
  #temp = 같은 제품들의 배열 index 값 저장
  temp<-grep(paste("^",df_quantity2$주문상품[i],"$", sep=""), df_quantity2$주문상품)
  # 중복 존재 시
  if(length(temp)>1){
    df_quantity2$비회원_주문수량[i] <- sum(df_quantity2$비회원_주문수량[temp])
    df_quantity2 <- df_quantity2[-temp[c(-1)],]
  }
  if(i==n) {break}
}
df_quantity2$주문상품 <- unique(temp_quantity)


# 합치기
df_quantity1 <- full_join(df_quantity1,df_quantity2,by="주문상품")


# 전처리 - 회원/비회원 수NA값 0으로 치환
df_quantity1$회원_주문수량[is.na(df_quantity1$회원_주문수량)] <- 0
df_quantity1$비회원_주문수량[is.na(df_quantity1$비회원_주문수량)] <- 0
# 제품별 총 주문수(총_주문수량) 추가
df_quantity1 <- mutate(df_quantity1,
                       총_주문수량=회원_주문수량+비회원_주문수량)
# 총 주문수 기준 데이터프레임 재정렬
df_quantity1 <- relocate(df_quantity1,c(주문상품,총_주문수량,회원_주문수량,비회원_주문수량))

for(i in 1:length(df_quantity1$주문상품)){
  for(j in 1:length(df_origin_quantity$주문상품)){
    if(df_origin_quantity$주문상품[i]==df_quantity1$주문상품[j]){
      df_quantity1$총_주문수량[j]    <-df_quantity1$총_주문수량[j]    +df_origin_quantity$총_주문수량[i]
      df_quantity1$회원_주문수량[j]  <-df_quantity1$회원_주문수량[j]  +df_origin_quantity$회원_주문수량[i]
      df_quantity1$비회원_주문수량[j]<-df_quantity1$비회원_주문수량[j]+df_origin_quantity$비회원_주문수량[i]
    }
  }
}

df_quantity1 <- df_quantity1[order(df_quantity1$총_주문수량,decreasing=TRUE),]

# 총 주문수 대비 회원 비중
df_quantity1 <- mutate(df_quantity1,
                       회원_주문수량_비율=(회원_주문수량/총_주문수량)*100)
df_quantity1$회원_주문수량_비율<-round(df_quantity1$회원_주문수량_비율,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_quantity1$회원_주문수량_비율 <- as.character(df_quantity1$회원_주문수량_비율)
df_quantity1$회원_주문수량_비율 <- paste(df_quantity1$회원_주문수량_비율,"%", sep = "")

# 총 주문수 대비 비회원 비중
df_quantity1 <- mutate(df_quantity1,
                       비회원_주문수량_비율=(비회원_주문수량/총_주문수량)*100)
df_quantity1$비회원_주문수량_비율<-round(df_quantity1$비회원_주문수량_비율,1)

# 문자열로 변환 후 % 붙여 출력
# 연산 필요시 본 주석 윗 줄에 추가
df_quantity1$비회원_주문수량_비율 <- as.character(df_quantity1$비회원_주문수량_비율)
df_quantity1$비회원_주문수량_비율 <- paste(df_quantity1$비회원_주문수량_비율,"%", sep = "")

df_product_result <- full_join(df_sold_num1,df_quantity1,by="주문상품")



# 결과 파일 출력
dir_name <- paste(dir_name,'/',origin_result_file,sep="")
write.csv(df_product_result,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################