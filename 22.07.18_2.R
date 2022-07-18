# 불러올 데이터프레임이 포함된 폴더 위치 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀')
dir_name <- getwd()

# 불러올 데이터 파일명 '' 사이에 입력 / 위에 작성한 폴더안에 존재하는 파일이어야 함
input_file1 <- 'Full_data.csv'
report1 <- 'report1.csv'
report2 <- 'report2.csv'
report3 <- 'report3.csv'
output_file <- 'Members_Ratio_output.csv'
df <- read.csv(input_file1, header=T)
class(df)


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
dir_name <- paste(dir_name,'/',output_file,sep="")
write.csv(df_product_result,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################


################################### Report1 ###################################

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
  # 주문횟수
  df$주문횟수[i] <- length(temp)
  if(i==n) {break}
}

# 소수점 버린 평균값
MEAN_payment <- as.integer(mean(df$총.결제금액))

# 결과 저장 및 파일 추출
df_report1 <- data.frame(df$주문번호, df$주문일시, df$주문주기, df$주문자ID, df$총.결제금액, df$`주문횟수`)
names(df_report1) <- c("첫주문번호","첫주문일자","평균주문주기","ID","총주문금액","주문횟수")
df_report1 <- relocate(df_report1,c(ID,첫주문일자,첫주문번호,평균주문주기,주문횟수,총주문금액))

# 주문번호 기준 재정렬(주문시간 순)
df_report1<-df_report1[order(df_report1$첫주문번호),]

# 파일 생성
dir_name <- gsub(output_file,"",dir_name)
dir_name <- paste(dir_name,report1,sep="")
write.csv(df_report1,file=dir_name,row.names = FALSE,fileEncoding="cp949")

################################### Report2 ###################################
# 데이터프레임 선언
df_report2 <- data.frame(matrix(nrow=(max(df_report1$평균주문주기)%/%10)+2,ncol=2))
names(df_report2) <- c("주문주기","회원 수")
# 인자 초기화
for(i in 0:(length(df_report2$주문주기)-1)){
  df_report2$주문주기[i+1]<- paste((i-1)*10+1,"일~",i*10,"일",sep="")
}
df_report2$주문주기[1]<-'0일'
df_report2$`회원 수`[is.na(df_report2$`회원 수`)] <- 0
# 회원 수 count
for(i in 1:length(df_report1$ID)){
  if(df_report1$평균주문주기[i]==0){
    df_report2$`회원 수`[1]<-as.numeric(df_report2$`회원 수`[1])+1
  }
  else{
    j<-((df_report1$평균주문주기[i]-1)%/%10)+2
    df_report2$`회원 수`[j]<-as.numeric(df_report2$`회원 수`[j])+1
  }
}
for(i in 1:length(df_report2$`회원 수`)){
  df_report2$`회원 수`[i]<-paste(df_report2$`회원 수`[i],'명',sep="")
}

# 결과 출력
dir_name <- gsub(report1,"",dir_name)
dir_name <- paste(dir_name,report2,sep="")
write.csv(df_report2,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################

################################### Report3 ###################################
# 데이터프레임 선언
df_report3 <- data.frame(matrix(nrow=max(df_report1$주문횟수),ncol=2))
names(df_report3) <- c("주문횟수","회원 수")
# 인자 초기화
for(i in 1:length(df_report3$주문횟수)){
  df_report3$주문횟수[i]<- paste(i,"회",sep="")
}
df_report3$`회원 수`[is.na(df_report3$`회원 수`)] <- 0
# 회원 수 count
for(i in 1:length(df_report1$ID)){
  j<-df_report1$주문횟수[i]
  df_report3$`회원 수`[j]<-as.numeric(df_report3$`회원 수`[j])+1
}
for(i in 1:length(df_report3$`회원 수`)){
  df_report3$`회원 수`[i]<-paste(df_report3$`회원 수`[i],'명',sep="")
}
# 결과 출력
dir_name <- gsub(report2,"",dir_name)
dir_name <- paste(dir_name,report3,sep="")
write.csv(df_report3,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################