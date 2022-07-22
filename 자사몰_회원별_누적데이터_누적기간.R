# 4-6월 간 회원 데이터 등, cafe24를 통해 다운받은 파일을 입력하면,
# 해당 기간의 데이터를 기존 누적데이터에 추가하여
# 자사몰 회원별 [첫주문번호,첫주문일자,평균주문주기,ID,총주문금액,주문횟수]를 결과 파일로 출력해준다.

library(dplyr)

# 불러올 데이터프레임이 포함된 폴더 위치 line8 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀/자사몰_회원별_누적데이터/20.10.21_22.06')
dir_name <- getwd()

# 불러올 데이터 파일명 line13 '' 사이에 입력 / 위에 작성한 폴더안에 존재하는 파일이어야 함
# 기존 누적 데이터 파일명 변경했을 시, line14~line16 '' 사이 값 수정 또는 폴더 안의 파일명 수정
input_file <- '22.01-22.06.csv'
report1 <- '회원별_누적데이터.csv'
report2 <- '평균주문주기별 회원 수.csv'
report3 <- '주문횟수별 회원 수.csv'
df <- read.csv(input_file, header=T)
df_report1_origin <- read.csv(report1, header=T,fileEncoding = "cp949")
df_report2_origin <- read.csv(report2, header=T,fileEncoding = "cp949")
df_report3_origin <- read.csv(report3, header=T,fileEncoding = "cp949")


# 전처리 - 주문자ID 결측치 분리
df$주문자ID[df$주문자ID == ""] <- NA
df <- df[!is.na(df$주문자ID),]

# 데이터프레임에서 각각 필요한 변수만 추출 및 새로운 변수 추가
df <- df[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]
df[,"주문주기"] <- NA
df[,"주문횟수"] <- NA

# 전처리 - 총 결제금액이 0인 데이터 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
df <- df[!is.na(df$총.결제금액),]

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

df$주문일시<-as.Date(df$주문일시, '%Y-%m-%d')


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
    
    # df에서 아이디 중복행 삭제
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


############ 누적 데이터 병합 
# 전처리 - 기존 누적 데이터 실수화
df_report1_origin$평균주문주기 <- as.numeric(gsub("일","",df_report1_origin$평균주문주기))
df_report1_origin$총주문금액 <- as.numeric(gsub("원","",df_report1_origin$총주문금액))
df_report1_origin$주문횟수 <- as.numeric(gsub("회","",df_report1_origin$주문횟수))
df_report1_origin$첫주문일자<-as.Date(df_report1_origin$첫주문일자, '%Y-%m-%d')

# 누적 데이터에 신규 데이터 병합
df_report1 <- rbind(df_report1,df_report1_origin)

# 주문번호 기준 재정렬(주문시간 순)
df_report1<-df_report1[order(df_report1$첫주문번호),]

# 중복되는 아이디 정보 갱신
n <- length(df_report1$ID)

for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(paste("^",df_report1$ID[i],"$", sep=""), df_report1$ID)
  # 중복 존재 시
  if(length(temp)>1){
    # 결제금액 합치기
    df_report1$총주문금액[i] <- sum(df_report1$총주문금액[temp])
    
    # df_report1 첫주문일자 갱신
    df_report1$첫주문일자[i] <- min(df_report1$첫주문일자[temp])
    # line109를 통해 업데이트 된 첫주문일자와 동일한 주문의 주문번호로 첫주문번호 갱신
    df_report1$첫주문번호[i] <- df_report1$첫주문번호[temp[which.min(df_report1$첫주문일자[temp])]]
    
    # df_report1 주문주기 평균
    df_report1$평균주문주기[i] <- mean(df_report1$평균주문주기[temp])
    
    # 주문횟수 합산
    df_report1$주문횟수[i] <- sum(df_report1$주문횟수[temp])
    
    # df_report1 중복 삭제
    df_report1 <- df_report1[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  else if(length(temp)==1){
    df_report1$평균주문주기[i]<-mean(df_report1$평균주문주기[temp])
  }
  if(i==n) {break}
}

# 단위 기입이 필요한 열에 단위 추가
df_report1$평균주문주기 <- paste(as.character(df_report1$평균주문주기),"일", sep = "")
df_report1$총주문금액 <- paste(as.character(df_report1$총주문금액),"원", sep = "")
df_report1$주문횟수 <- paste(as.character(df_report1$주문횟수),"회", sep = "")

# 파일 생성
dir_name <- paste(dir_name,'/',report1,sep="")
write.csv(df_report1,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################

################################### Report2 ###################################
# 데이터프레임 선언
df_report1$평균주문주기 <- as.numeric(gsub("일","",df_report1$평균주문주기))
df_report2 <- data.frame(matrix(nrow=(max(df_report1$평균주문주기)%/%10)+2,ncol=2))
names(df_report2) <- c("주문주기","회원 수")
# 인자 초기화
for(i in 0:(length(df_report2$주문주기)-1)){
  df_report2$주문주기[i+1]<- paste((i-1)*10+1,"일~",i*10,"일",sep="")
}
df_report2$주문주기[1]<-'0일'
df_report2$`회원 수`[is.na(df_report2$`회원 수`)] <- 0
# 주문주기 구간별 회원 수 count
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
df_report1$주문횟수 <- as.numeric(gsub("회","",df_report1$주문횟수))
df_report3 <- data.frame(matrix(nrow=max(df_report1$주문횟수),ncol=2))
names(df_report3) <- c("주문횟수","회원 수")
# 인자 초기화
for(i in 1:length(df_report3$주문횟수)){
  df_report3$주문횟수[i]<- paste(i,"회",sep="")
}
df_report3$`회원 수`[is.na(df_report3$`회원 수`)] <- 0
# 주문횟수 구간별 회원 수 count
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