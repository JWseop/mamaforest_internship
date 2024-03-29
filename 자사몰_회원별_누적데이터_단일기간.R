# 4-6월 간 회원 데이터 등, cafe24를 통해 다운받은 파일을 입력하면,
# 해당 기간의 데이터 상에서
# 자사몰 회원별 [첫주문번호,첫주문일자,평균주문주기,ID,총주문금액,주문횟수]를 결과 파일로 출력해준다.

library(dplyr)

# 불러올 데이터프레임이 포함된 폴더 위치 line8 '' 사이에 입력
setwd('/Users/jungwooseop/모시공/행사/2022 하계 인턴십/문서/엑셀/자사몰_회원별_누적데이터/20.10.21_22.06')
dir_name <- getwd()

# 불러올 데이터 파일명 line13 '' 사이에 입력 / 위에 작성한 폴더안에 존재하는 파일이어야 함
# 결과 파일명 변경 필요 시 line14~line17 '' 사이 값 수정
input_file <- '20.10.21-20.12.csv'
report1 <- '회원별_누적데이터.csv'
report2 <- '평균주문주기별 회원 수.csv'
report3 <- '주문횟수별 회원 수.csv'
report4 <- '회원별_주문일시_누적데이터.csv'
df <- read.csv(input_file, header=T)


# 전처리 - 주문자ID 결측치 분리
df$주문자ID[df$주문자ID == ""] <- NA
df <- df[!is.na(df$주문자ID),]

# 데이터프레임에서 각각 필요한 변수만 추출 및 새로운 변수 추가
df <- df[,c("주문번호","주문상품명","수량","주문자ID","주문자.가입일","주문일시","총.결제금액")]
df[,"주문주기"] <- NA
df[,"주문주기_표준편차"] <- NA
df[,"주문횟수"] <- NA

# 전처리 - 총 결제금액이 0인 데이터 제거
df$총.결제금액[df$총.결제금액 == 0] <- NA
df <- df[!is.na(df$총.결제금액),]

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

# Create the function.
countmode <- function(v) {
  uniqv <- unique(v)
  charv<-uniqv[which.max(tabulate(match(v, uniqv)))]
  length(which(v==charv))
}

# #################### 회원별 주문일시 저장할 공간 생성
# # ncol=다른 주문번호 같은 아이디 최빈값의 개수, nrow=ID개수
m <- matrix(NA, ncol = countmode(df$주문자ID), nrow = length(unique(df$주문자ID)))
df_report4 <- as.data.frame(m)
rm(m)
names(df_report4) <- c(1:ncol(df_report4))
df_report4 <- cbind(ID=unique(df$주문자ID),df_report4)




#################### Result
# 회원별 월간 총 결제금액 연산
n <- length(df$주문자ID)

# 같은 아이디 열합치기
for(i in 1:n){
  #temp = 같은 아이디의 배열에서의 index 값 저장
  temp<-grep(paste("^",df$주문자ID[i],"$", sep=""), df$주문자ID)


################################ Report4 - 회원별 주문일시 누적 ################################
  ####### 주문일시 배열 저장
  # 임시 데이터프레임에 주문자ID[i]의 주문일시 저장
  df_temp <- as.data.frame(c(df$주문자ID[i],as.character(df$주문일시[temp])))
  names(df_temp) <- c(i)
  df_temp<-as.data.frame(t(df_temp))
  names(df_temp) <- c("ID",sum(!is.na(df_report4[i,])):(length(temp)+sum(!is.na(df_report4[i,]))-1))
  
  # 반복문에서 반복할 몇번째 주문인지에 관한 index값 지정
  start_index <- as.numeric(colnames(df_temp[2]))
  end_index   <- as.numeric(colnames(df_temp[length(temp)+1]))
  # start_index와 end_index가 몇번째 열인지 기입
  start_col_num <- start_index+1
  end_col_num <- end_index+1
  
  # index 위치가 NA일때 값 입력, 에러발생시 에러리포트 출력
  k<-2
  for(j in start_col_num:end_col_num){
    if(!is.na(df_report4[i,j])){
      print("ERROR:report4 is not NA")
      print(j)
      print("번째 index")
      break
    }
    if(is.na(df_report4[i,j])){
      df_report4[i,j] <- df_temp[1,k]
      k<-k+1
    }
  }
  # 임시 데이터 프레임 삭제
  rm(df_temp)
  
  # 중복 존재 시
  if(length(temp)>1){
    # 결제금액 합치기
    df$총.결제금액[i] <- sum(df$총.결제금액[temp])
    
    # df_report1 첫주문일자 갱신
    df$주문일시[i] <- df_report4[i,2]
    
    # df_temp에 df$ID[i] 아이디 주문일시 정보 입력
    df_temp<-df_report4[i,2:sum(!is.na(df_report4[i,]))]
    df_temp<-as.Date(t(df_temp),"%Y-%m-%d")
    
    # df 주문주기 평균
    df$주문주기[i] <- mean(diff(df_temp))
    
    # df 주문주기의 표준편차
    if(length(diff(df_temp))==1){
      df$주문주기_표준편차[i] <- ""
    }
    else{
      df$주문주기_표준편차[i] <- sd(diff(df_temp))
    }
    
    # 임시 데이터프레임 삭제
    rm(df_temp)
    
    # df에서 아이디 중복행 삭제
    df <- df[-temp[c(-1)],]
    n<-n-length(temp)+1
  }
  else if(length(temp)==1){
    df$주문주기[i]<-0
    df$주문주기_표준편차[i]<-""
  }
  # 주문횟수
  df$주문횟수[i] <- length(temp)
  if(i==n) {break}
}

# df_report4에서 행 전부가 NA값인 열 제거
df_report4 <- df_report4[,colSums(is.na(df_report4))<nrow(df_report4)]

# report4 - 회원별 주문일시 누적 결과 출력
df_report4[is.na(df_report4)]<-""
dir_name <- paste(dir_name,'/',report4,sep="")
write.csv(df_report4,file=dir_name,row.names = FALSE,fileEncoding="cp949")

# 결과 저장 및 파일 추출
df_report1 <- data.frame(df$주문번호, df$주문일시, df$주문주기, df$주문주기_표준편차, df$주문자ID, df$총.결제금액, df$`주문횟수`)
names(df_report1) <- c("첫주문번호","첫주문일자","평균주문주기(일)","주문주기_표준편차(일)","ID","총주문금액(원)","주문횟수(회)")
df_report1 <- relocate(df_report1,c(ID,첫주문일자,첫주문번호,`평균주문주기(일)`,`주문주기_표준편차(일)`,`주문횟수(회)`,`총주문금액(원)`))

# 주문번호 기준 재정렬(주문시간 순)
df_report1<-df_report1[order(df_report1$첫주문번호),]

# 평균주문주기 소숫점 자릿수 반올림
df_report1$`평균주문주기(일)` <- round(df_report1$`평균주문주기(일)`,1)

# 결과 출력
dir_name <- gsub(report4,"",dir_name)
dir_name <- paste(dir_name,report1,sep="")
write.csv(df_report1,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################


####################### Report2 - 평균주문주기별 회원수 #######################
# 데이터프레임 선언
df_report1$`평균주문주기(일)` <- as.numeric(df_report1$`평균주문주기(일)`)
df_report2 <- data.frame(matrix(nrow=(max(df_report1$`평균주문주기(일)`)%/%10)+2,ncol=2))
names(df_report2) <- c("주문주기(일)","회원 수(명)")

# 인자 초기화
for(i in 0:(length(df_report2$`주문주기(일)`)-1)){
  df_report2$`주문주기(일)`[i+1]<- paste((i-1)*10+1,"일~",i*10,"일",sep="")
}
df_report2$`주문주기(일)`[1]<-'0일'
df_report2$`회원 수(명)`[is.na(df_report2$`회원 수(명)`)] <- 0

# 주문주기 구간별 회원 수 count
for(i in 1:length(df_report1$ID)){
  if(df_report1$`평균주문주기(일)`[i]==0){
    df_report2$`회원 수(명)`[1]<-as.numeric(df_report2$`회원 수(명)`[1])+1
  }
  else{
    j<-((df_report1$`평균주문주기(일)`[i]-1)%/%10)+2
    df_report2$`회원 수(명)`[j]<-as.numeric(df_report2$`회원 수(명)`[j])+1
  }
}

# 결과 출력
dir_name <- gsub(report1,"",dir_name)
dir_name <- paste(dir_name,report2,sep="")
write.csv(df_report2,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################

######################### Report3 - 주문횟수별 회원수 #########################
# 데이터프레임 선언
df_report3 <- data.frame(matrix(nrow=max(df_report1$주문횟수),ncol=2))
names(df_report3) <- c("주문횟수(회)","회원 수(명)")

# 인자 초기화
for(i in 1:length(df_report3$`주문횟수(회)`)){
  df_report3$`주문횟수(회)`[i]<- i
}
df_report3$`회원 수(명)`[is.na(df_report3$`회원 수(명)`)] <- 0

# 주문횟수 구간별 회원 수 count
for(i in 1:length(df_report1$ID)){
  j<-df_report1$`주문횟수(회)`[i]
  df_report3$`회원 수(명)`[j]<-as.numeric(df_report3$`회원 수(명)`[j])+1
}

# 결과 출력
dir_name <- gsub(report2,"",dir_name)
dir_name <- paste(dir_name,report3,sep="")
write.csv(df_report3,file=dir_name,row.names = FALSE,fileEncoding="cp949")
###############################################################################