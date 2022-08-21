getModelDeclineCurve <- function(tbl, numMth){
  require(dplyr)
  require(data.table)
  paidOrNotPaid <- function(startCode, payCodeTbl, numMth){
    tbl <- data.table(step = 0, code=startCode, payNum=0, prob=1)
    resultList <- rep(0,numMth)
    lenCode <- nchar(startCode)
    for(i in 1:numMth){
      tbl <- tbl %>% inner_join(payCodeTbl, c("code"="code")) %>% 
        mutate(code = paste(substr(code,2,lenCode),"1",sep=""), payNum = payNum + 1,
               prob = prob * paidPerc) %>% 
        union_all(tbl %>% inner_join(payCodeTbl, c("code"="code")) %>% 
                    mutate(code = paste(substr(code,2,lenCode),"0",sep=""),
                           prob = prob * notPaidPerc)) %>% 
        mutate(step = step+1) %>% select(-paidPerc, -notPaidPerc) %>% 
        group_by(step, code, payNum) %>% summarise(prob = sum(prob)) %>% 
        arrange(desc(payNum), code) %>% ungroup(step, code, payNum)
      
      resultList[i] <- tbl %>% summarise(ep = sum(payNum*prob)) %>% pull(ep)
    }
    data.table(startCode = startCode,
               numStep = 1:numMth, 
               expectedPayt = resultList)
  }
  
  result <- paidOrNotPaid(tbl$code[1], data.table(tbl), numMth)
  
  for(i in 2:nrow(tbl)) {
    result <- result %>% union_all(paidOrNotPaid(tbl$code[i],tbl,numMth))
  }
  return(result %>% mutate(declineCurve = expectedPayt/numStep))
}


getModelDeclineCurve12Mth <- function(tbl, numMth, start_mth_num){
  
  # for one of the starting code
  paidOrNotPaid <- function(startCode, payCodeTbl, numMth, start_mth_num){
    
    require(tidyverse)
    require(data.table)
    
    mth_num_curr <- start_mth_num
    
    tbl <- data.table(step = 0, code=startCode, payNum=0, prob=1)
    resultList <- rep(0,numMth)
    lenCode <- nchar(startCode)
    for(i in 1:numMth){
      # in each step, take away the first part of the code and add on the paid or not paid, and assign probarility depends on month
      # then group it by their total payment number
      tbl <- tbl %>% inner_join(payCodeTbl %>% filter(mth_num==mth_num_curr) %>% select(-mth_num) , c("code"="code")) %>% 
        mutate(code = paste(substr(code,2,lenCode),"1",sep=""), payNum = payNum + 1, prob = prob * paidPerc) %>% 
        union_all(tbl %>% inner_join(payCodeTbl %>% filter(mth_num==mth_num_curr) %>% select(-mth_num), c("code"="code")) %>% 
                    mutate(code = paste(substr(code,2,lenCode),"0",sep=""), prob = prob * notPaidPerc)) %>% 
        mutate(step = step+1) %>% select(-paidPerc, -notPaidPerc) %>% 
        group_by(step, code, payNum) %>% summarise(prob = sum(prob)) %>% 
        arrange(desc(payNum), code) %>% ungroup(step, code, payNum)
    
      resultList[i] <- tbl %>% summarise(ep = sum(payNum*prob)) %>% pull(ep)
      
      mth_num_curr <- ifelse(mth_num_curr==12,1,mth_num_curr+1)
    }
    data.table(startCode = startCode,
               numStep = 1:numMth, 
               expectedPayt = resultList)
  }
  
  result <- paidOrNotPaid(tbl$code[1], data.table(tbl), numMth, start_mth_num)
  
  for(i in 2:(nrow(tbl)/12)) {
    result <- result %>% union_all(paidOrNotPaid(tbl$code[i],tbl,numMth, start_mth_num))
  }
  return(result %>% mutate(declineCurve = expectedPayt/numStep))
}


