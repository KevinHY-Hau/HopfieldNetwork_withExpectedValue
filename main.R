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


