# Kevin Hon Yin Hau 2026-March
# 1. update to be more efficient by using paidOrNotNext to calculate next step from previous step
# 2. include maxPay for maturity, i.e. if loans stop at 24 months, the algorithm will have maximum of 24 payments
# 3. using getPymtPercTbl to output all the payments with probability for each step
# 4. calculate declineCurve differently from previous version, as expected payment for each month and it's more like decline / growth curve
#    where expectedPayt stays the same as total number of expected payment up to the current month.
# 5. not using data.table as to minimise the library


library(dplyr)
options(dplyr.summarise.inform = FALSE)

paidOrNotNext <- function(tbl, payCodeTbl, maxPay = 999){
  
  # input the tbl of previous step of calculation  to calculate the next step
  # payCodeTbl might change through different step and that's why this is taken out for individual function
  
  lenCode <- nchar(tbl$code[1])
  
  tbl_last <- tbl %>% 
    filter(numStep == max(tbl$numStep)) %>% 
    inner_join(payCodeTbl, c("code"="code"))
  
  out <- tbl %>% 
    union_all(
      tbl_last %>%
        mutate(code = paste(substr(code,2,lenCode),"1",sep=""), 
               payNum = if_else(payNum >= maxPay, maxPay, payNum + 1),
               prob = prob * paidPerc) %>% 
        union_all(tbl_last %>% 
                    mutate(code = paste(substr(code,2,lenCode),"0",sep=""), 
               prob = prob * notPaidPerc)) %>% 
        mutate(numStep = numStep+1) %>% 
        select(-paidPerc, -notPaidPerc)
    ) 
  out %>% group_by(numStep, code, payNum) %>% summarise(prob = sum(prob)) %>% ungroup()
  
}


paidOrNotPaid <- function(startCode, payCodeTbl, numMth, start_mth_num = 1, maxPay = 999){
  
  # loop each step for each individual startCode
  # output all the startCode, numStep, payNum, code, probability
  
  chk_cycle <- any(names(payCodeTbl) == "mth_num")
  
  if(!chk_cycle) pay_code <- payCodeTbl %>% select(code, paidPerc, notPaidPerc)
  
  tbl <- data.frame(numStep = 0, code=startCode, payNum=0, prob=1)
  
  for(i in 1:numMth){
    # in each step, take away the first part of the code and add on the paid or not paid, and assign probability depends on month
    if(chk_cycle){
      pay_code <- payCodeTbl %>% filter(mth_num == ((start_mth_num +i-1 ) %% max(payCodeTbl$mth_num)) + 1) %>% select(code, paidPerc, notPaidPerc) # do the cycle
    } 
    
    tbl <- paidOrNotNext(tbl, pay_code, maxPay)
  }
  
  tbl %>% mutate(startCode = startCode) %>% 
    group_by(startCode, numStep, payNum) %>% 
    summarise(prob = sum(prob)) %>% ungroup()
  
}


getPymtPercTbl <- function(payCodeTbl, numMth, start_mth_num = 1, maxPay = 999){
  
  # calculate each of all the start code, for each num Step, return the number of payments and its probability 
  calc_row_1_tbl <- length(unique(payCodeTbl$code))
  
  # for one of the starting code
  result <- paidOrNotPaid(payCodeTbl$code[1], payCodeTbl, numMth, start_mth_num, maxPay)
  
  for(i in 2:calc_row_1_tbl) {
    result <- result %>% union_all(
      paidOrNotPaid(payCodeTbl$code[i], payCodeTbl, numMth, start_mth_num, maxPay)
    )
  }
  
  result 
}

# Calculate all the decline curves with dynamic code
getModelDeclineCurve <- function(pymtPercTbl) {
  
  result <- pymtPercTbl %>% 
    group_by(startCode, numStep) %>% 
    summarise(expectedPayt = sum(payNum * prob))
  
  return(result %>% 
           left_join(result %>% 
                       mutate(numStep = numStep + 1,
                              expectedPaytLast = expectedPayt) %>% 
                       select(startCode, numStep, expectedPaytLast), 
                     by=c("startCode"="startCode","numStep"="numStep")) %>% 
           mutate(declineCurve = expectedPayt - coalesce(expectedPaytLast,0)) %>% 
           select(-expectedPaytLast)
  )
}
