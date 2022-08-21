# this example is based on 2 months of pay (1) or not pay (0)
# where cnt is total count of the 2 months sample 
# and output is the following month who pay
x <- data.frame(code = c("11","10","01","00"), 
                cnt  = c(1000, 1000, 1000, 1000),
                output = c(900, 500, 300, 50)
                )
# 3 months of pay/not pay code will have 8 rows, and 4 months of pay/not pay code will have 16 rows 

getCodeTbl <- function(dt){
  data.frame(code = dt$code, 
             total = as.integer(dt$cnt),
             paidPerc =as.integer(dt$output)/as.integer(dt$cnt), 
             notPaidPerc = 1-as.integer(dt$output)/as.integer(dt$cnt), 
             stringsAsFactors = FALSE) %>% arrange(code)
}

y <- getCodeTbl(x)

# > y
#   code total paidPerc notPaidPerc
# 1   00  1000     0.05        0.95
# 2   01  1000     0.30        0.70
# 3   10  1000     0.50        0.50
# 4   11  1000     0.90        0.10

getModelDeclineCurve(y, 5)

#     startCode numStep expectedPayt declineCurve
#  1:        00       1    0.0500000   0.05000000
#  2:        00       2    0.1125000   0.05625000
#  3:        00       3    0.2028750   0.06762500
#  4:        00       4    0.3077562   0.07693906
#  5:        00       5    0.4312097   0.08624194
#  6:        01       1    0.3000000   0.30000000
#  7:        01       2    0.9200000   0.46000000
#  8:        01       3    1.3005000   0.43350000
#  9:        01       4    1.7768250   0.44420625
# 10:        01       5    2.1655738   0.43311475
# 11:        10       1    0.5000000   0.50000000
# 12:        10       2    0.6750000   0.33750000
# 13:        10       3    1.0162500   0.33875000
# 14:        10       4    1.2516875   0.31292188
# 15:        10       5    1.5422906   0.30845812
# 16:        11       1    0.9000000   0.90000000
# 17:        11       2    1.7600000   0.88000000
# 18:        11       3    2.5515000   0.85050000
# 19:        11       4    3.2979750   0.82449375
# 20:        11       5    3.9933463   0.79866925

