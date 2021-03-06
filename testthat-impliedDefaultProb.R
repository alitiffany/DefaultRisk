library(testthat)
# library(quantlib)
library(data.table)
library(dplyr)
library(GCAMCPUB)
source('D:/RWD/DefaultRisk/impliedDefaultProb.R')
defProb <- impliedDefaultProb$new()
defProb$updateData()
defProb$saveData("D:/defProb.rds")

defProb <- impliedDefaultProb$new("D:/defProb.rds")
defProb$readVal(as.Date("2015-11-05"), 24437L)

dplyr::sample_n(defProb$data$univ, 10)
defProb$calc(as.Date("2015-11-05"), c("124636.SH", "101559030.IB"), 0.4)

db_mo <- GCAMCPUB::activate_conn(db_mo)
sql <- "select distinct Date, Sec_Code, Sec_Name from CORE_Data_Holding
where Date = '2015-11-10' and if_Standard_Bond = 1 and if_Riskfree = 0"
r <- DBI::dbGetQuery(db_mo, sql) %>% dplyr::mutate(Date = GCAMCPUB::to_date(Date))
res <- defProb$calc(r$Date, r$Sec_Code, 0.3)

### na in res ###
res_na <- res[is.na(ImpliedDefaultProb)]

## there are no WindCodes in bond universe ##
res_wind <- defProb$data$univ[res_na[is.na(InnerCode), WindCode]]

## other reasons ##

# liquidity

res_liquidity <- res_na[!is.na(InnerCode) & Liquidity == 1]

# others(in fact, maturity dates of these bonds are also in 2016-11)

res_remain <- res_na[!is.na(InnerCode) & is.na(Liquidity)]

## test the amount ##
res_non_na <- res[!is.na(ImpliedDefaultProb)]



DBI::dbDisconnect(db_mo)


