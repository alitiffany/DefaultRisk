library(testthat)
# library(quantlib)
library(data.table)
library(dplyr)
library(GCAMCPUB)
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
DBI::dbDisconnect(db_mo)
