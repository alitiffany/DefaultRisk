#' Implied Default Probability (Market-Netral) Calculator
#'
#' A implied default probability calculator based on the observed market price. The
#' methodology is mainly based on \emph{Introducing the J.P. Morgan Implied
#' Default Probability Model: A Powerful Tool for Bond Valuation September 20,
#' 2000}. We opt the simple version of this calculation, which assumes a static
#' default prob.
#'
#' The model provides
#' \enumerate{
#'    \item a calculator, in which you input cash flows and the spot rate,
#'    it would return the solve of the implied default rate.
#'    \item a data vendor, provides the projected cash flows as well as
#'    spot rates in China markets, so that the calculation would be completed
#'    in seconds.
#'    \item a wapper functions for calculate the implied default rate with
#'    the code and date. It will read the cash flow, price and sport rate data
#'    from the data vendor defined above.
#' }
#' @field data The data source that contains the price, cash flows as well as the
#' sport rate yield curve of government bond.
#'
#' @section Methods:
#'
#' \describe{
#'    \item{\code{new(rdsPath = NULL)}}{
#'      Establish a new entity. If the rdsPath is not \code{NULL}, it will load the
#'      data at the same time.
#'    }
#'    \item{\code{updateData}}{
#'      update the \code{data} field from JuYuan database, it contains three objects,
#'      that are: \code{univ} for the China Bond Universe, \code{bondCF} for bonds'
#'      projected cash flows, \code{gbSpotYield} for risk fress spot yield curve data.
#'    }
#'    \item{\code{readVal(dates, innerCodes)}}{
#'      read the China bond full valuation from JuYuan database, providing the
#'      corresponding dates and innerCodes, it will reads the last available valuations.
#'    }
#'    \item{\code{calc(dates, windCodes, recoverValue = 0.2)}}{
#'
#'    }
#'    \item{\code{calcRaw(price, k, cf, rf, rv))}}{
#'
#'    }
#'    \item{\code{calRFR(date, k)}}{
#'      calculate the needed risk-free rate based on the \code{self$data$gbSpotYield}
#'      and using the interplotation model \emph{Hermite}.
#'    }
#'    \item{\code{loadData(rdsPath)}}{Load the data from \code{rdsPath}.}
#'    \item{\code{saveData(rdsPath)}}{Save the data to \code{rdsPath}.}
#' }
#'
#' @export
impliedDefaultProb <- R6::R6Class(
  "impliedDefaultProb",
  public = list(
    data = list(),
    loadData = function(rdsPath) {
      self$data <- readRDS(rdsPath)
    },
    saveData = function(rdsPath) {
      saveRDS(self$data, rdsPath)
    },
    updateData = function() {
      # bond universe
      GCAMCPUB::f_msg("read bond universe...")
      univSql <-
        "select b.InnerCode, b.MainCode, a.SecuCode, a.SecuAbbr,
        a.SecuMarket, b.ListedDate from
        JYDB.dbo.SecuMain a inner join JYDB.dbo.Bond_Code b
        on a.InnerCode = b.InnerCode
        where a.SecuMarket in (83, 89, 90)
        and b.BondNature in (1, 2, 3, 13, 14, 23, 24, 8)"
      univ <-
        local({
          sql <- univSql
          r <-
            DBI::dbGetQuery(private$db_jy, sql) %>%
            dplyr::mutate(ListedDate = GCAMCPUB::to_date(ListedDate)) %>%
            data.table::setDT(key = c("SecuCode", "SecuMarket"))
          r <-
            private$mktSuffix[r, on = "SecuMarket"][
              , c("WindCode", "CodeSuffix") :=
                list(paste0(SecuCode, CodeSuffix), NULL)][]
          setkey(r, WindCode)
          r[]
        })
      # bond cash flows
      GCAMCPUB::f_msg("read bond cash flows...")
      bondCF <-
        local({
          sql <-
            "select InnerCode, PaymentDate, InterestPer, PaymentPer, CashFlowType from
          JYDB.dbo.Bond_CashFlow where InnerCode in (select InnerCode from (%s) e)"
          sql <- sprintf(sql, univSql)
          r <-
            DBI::dbGetQuery(private$db_jy, sql) %>%
            dplyr::mutate(PaymentDate = GCAMCPUB::to_date(PaymentDate)) %>%
            data.table::setDT(key = c("InnerCode", "PaymentDate"))
          stopifnot(r$CashFlowType %in% c(1L, 2L))
          r <-
            r[, .(InnerCode, PaymentDate, CashFlow = ifelse(
              CashFlowType == 1L, InterestPer, PaymentPer
            ))]
          stopifnot(nrow(
            r[, list(Ct = .N), by = .(InnerCode, PaymentDate)][Ct > 1]
          ) > 0)
          r[]
        })

      # government bond spot yield curve
      GCAMCPUB::f_msg("read risk free spot yield...")
      gbSpotYield <-
        local({
          sql <- "select EndDate, YearsToMaturity, Yield from JYDB.dbo.Bond_CBYieldCurve
          where CurveCode = 10 and YieldTypeCode = 2 and EndDate >= '2009-12-31'"
          r <- DBI::dbGetQuery(private$db_jy, sql) %>%
            dplyr::mutate(EndDate = GCAMCPUB::to_date(EndDate)) %>%
            data.table::setDT(key = "EndDate")
          r[]
        })
      # end
      self$data <- mget(c("univ", "bondCF", "gbSpotYield"))
      GCAMCPUB::f_msg("successfully load to self$data.")
      invisible()
    },
    val = function(lamda, k, cf, rf, rv) {
      sum(
        ((1 + rf) ^ -k) * exp(-k * lamda) * (cf + rv * 100 * (exp(k * lamda) - 1))
      )
    },
    calcRaw = function(price, k, cf, rf, rv) {
      tryCatch(
        stats::uniroot(
          function(...) self$val(...) - price,
          interval = c(0, 1), k = k, cf = cf, rf = rf, rv = rv,
          tol = 1e-10, maxiter = 1e+05
        )$root,
        error = function(e) NA_real_
      )
    },
    calc = function(dates, windCodes, recoverValue = 0.2) {
      stopifnot("Date" %in% class(dates),
                length(dates) == length(windCodes) | length(dates) == 1,
                typeof(windCodes) == "character",
                is.numeric(recoverValue),
                length(recoverValue) == 1 | length(dates) == length(recoverValue))
      # inner code
      r <- self$data$univ[J(windCodes),
                          .(WindCode, InnerCode, MainCode, SecuAbbr,
                            EndDate = dates, RecoverValue = recoverValue)]
      # valuation price, yield
      bondVal <- self$readVal(r$EndDate, r$InnerCode)
      r <- bondVal[r, on = c("InnerCode", "EndDate"), roll = TRUE]
      # prepare
      r[, ImpliedDefaultProb := NA_real_]
      for (i in seq_len(nrow(r))) {
        price <- r[i, ValueFullPrice]
        if (is.na(price)) next
        # cash flow
        cf <- self$data$bondCF[r[i, .(InnerCode)], .(PaymentDate, CashFlow), nomatch = 0]
        if (nrow(cf) == 0) next
        # k
        k <- as.integer(cf$PaymentDate - r[i, EndDate]) / 365
        # risk free spot yield
        rf <- self$calRFR(r[i, EndDate], k)
        if (anyNA(rf)) next
        # prepare input
        cf <- cf$CashFlow
        rv <- r[i, recoverValue]
        # cal
        data.table::set(r, i, "ImpliedDefaultProb", self$calcRaw(price, k, cf, rf, rv))
      }
      # return
      r[]
    },
    readVal = function(dates, innerCodes) {
      # check the input
      stopifnot("Date" %in% class(dates),
                length(dates) == length(innerCodes),
                typeof(innerCodes) == "integer")
      # bond valuation data
      bondVal <-
        local({
          sql <-
            "select InnerCode, EndDate, VPYield,
          ValueCleanPrice, ValueFullPrice from
          JYDB.dbo.Bond_CBValuation where InnerCode in (%s)
          and EndDate >= '%s' and EndDate <= '%s'"
          sql <-
            sprintf(
              sql,
              paste0(unique(innerCodes[!is.na(innerCodes)]), collapse = ","),
              format(min(dates) - 30L, "%Y-%m-%d"),
              format(max(dates) + 0L, "%Y-%m-%d"))
          r <-
            DBI::dbGetQuery(private$db_jy, sql) %>%
            dplyr::mutate(EndDate = GCAMCPUB::to_date(EndDate)) %>%
            data.table::setDT(key = c("InnerCode", "EndDate"))
          r[]
        })
      r <- data.table(InnerCode = innerCodes, EndDate = dates,
                      key = c("InnerCode", "EndDate"))
      bondVal[r, roll = TRUE, nomatch = 0]
    },
    calRFR = function(date, k) {
      stopifnot(length(date) == 1, lubridate::is.Date(date),
                is.numeric(k), length(k) > 1)
      rf <- self$data$gbSpotYield[J(date), .(YearsToMaturity, Yield),
                                  roll = TRUE, nomatch = 0]
      tryCatch(
        stats::splinefun(rf$YearsToMaturity, rf$Yield, method = "monoH.FC")(k),
        error = function(e) return(rep(NA_real_, length(k)))
      )
    },
    initialize = function(rdsPath = NULL) {
      # load data
      if (!is.null(rdsPath)) {
        self$loadData(rdsPath)
        message("Load data from ", rdsPath, " successfully.")
      }
      # connect database
      private$db_jy <- GCAMCPUB::activate_conn(db_jy)
      # reg database connection disconnect
      reg.finalizer(
        self,
        function(e) DBI::dbDisconnect(private$db_jy),
        onexit = TRUE
      )
    }
  ),
  private = list(
    mktSuffix = data.table(
      SecuMarket = c(83L, 89L, 90L),
      CodeSuffix = c(".SH", ".IB", ".SZ"),
      key = "SecuMarket"
    ),
    db_jy = NULL
  )
)
