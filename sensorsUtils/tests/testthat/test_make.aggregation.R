#Test if a simple daily aggregation works correctly
test_that("basic aggregation",{
  start <- as.POSIXct("2019/01/01")
  df<-data.frame(time = seq(start, by = "1 hour", length.out = 48),
                 A = c(rep(1,24),rep(2,24)),
                 B = c(rep(1,24),rep(2,24)))
  df<-make.aggregation(df, freq = 24, fun = mean)

  res <- data.frame(dates = seq(start, by = "1 day", length.out = 2),
                       A = c(1,2),
                       B = c(1,2))
  expect_equal(res, df)
})

#Test if when all the data is aggregated in a single row the resulting
# dataframe is presented in a correct format
test_that("basic aggregation",{
  start <- as.POSIXct("2019/01/01")
  df<-data.frame(time = seq(start, by = "1 hour", length.out = 48),
                 A = c(rep(1,24),rep(2,24)),
                 B = c(rep(1,24),rep(2,24)))
  df<-make.aggregation(df, freq = 48, fun = mean)

  res <- data.frame(dates = seq(start, by = "1 day", length.out = 1),
                    A = c(1.5),
                    B = c(1.5))
  expect_equal(res, df)
})
