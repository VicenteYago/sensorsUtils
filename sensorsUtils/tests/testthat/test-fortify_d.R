
#Check if a dataframe is already correct, no changes will be made.
test_that("basic test", {
  start <- as.POSIXct("2019/01/01")
  end   <- as.POSIXct("2019/01/02")
  dates <- seq(start, end, by = "hour")
  df <- data.frame(dates = dates, values = 1:25)
  res <-fortify_df(df = list(df), by = "hour", names = "values")
  expect_equal(res, df)
})

#Check fInteval works properly with a forced interval highly restrictive
test_that("basic test2", {
  start <- as.POSIXct("2019/01/01")
  end   <- as.POSIXct("2019/01/02")
  dates <- seq(start, end, by = "hour")
  df <- data.frame(dates = dates, values = 1:25)
  res <-fortify_df(df = list(df),
                   fInterval = c(start, as.POSIXct("2019/01/01 12:00:00")),
                   by = "hour", names = "values")
  expect_equal(res, df[1:13,])
})

#Check that multiple dataframe list fortifications works well
test_that("basic test3", {
  start <- as.POSIXct("2019/01/01")
  end   <- as.POSIXct("2019/01/02")
  dates <- seq(start, end, by = "hour")

  df.A <- data.frame(dates = dates, values = 1:25)
  df.B <- data.frame(dates = dates, values = 1:25)
  df.C <- data.frame(dates = dates, values = 1:25)
  df.D <- data.frame(dates = dates, values = 1:25)

  df.A<-df.A[-(sample(1:25,5)),]
  df.B<-df.B[-(sample(1:25,5)),]
  df.C<-df.C[-(sample(1:25,5)),]
  df.D<-df.D[-(sample(1:25,5)),]

  res <-fortify_df(df = list(df.A, df.B, df.C, df.D),
                   fInterval = c(start, end),
                   by = "hour")
  expect_equal(25, nrow(res))
})
