pkgname <- "sensorsUtils"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "sensorsUtils-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('sensorsUtils')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fortify_df")
### * fortify_df

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fortify_df
### Title: Create a fortified dataframe.
### Aliases: fortify_df

### ** Examples


#One column dataframe with silent lost observations
start <- as.POSIXct("2019/01/01")
end   <- as.POSIXct("2019/01/02")
dates <- seq(start, end, by = "hour")
df.simple <- data.frame(dates = dates[-(sample(1:25,5))], values=rep(1,20))
fortify_df(df=df.simple, by="hour")

#Multiple column dataframe with silent lost observations
df.complex <- data.frame(dates = dates[-(sample(1:25,5))], A = 1:20, B = LETTERS[1:20], C = rnorm(1:20))
fortify_df(df=df.complex, by="hour")

# Multiple datafrems with silent lost observations
df.A <- data.frame(dates = dates[-(sample(1:25,5))], a = 1:20)
df.B <- data.frame(dates = dates[-(sample(1:25,3))], a = 1:22)
df.C <- data.frame(dates = dates[-(sample(1:25,7))], a = 1:18)
fortify_df(list(df.A, df.B, df.C), by = "hour")



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fortify_df", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("make.aggregation")
### * make.aggregation

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: make.aggregation
### Title: Aggregate dataframe with a function.
### Aliases: make.aggregation

### ** Examples

# Mean of 3 day data by days
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df <- data.frame(dates = seq.POSIXt(a,b,"hour"), A = runif(24*3))
make.aggregation(df, freq = 24, mean)

# The same with multicolumn datadrame
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
              A = runif(24*3),
              B = rep(1:3,each=24))
make.aggregation(df, freq = 24, mean)

# The previus example but using a 12 hour mean
a = as.POSIXct("2019/01/01", tz = "UTC")
b = as.POSIXct("2019/01/03 23:00:00", tz = "UTC")
df<-data.frame(dates = seq.POSIXt(a,b,"hour"),
              A = runif(24*3),
              B = rep(1:3,each=24))
make.aggregation(df, freq = 12, mean)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("make.aggregation", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
