## ----env, include = FALSE-----------------------------------------------------
options(scipen=999)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----hypnogram, echo=FALSE, message=FALSE, warning=FALSE----------------------
library(rsleep)

if(!file.exists("15012016HD.csv")){
  download.file(
  url = "https://rsleep.org/data/15012016HD.csv",
  destfile = "15012016HD.csv",
  method="curl")}

events <- read_events_noxturnal("15012016HD.csv")

events = hypnogram(events)

plot_hypnogram(events)

## -----------------------------------------------------------------------------

events.vmrk = data.frame(Description = as.character(events$event))
events.vmrk$Description[events.vmrk$Description == "AWA"] = 0
events.vmrk$Description[events.vmrk$Description == "N1"] = 1
events.vmrk$Description[events.vmrk$Description == "N2"] = 2
events.vmrk$Description[events.vmrk$Description == "N3"] = 3
events.vmrk$Description[events.vmrk$Description == "REM"] = 5
events.vmrk$Description = as.integer(events.vmrk$Description)
events.vmrk$Type = "SleepStage"
events.vmrk = events.vmrk[,c(2,1)]

newdir <- file.path(tempdir(),"SleepCycles")

dir.create(newdir, showWarnings = FALSE)

write.table(
  events.vmrk, 
  file = paste(newdir, "events.txt", sep = "/"),
  row.names=FALSE,
  col.names = TRUE, 
  quote = FALSE, 
  sep = ",")



## -----------------------------------------------------------------------------

devtools::install_github("boupetch/SleepCycles")

cycles = SleepCycles::SleepCycles(
  p = newdir, 
  filetype = "txt", 
  plot = FALSE)

unlink(newdir, recursive=TRUE)

head(cycles)


## -----------------------------------------------------------------------------

hypnogram.full = cbind(events, cycles)

# Number of cycles
max(hypnogram.full$SleepCycle, na.rm = TRUE)
  
# Duration of each cycle
hypnogram.agg = aggregate(
  event ~ SleepCycle, 
  data = hypnogram.full, 
  FUN = length)
hypnogram.agg$minutes = hypnogram.agg$event/2
hypnogram.agg

# Composition of each cycle
cycles.comp = aggregate(
  SleepStages ~ SleepCycle + event, 
  data = hypnogram.full, 
  FUN = length)
cycles.comp = reshape(
  data = cycles.comp, 
  direction = "wide", 
  timevar  = "event",
  idvar  = "SleepCycle")
cycles.comp


