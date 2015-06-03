library(dplyr) ; library(data.table)



if(!file.exists("./data/")){dir.create("./data/")}
url_data <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
download.file(url_data, destfile="./data/StormData.csv.bz2", method="curl")
StormData <- data.table(read.csv(bzfile("./data/StormData.csv.bz2")))


names(StormData)

StormData <- select(StormData, c(STATE__:EVTYPE, LENGTH:CROPDMGEXP, REFNUM))

names(StormData)


StormData$EVTYPE <- toupper(StormData$EVTYPE)




StormData$EVTYPE <- as.character(StormData$EVTYPE)

StormData <- StormData[!grepl("SUMMARY", StormData$EVTYPE)]

StormData$EVTYPE[grepl("THU", StormData$EVTYPE) | grepl("TSTM", StormData$EVTYPE)] <- "THUNDERSTORM"

StormData$EVTYPE[grepl("SNOW", StormData$EVTYPE) ] <- "SNOW"

StormData$EVTYPE[grepl("FLOOD", StormData$EVTYPE) ] <- "FLOOD"

StormData$EVTYPE[grepl("VOLCANIC", StormData$EVTYPE) ] <- "VOLCANIC"

StormData$EVTYPE[grepl("TORN", StormData$EVTYPE) ] <- "TORNADO"

StormData$EVTYPE[grepl("LIGHTN", StormData$EVTYPE) ] <- "LIGHTNING"

StormData$EVTYPE[grepl("RAIN", StormData$EVTYPE) ] <- "RAIN"

StormData$EVTYPE[grepl("FIRE", StormData$EVTYPE) ] <- "FIRE"

StormData$EVTYPE[grepl("TORN", StormData$EVTYPE) ] <- "TORNADO"

StormData$EVTYPE[grepl("HURRIC", StormData$EVTYPE) ] <- "HURRICANE"

StormData$EVTYPE[grepl("HAIL", StormData$EVTYPE) ] <- "HAIL"

StormData$EVTYPE[grepl("BLIZZARD", StormData$EVTYPE) ] <- "BLIZZARD"

StormData$EVTYPE[grepl("WINT", StormData$EVTYPE) ] <- "WINTER STORM"

StormData$EVTYPE[grepl("TROPICAL STORM", StormData$EVTYPE) ] <- "TROPICAL STORM"

StormData$EVTYPE[grepl("TROPICAL", StormData$EVTYPE) ] <- "TROPICAL STORM"

StormData$EVTYPE[grepl("ICE", StormData$EVTYPE) ] <- "ICE"

StormData$EVTYPE[grepl("FLOOD", StormData$EVTYPE) | grepl("FLDG", StormData$EVTYPE)] <- "FLOOD"

StormData$EVTYPE[grepl("MICROBURST", StormData$EVTYPE) ] <- "MICROBURST"

StormData$EVTYPE[grepl("HAIL", StormData$EVTYPE) ] <- "HAIL"

StormData$EVTYPE[grepl("WIND", StormData$EVTYPE) ] <- "WIND"

StormData$EVTYPE[grepl("HAIL", StormData$EVTYPE) ] <- "HAIL"

StormData$EVTYPE[grepl("WATERSP", StormData$EVTYPE) ] <- "WATERSPOUT"

StormData$EVTYPE[grepl("COOL", StormData$EVTYPE) | grepl("COLD", StormData$EVTYPE)] <- "COLD"

StormData$EVTYPE[grepl("HOT", StormData$EVTYPE) | grepl("WARM", StormData$EVTYPE) | grepl("HEAT", StormData$EVTYPE) | grepl("HIGH TEMP", StormData$EVTYPE)] <- "HEAT"

StormData$EVTYPE[grepl("HEAVY PR", StormData$EVTYPE) | grepl("HEAVY SH", StormData$EVTYPE)] <- "THUNDERSTORM"

StormData$EVTYPE[grepl("FREEZ", StormData$EVTYPE) ] <- "FREEZE"

StormData$EVTYPE[grepl("DUST", StormData$EVTYPE) ] <- "DUST"

StormData$EVTYPE[grepl("MUD", StormData$EVTYPE) ] <- "MUD SLIDE"

StormData$EVTYPE[grepl("STREAM", StormData$EVTYPE) ] <- "STREAM FLOOD"

StormData$EVTYPE[grepl("DRY", StormData$EVTYPE) ] <- "DRY"

StormData$EVTYPE[grepl("FUNNEL", StormData$EVTYPE) ] <- "FUNNEL CLOUD"

StormData$EVTYPE[grepl("WND", StormData$EVTYPE) ] <- "WIND"

StormData$EVTYPE[grepl("DAM", StormData$EVTYPE) ] <- "DAM FAILURE"

StormData$EVTYPE[grepl("RIP CURRENT", StormData$EVTYPE) ] <- "RIP CURRENT"

StormData$EVTYPE[grepl("HIGH SURF", StormData$EVTYPE) | grepl("HEAVY SURF", StormData$EVTYPE) | grepl("ROUGH SURF", StormData$EVTYPE)] <- "HEAVY SURF"

StormData$EVTYPE[grepl("STORM SURGE", StormData$EVTYPE) ] <- "STORM SURGE"

StormData$EVTYPE[grepl("AVALANC", StormData$EVTYPE) ] <- "AVALANCHE"

StormData$EVTYPE <- as.factor(as.character(StormData$EVTYPE))
StormLevels <- levels(StormData$EVTYPE)


length(levels(StormData$EVTYPE))



sum(StormData$INJURIES > 0 | StormData$FATALITIES > 0)
StormInjuriesFatalities <- filter(StormData, StormData$INJURIES > 0 | StormData$FATALITIES > 0)
dim(StormInjuriesFatalities)


StormInjuriesFatalities$WFI <- sapply(StormInjuriesFatalities$FATALITIES, function(x) x*10) + StormInjuriesFatalities$INJURIES


WFIbyEVTYPE <- tapply(StormInjuriesFatalities$WFI, StormInjuriesFatalities$EVTYPE, sum, na.rm=TRUE)
WFIbyEVTYPE <- data.table(EVTYPE = names(WFIbyEVTYPE), WFI = WFIbyEVTYPE)


WFIbyEVTYPE <- WFIbyEVTYPE[!is.na(WFIbyEVTYPE$WFI)]

#The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions 
#about severe weather events. You must use the database to answer the questions below and show the code 
#for your entire analysis. Your analysis can consist of tables, figures, or other summaries. 
#You may use any R package you want to support your analysis.



#Your data analysis must address the following questions:
#  1	Across the United States, which types of events (as indicated in the EVTYPE variable) 
#    are most harmful with respect to population health?
#  2	Across the United States, which types of events have the greatest economic consequences?
#Consider writing your report as if it were to be read by a government or municipal manager who 
#might be responsible for preparing for severe weather events and will need to prioritize resources 
#for different types of events. However, there is no need to make any specific recommendations in your report.