library(dplyr) ; library(data.table); library(ggplot2)



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

StormData$EVTYPE[grepl("ICE", StormData$EVTYPE) | grepl("GLAZE", StormData$EVTYPE) | grepl("ICY", StormData$EVTYPE) ] <- "ICE"

StormData$EVTYPE[grepl("FLOOD", StormData$EVTYPE) | grepl("FLDG", StormData$EVTYPE) | grepl("RAPIDLY RISING WATER", StormData$EVTYPE)] <- "FLOOD"

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

StormData$EVTYPE[grepl("MUD", StormData$EVTYPE) ] <- "MUDSLIDE"

StormData$EVTYPE[grepl("STREAM", StormData$EVTYPE) ] <- "STREAM FLOOD"

StormData$EVTYPE[grepl("DRY", StormData$EVTYPE) ] <- "DRY"

StormData$EVTYPE[grepl("FUNNEL", StormData$EVTYPE) ] <- "FUNNEL CLOUD"

StormData$EVTYPE[grepl("WND", StormData$EVTYPE) ] <- "WIND"

StormData$EVTYPE[grepl("DAM", StormData$EVTYPE) ] <- "DAM FAILURE"

StormData$EVTYPE[grepl("RIP CURRENT", StormData$EVTYPE) ] <- "RIP CURRENT"

StormData$EVTYPE[grepl("HIGH SURF", StormData$EVTYPE) | grepl("HEAVY SURF", StormData$EVTYPE) | grepl("ROUGH SURF", StormData$EVTYPE) | grepl("HAZARDOUS SURF", StormData$EVTYPE)] <- "HEAVY SURF"

StormData$EVTYPE[grepl("STORM SURGE", StormData$EVTYPE) ] <- "STORM SURGE"

StormData$EVTYPE[grepl("COASTALSTORM", StormData$EVTYPE) ] <- "COASTAL STORM"

StormData$EVTYPE[grepl("AVALANC", StormData$EVTYPE) ] <- "AVALANCHE"

StormData$EVTYPE[grepl("HEAVY SEAS", StormData$EVTYPE) | grepl("ROUGH SEAS", StormData$EVTYPE)] <- "ROUGH SEAS"

StormData$EVTYPE[grepl("MARINE MISHAP", StormData$EVTYPE)] <- "MARINE ACCIDENT"

StormData$EVTYPE[grepl("HYPOTHERM", StormData$EVTYPE)] <- "HYPOTHERMIA"

StormData$EVTYPE[grepl("HYPERTHERMIA", StormData$EVTYPE)] <- "HYPERTHERMIA"

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


## Sort this in order.


WFIbyEVTYPE <- arrange(WFIbyEVTYPE, desc(WFI))

WFIbyEVTYPE$WFI[1]


#WFIbyEVTYPE$WFI[i] / sum(StormData$EVTYPE == WFIbyEVTYPE$EVTYPE[i])

for (i in 1: length(WFIbyEVTYPE$WFI)){
  WFIbyEVTYPE$NumberOfEvents[i] <- sum(StormData$EVTYPE == WFIbyEVTYPE$EVTYPE[i])
  WFIbyEVTYPE$WFIperEvent[i] <- WFIbyEVTYPE$WFI[i] / WFIbyEVTYPE$NumberOfEvents[i]
}
      


WFIbyEVTYPE <- WFIbyEVTYPE[WFIbyEVTYPE$NumberOfEvents >= 100]





        
p1 <- ggplot(WFIbyEVTYPE, aes(EVTYPE, WFI))
p1 <- p1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 270, hjust=0))
p1 <- p1 + labs(title = "Weighted Fatalities and Injuries by Event", x="Weather Event", y="Weighted Fatalities and Injuries")
  



p2 <- ggplot(WFIbyEVTYPE, aes(EVTYPE, WFIperEvent))
p2 <- p2 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 270, hjust = 0))
p2 <- p2 + labs(title = "Weighted Fatalities and Injuries by Event\ Divided by the number of weather events", x="Weather Event", y="Weighted Fatalities and Injuries")


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


multiplot(p1,p2, cols=1)

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