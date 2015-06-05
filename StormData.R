library(dplyr) ; library(data.table); library(ggplot2); options(width=60)


if(!file.exists("./data/StormData.csv.bz2")){
        if(!file.exists("./data/")){dir.create("./data/")}
        url_data <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
        download.file(url_data, destfile="./data/StormData.csv.bz2", method="curl")
}



if(!which(ls() == "StormData") > 0){
        StormData <- data.table(read.csv(bzfile("./data/StormData.csv.bz2")))
}



###
# Remove unnecessary variables
###
names(StormData)

StormData <- select(StormData, c(STATE__:EVTYPE, LENGTH:CROPDMGEXP, REFNUM))

names(StormData)



####
# Aggregate different EVTypes
###

StormData$EVTYPE <- as.character(StormData$EVTYPE)

StormData$EVTYPE <- toupper(StormData$EVTYPE)

StormData <- StormData[!grepl("SUMMARY", StormData$EVTYPE)]

StormData$EVTYPE[grepl("THU", StormData$EVTYPE) | grepl("TSTM", StormData$EVTYPE)] <- "THUNDERSTORM"

StormData$EVTYPE[grepl("SNOW", StormData$EVTYPE) ] <- "SNOW"

StormData$EVTYPE[grepl("FLOOD", StormData$EVTYPE) ] <- "FLOOD"

StormData$EVTYPE[grepl("VOLCANIC", StormData$EVTYPE) ] <- "VOLCANIC"

StormData$EVTYPE[grepl("TORN", StormData$EVTYPE) ] <- "TORNADO"

StormData$EVTYPE[grepl("LIGHTN", StormData$EVTYPE) | grepl("LIGHTING", StormData$EVTYPE) | grepl("LIGNTNING", StormData$EVTYPE) ] <- "LIGHTNING"

StormData$EVTYPE[grepl("RAIN", StormData$EVTYPE) ] <- "RAIN"

StormData$EVTYPE[grepl("FIRE", StormData$EVTYPE) ] <- "FIRE"

StormData$EVTYPE[grepl("TORN", StormData$EVTYPE) ] <- "TORNADO"

StormData$EVTYPE[grepl("HURRIC", StormData$EVTYPE) ] <- "HURRICANE"

StormData$EVTYPE[grepl("HAIL", StormData$EVTYPE) ] <- "HAIL"

StormData$EVTYPE[grepl("BLIZZARD", StormData$EVTYPE) ] <- "BLIZZARD"

StormData$EVTYPE[grepl("WINT", StormData$EVTYPE) ] <- "WINTER STORM"

StormData$EVTYPE[grepl("WINTER STORM", StormData$EVTYPE) | grepl("BLIZZARD", StormData$EVTYPE) ] <- "WINTER STORM / BLIZZARD"

StormData$EVTYPE[grepl("TROPICAL STORM", StormData$EVTYPE) ] <- "TROPICAL STORM"

StormData$EVTYPE[grepl("TROPICAL", StormData$EVTYPE) ] <- "TROPICAL STORM"

StormData$EVTYPE[grepl("ICE", StormData$EVTYPE) | grepl("GLAZE", StormData$EVTYPE) | grepl("ICY", StormData$EVTYPE) ] <- "ICE"

StormData$EVTYPE[grepl("FLOOD", StormData$EVTYPE) | grepl("FLDG", StormData$EVTYPE) | grepl("RAPIDLY RISING WATER", StormData$EVTYPE)] <- "FLOOD"

StormData$EVTYPE[grepl("MICROBURST", StormData$EVTYPE) ] <- "MICROBURST"

StormData$EVTYPE[grepl("HAIL", StormData$EVTYPE) ] <- "HAIL"

StormData$EVTYPE[grepl("WIND", StormData$EVTYPE) ] <- "WIND"

StormData$EVTYPE[grepl("WATERSP", StormData$EVTYPE) ] <- "WATERSPOUT"

StormData$EVTYPE[grepl("COOL", StormData$EVTYPE) | grepl("COLD", StormData$EVTYPE) | grepl("HYPOTHERM", StormData$EVTYPE)] <- "COLD"

StormData$EVTYPE[grepl("HOT", StormData$EVTYPE) | grepl("WARM", StormData$EVTYPE) | grepl("HEAT", StormData$EVTYPE) | grepl("HIGH TEMP", StormData$EVTYPE) | grepl("HYPERTHERMIA", StormData$EVTYPE)] <- "HEAT"

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

StormData$EVTYPE[grepl("SLEET", StormData$EVTYPE)] <- "SLEET"

StormData$EVTYPE[grepl("FOG", StormData$EVTYPE)] <- "FOG"

StormData$EVTYPE[grepl("FOG", StormData$EVTYPE)] <- "FOG"

StormData$EVTYPE <- as.factor(as.character(StormData$EVTYPE))
levels(StormData$EVTYPE)

length(levels(StormData$EVTYPE))



###
# Creating a dataset that shows weather events with just injuries and fatalities
###
sum(StormData$INJURIES > 0 | StormData$FATALITIES > 0)
StormInjuriesFatalities <- filter(StormData, StormData$INJURIES > 0 | StormData$FATALITIES > 0)
dim(StormInjuriesFatalities)


StormInjuriesFatalities$WFI <- sapply(StormInjuriesFatalities$FATALITIES, function(x) x*10) + StormInjuriesFatalities$INJURIES

###
# New dataset aggregating WFI into different weather event types
###

WFIbyEVTYPE <- tapply(StormInjuriesFatalities$WFI, StormInjuriesFatalities$EVTYPE, sum, na.rm=TRUE)
WFIbyEVTYPE <- data.table(EVTYPE = names(WFIbyEVTYPE), WFI = WFIbyEVTYPE)


WFIbyEVTYPE <- WFIbyEVTYPE[!is.na(WFIbyEVTYPE$WFI)]


## Sort this in descending order.
WFIbyEVTYPE <- arrange(WFIbyEVTYPE, desc(WFI))


###
# Creating a relative variable indicating WFI per total number of events.
###


#WFIbyEVTYPE$WFI[i] / sum(StormData$EVTYPE == WFIbyEVTYPE$EVTYPE[i])

for (i in 1: length(WFIbyEVTYPE$WFI)){
  WFIbyEVTYPE$NumberOfEvents[i] <- sum(StormData$EVTYPE == WFIbyEVTYPE$EVTYPE[i])
  WFIbyEVTYPE$WFIperEvent[i] <- WFIbyEVTYPE$WFI[i] / WFIbyEVTYPE$NumberOfEvents[i]
}
      


WFIbyEVTYPE <- WFIbyEVTYPE[WFIbyEVTYPE$NumberOfEvents >= 100]


WFIbyEVTYPE <- arrange(WFIbyEVTYPE, desc(WFI)) 
TopInjuriesFatalities <- WFIbyEVTYPE[1:10]

p1 <- ggplot(WFIbyEVTYPE[1:10], aes(EVTYPE, WFI))
p1 <- p1 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 270, hjust=0))
p1 <- p1 + labs(title = "Weighted Fatalities and Injuries by Weather Event Type", x="", y="Weighted Fatalities and Injuries")


WFIbyEVTYPE <- arrange(WFIbyEVTYPE, desc(WFIperEvent)) 
MostLethalWeather <- WFIbyEVTYPE[1:10]


p2 <- ggplot(WFIbyEVTYPE[1:10], aes(EVTYPE, WFIperEvent))
p2 <- p2 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 270, hjust = 0))
p2 <- p2 + labs(title = "Weighted Fatalities and Injuries by Weather Event Type\nDivided by the Total Number of Each Weather Event Type", x="", y="Weighted Fatalities and Injuries") + 
        theme(plot.title = element_text(hjust = 0.5))

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

######
#####
#  Crop and Property Damage
#####
#####

sum(StormData$PROPDMG > 0 | StormData$CROPDMG > 0)



StormDamage <- filter(StormData, StormData$PROPDMG > 0 | StormData$CROPDMG > 0)
dim(StormDamage)

StormDamage$PROPDMGEXP <- as.character(StormDamage$PROPDMGEXP)
StormDamage$CROPDMGEXP <- as.character(StormDamage$CROPDMGEXP)

StormDamage$PROPDMGEXP <- toupper(StormDamage$PROPDMGEXP)
StormDamage$CROPDMGEXP <- toupper(StormDamage$CROPDMGEXP)

StormDamage$PROPDMGEXP[grepl("K", StormDamage$PROPDMGEXP)] <- "1000"
StormDamage$PROPDMGEXP[grepl("M", StormDamage$PROPDMGEXP)] <- "1000000"
StormDamage$PROPDMGEXP[grepl("H", StormDamage$PROPDMGEXP)] <- "100"
StormDamage$PROPDMGEXP[grepl("B", StormDamage$PROPDMGEXP)] <- "1000000000"


StormDamage$CROPDMGEXP[grepl("K", StormDamage$CROPDMGEXP)] <- "1000"
StormDamage$CROPDMGEXP[grepl("M", StormDamage$CROPDMGEXP)] <- "1000000"
StormDamage$CROPDMGEXP[grepl("H", StormDamage$CROPDMGEXP)] <- "100"
StormDamage$CROPDMGEXP[grepl("B", StormDamage$CROPDMGEXP)] <- "1000000000"


StormDamage$CROPDMGEXP<- as.numeric(StormDamage$CROPDMGEXP)
StormDamage$PROPDMGEXP<- as.numeric(StormDamage$PROPDMGEXP)

sum(is.na(StormDamage$PROPDMGEXP))
sum(is.na(StormDamage$CROPDMGEXP))



StormDamage$CROPDMGEXP[StormDamage$CROPDMGEXP == 0 & !is.na(StormDamage$CROPDMGEXP)] <- 1
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 1 & !is.na(StormDamage$PROPDMGEXP)] <- 10
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 2 & !is.na(StormDamage$PROPDMGEXP)] <- 100
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 3 & !is.na(StormDamage$PROPDMGEXP)] <- 1000
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 4 & !is.na(StormDamage$PROPDMGEXP)] <- 10000
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 5 & !is.na(StormDamage$PROPDMGEXP)] <- 100000
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 6 & !is.na(StormDamage$PROPDMGEXP)] <- 1000000
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 7 & !is.na(StormDamage$PROPDMGEXP)] <- 10000000
StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 8 & !is.na(StormDamage$PROPDMGEXP)] <- 100000000

StormDamage$PROPDMGEXP[StormDamage$PROPDMGEXP == 0 & !is.na(StormDamage$PROPDMGEXP)] <- 1

StormDamage$CROPDMG[is.na(StormDamage$CROPDMG)] <- 0
StormDamage$CROPDMGEXP[is.na(StormDamage$CROPDMGEXP)] <- 0
StormDamage$PROPDMG[is.na(StormDamage$PROPDMG)] <- 0  
StormDamage$PROPDMGEXP[is.na(StormDamage$PROPDMGEXP)] <- 0

StormDamage$CROPDMG <- StormDamage$CROPDMG * StormDamage$CROPDMGEXP
StormDamage$PROPDMG <- StormDamage$PROPDMG * StormDamage$PROPDMGEXP




StormDamage$TOTALDMG <- StormDamage$PROPDMG + StormDamage$CROPDMG




levels(StormDamage$EVTYPE)
names(DamageByEvent)
DamageByEvent<- tapply(StormDamage$TOTALDMG, StormDamage$EVTYPE, sum)
DamageByEvent<- data.table(EVTYPE = names(DamageByEvent), DAMAGE = DamageByEvent)
DamageByEvent<- DamageByEvent[!is.na(DamageByEvent$DAMAGE)]

DamageByEvent <- arrange(DamageByEvent, desc(DAMAGE))


p3 <- ggplot(DamageByEvent[1:15], aes(EVTYPE, DAMAGE))
p3 <- p3 + geom_bar(stat="identity") + theme(axis.text.x = element_text(angle = 270, hjust=0))
p3 <- p3 + labs(title = "Crop and Property Damage By Weather Event Type", x="", y="Dollars of Damage")



MostSevereEVTYPE<- DamageByEvent$EVTYPE[1:15]

HighestDamage <- StormDamage[StormDamage$EVTYPE %in% MostSevereEVTYPE]
HighestDamage$EVTYPE <- as.factor(as.character(HighestDamage$EVTYPE))



p5 <- qplot(EVTYPE, TOTALDMG, data=HighestDamage,na.rm = TRUE, geom="boxplot")
p5 <- p5 + scale_y_log10() + labs(title = "Crop and Property Damage By Weather Event Type", x="", y="Dollars of Damage") + theme(axis.text.x = element_text(angle = 270, hjust=0))



multiplot(p3,p5,cols=1)

# Variable PROPDMGEXP and CROPDMGEXP contain a factor to multiply the CROPDMG and PROPDMG 
#variables. Mostly it's H, K, and M for hundreds, thousands and millions, but there are 
#also numbers from 0 to 8 and some other random symbols. 


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