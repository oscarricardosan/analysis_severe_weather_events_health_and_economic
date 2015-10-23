setwd("D:/Proyectos/R/Reproducible Research/Asignment 2")

fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if(!file.exists("data/repdata-data-StormData.csv.bz2")) {
  download.file(
      fileUrl, "data/repdata-data-StormData.csv.bz2", 
      method = "auto", mode = "eb"
  )
}

if(!exists("dataPrin"))
  dataPrin <- read.csv("data/repdata-data-StormData.csv.bz2")

require(ggplot2)
dataPrin$BGN_DATE <- as.Date(dataPrin$BGN_DATE, "%m/%d/%Y")
dataPrin$year3 <- as.numeric(format(dataPrin$BGN_DATE,'%Y'))
dataPrin$year3 <- dataPrin$year3 - (dataPrin$year3 %% 3)



subsetTemp = subset(
  dataPrin[],
  BGN_DATE>=as.Date('2000-01-01')
)
nTop <- 3
topEventHealth <- aggregate(
  subsetTemp[,c('INJURIES','FATALITIES')],
  by = list(EVTYPE=subsetTemp$EVTYPE),#, STATE=subsetTemp$STATE
  FUN = sum
)
topEventHealth <- topEventHealth[
  order(-topEventHealth$INJURIES, -topEventHealth$FATALITIES),
  ][1:nTop,]
topEventHealth$levelDanger <- 1:nTop
topEventHealth$EVTYPE <- as.factor(as.character(topEventHealth$EVTYPE))

eventForHealth <- dataPrin[
  dataPrin$EVTYPE %in% topEventHealth$EVTYPE,
]
topTenHealtInjuries <- aggregate(
  eventForHealth[,c('INJURIES')],
  by = list(
    eventForHealth$EVTYPE, 
    eventForHealth$year3),#
  FUN = sum
)
names(topTenHealtInjuries)<-c('EVTYPE','year3','numPersons')
topTenHealtInjuries$typeHurt <- 'Injurie'
topTenHealtFatalities <- aggregate(
  eventForHealth[,c('FATALITIES')],
  by = list(
    EVTYPE=eventForHealth$EVTYPE, 
    year3=eventForHealth$year3),#
  FUN = sum
)
names(topTenHealtFatalities)<-c('EVTYPE','year3','numPersons')
topTenHealtFatalities$typeHurt <- 'Fatality'

topTenHealt<- rbind(topTenHealtFatalities, topTenHealtInjuries)

topTenHealt <- merge(topTenHealt, topEventHealth[,c('EVTYPE', 'levelDanger')])

topTenHealt$EVTYPE <- factor(topTenHealt$EVTYPE, levels = topEventHealth$EVTYPE)


##ECONOMICS
#crop damage  daños a los cultivos
#property damages   daños a la propiedad


topEventEconomic <- aggregate(
  subsetTemp[,c('PROPDMG','CROPDMG')],
  by = list(EVTYPE=subsetTemp$EVTYPE),#, STATE=subsetTemp$STATE
  FUN = sum
)
topEventEconomic <- topEventEconomic[
  order(-topEventEconomic$PROPDMG, -topEventEconomic$CROPDMG),
  ][1:nTop,]
topEventEconomic$levelDanger <- 1:nTop
topEventEconomic$EVTYPE <- as.factor(as.character(topEventEconomic$EVTYPE))

eventForEconomic <- dataPrin[
  dataPrin$EVTYPE %in% topEventEconomic$EVTYPE,
  ]
topTenEconomicPropdmg <- aggregate(
  eventForEconomic[,c('PROPDMG')],
  by = list(
    eventForEconomic$EVTYPE, 
    eventForEconomic$year3),#
  FUN = sum
)
names(topTenEconomicPropdmg)<-c('EVTYPE','year3','costDamage')
topTenEconomicPropdmg$typeHurt <- 'Propdmg'
topTenEconomicCropdmg <- aggregate(
  eventForEconomic[,c('CROPDMG')],
  by = list(
    EVTYPE=eventForEconomic$EVTYPE, 
    year3=eventForEconomic$year3),#
  FUN = sum
)
names(topTenEconomicCropdmg)<-c('EVTYPE','year3','costDamage')
topTenEconomicCropdmg$typeHurt <- 'Cropdmg'

topTenEconomic<- rbind(topTenEconomicCropdmg, topTenEconomicPropdmg)

topTenEconomic <- merge(topTenEconomic, topEventEconomic[,c('EVTYPE', 'levelDanger')])

topTenEconomic$EVTYPE <- factor(topTenEconomic$EVTYPE, levels = topEventEconomic$EVTYPE)


ggplot(
  data = topTenEconomic,
  aes(year3, costDamage/100, colour = typeHurt)
)+ geom_point()+ 
  facet_wrap(~EVTYPE, ncol = 2, scales = "free")+
  guides(col = guide_legend(ncol = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=seq(from = 1950,by = 3,to = 2011))+
  geom_smooth(method="lm", se = F, lty = 2)+
  scale_color_brewer(palette="Set1")+
  ylab('Cost/100')+xlab('')

ggplot(
  data = topTenHealt,
  aes(year3, numPersons, colour = typeHurt)
)+ geom_point()+ 
  facet_wrap(~EVTYPE, ncol = 2, scales = "free")+
  guides(col = guide_legend(ncol = 1))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks=seq(from = 1950,by = 3,to = 2011))+
  geom_smooth(method="lm", se = F, lty = 2)+
  scale_color_brewer(palette="Set1")+
  ylab('Number Persons')+xlab('')


dataPrin[
  dataPrin$EVTYPE %in% topEventEconomic$EVTYPE|dataPrin$EVTYPE %in% topEventHealth$EVTYPE
]

topEventEconomic 
topEventHealth