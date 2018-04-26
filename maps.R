# install necessary packages
library(dplyr)
library(maptools)
library(ggplot2)
#library(ggmap)  # only for superimposing with Google maps
library(doBy)

# to make fortify possible without errors
library(gpclib)
gpclibPermit()

# requires downloading and unzipping NUTS_2013_01M_SH.zip from
# http://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units
eurMap <- readShapeSpatial("NUTS_2013_01M_SH/data/NUTS_RG_01M_2013.shp")

# only main adm level
eurMap <- subset(eurMap, nchar(as.character(NUTS_ID)) == 2)

# EL for Grece, UK for United Kingdom
eurMapDf <- fortify(eurMap, region='NUTS_ID')

# we assume two columns: CountryCode and NGA_Coverage
eurData <- read.csv("Nuovo documento di testo.csv", stringsAsFactors = F)

View(eurData)

Elections_mean <- Elections_mean %>% rename("CountryCode" = CountryCode, "Populist index" = `NGA_Coverage`)

eurData <- Elections_mean

eurData
# merge map and data
eurMapDataDf <- merge(eurMapDf, eurData, by.x="id", by.y="CountryCode")
# sort, so that polygons are drawn correctly
eurMapDataDf <- eurMapDataDf[order(eurMapDataDf$order),]

# limit data to main Europe
eurMapDataDf <- subset(eurMapDataDf, long > -15 & long < 32 & lat > 34 & lat < 75)

# add text; instead of mean I do middle (not to be to biased towards detailed coastlines)
middle = function (x) {
  (max(x) + min(x)) / 2
}  
txtVal <- summaryBy(long + lat + `Populist index` ~ id, data=eurMapDataDf, FUN=middle, keep.names=T)
View(eurMapDataDf)
# inverse order (to have visible borders)

p <- ggplot(data=eurMapDataDf) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=`Populist index`)) +
  geom_path(aes(x=long, y=lat, group=group), color='black', alpha=.5) +
  geom_text(aes(x=long, y=lat, label=sprintf("%f", `Populist index`)), data=txtVal, col="gray", cex=3) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
  theme_bw() +
  coord_equal()


View(eurMapDataDf)


