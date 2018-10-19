#fetch data, convert to 10*10 km and plot both, maybe interactively.


#To DO:
#- incrsuat video
#- limpiar datos (gen_sp, canarias, mar)
#- Añadir metadatos especies autor, link
#- Entrar datos en traitbase
#- funciones para query traitbase mejor.

#Gbif----
library(rgbif)
spain_code <- isocodes[grep("Spain", isocodes$name), "code"]
portugal_code <- isocodes[grep("Portugal", isocodes$name), "code"]
apidae_key <- name_backbone(name="Apidae", rank = "family")$usageKey
andrenidae_key <- name_backbone(name="Andrenidae", rank = "family")$usageKey
halictidae_key <- name_backbone(name="Halictidae", rank = "family")$usageKey
colletidae_key <- name_backbone(name="Colletidae", rank = "family")$usageKey
megachilidae_key <- name_backbone(name="Megachilidae", rank = "family")$usageKey
stenotritidae_key <- name_backbone(name="Stenotritidae", rank = "family")$usageKey
melittidae_key <- name_backbone(name="Melittidae", rank = "family")$usageKey

occ_count(taxonKey=c(apidae_key, andrenidae_key,
                     halictidae_key, colletidae_key,
                     megachilidae_key, stenotritidae_key, 
                     melittidae_key), 
          georeferenced=TRUE, 
          country=c(spain_code, portugal_code)) #not working with vectors¿?

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #1701
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #702
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #858
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #585
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #1453
occ_count(taxonKey= stenotritidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #0 (great, one less)
occ_count(taxonKey= melittidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #150

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #59
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #4
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #21
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #33
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #18
occ_count(taxonKey= stenotritidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #0 (great, one less)
occ_count(taxonKey= melittidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #0
#fetch data
dat <- data.frame(name = NA, decimalLatitude = NA,
                  decimalLongitude = NA, scientificName = NA,
                  family = NA, genus = NA, species = NA,
                  year = NA, month = NA, day = NA, recordedBy = NA,
                  identifiedBy = NA, sex = NA)
for(i in c(apidae_key, andrenidae_key,
           halictidae_key, colletidae_key,
           megachilidae_key, 
           melittidae_key)){
  temp <- occ_search(taxonKey= i, 
                    return='data', 
                    hasCoordinate=TRUE,
                    hasGeospatialIssue=FALSE,
                    limit=7000, #based on rounding up counts above
                    country = c(spain_code, portugal_code),
                    fields = c('name','decimalLatitude',
                               'decimalLongitude', 'scientificName',
                               'family','genus', 'species',
                               'year', 'month', 'day', 'recordedBy',
                               'identifiedBy', 'sex'))
  if(length(temp$PT) == 1){
    temp$PT <- data.frame(name = NA, decimalLatitude = NA,
                          decimalLongitude = NA, scientificName = NA,
                          family = NA, genus = NA, species = NA,
                          year = NA, month = NA, day = NA, recordedBy = NA,
                          identifiedBy = NA, sex = NA)
  }
  if(is.null(temp$ES$sex)){
    temp$ES$sex <- NA
  }
  if(is.null(temp$PT$sex)){
    temp$PT$sex <- NA
  }
  temp$ES <- temp$ES[,c('name','decimalLatitude',
                        'decimalLongitude', 'scientificName',
                        'family','genus', 'species',
                        'year', 'month', 'day', 'recordedBy',
                        'identifiedBy', 'sex')]
  temp$PT <- temp$PT[,c('name','decimalLatitude',
                        'decimalLongitude', 'scientificName',
                        'family','genus', 'species',
                        'year', 'month', 'day', 'recordedBy',
                        'identifiedBy', 'sex')]
  dat <- rbind(dat, as.data.frame(temp$ES), as.data.frame(temp$PT))
}
dat <- dat[-1,]
head(dat)
tail(dat)
dat <- dat[-nrow(dat),]

gbifmap(input = dat, region = c("Spain", "Portugal"))

#iNaturalist----
library(rinat)
bounds <- c(44.15, -10.13, 35.67, 4.76) #Spain
apidae <- get_inat_obs(taxon_name = "Apidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
andrenidae <- get_inat_obs(taxon_name = "Andrenidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
halictidae <- get_inat_obs(taxon_name = "Halictidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
colletidae <- get_inat_obs(taxon_name = "Colletidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
megachilidae <- get_inat_obs(taxon_name = "Megachilidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
melittidae <- get_inat_obs(taxon_name = "Melittidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
apidae$scientific_name #278
andrenidae$scientific_name #15 (most genus only)
halictidae$scientific_name #8
colletidae$scientific_name #1
megachilidae$scientific_name #42
melittidae$scientific_name #0

inat <- rbind(apidae, andrenidae, halictidae, colletidae, megachilidae)
head(inat)

#Traitbase----
library(traitbaser)
cnx <- connect("http://www.traitbase.info", "", "")
off <- resource(cnx, "species")

query(off)
query(off, conditions=buildCondition("species", "==", "Bombus")  )
query(off, limit=2, skip=0)
query(off, limit=2, skip=2)

#merge Gen sp plant lat long, date, credit----
colnames(dat)
colnames(inat)


#Clean----
#Duplicates
#species in canary islands
#species in the see
#species with only genus

#https://github.com/ropensci/scrubr

#Maybe add spocc? https://cran.r-project.org/web/packages/spocc/vignettes/spocc_vignette.html


#Quick map points----
head(dat)
library(mapr)
col <- unique(dat[, c("name", "family")]) 
col$id <- as.numeric(as.factor(col$family))
col <- merge(col, data.frame(id = 1:6, col = rainbow(6))) 
head(col)
map_leaflet(dat, lon = "decimalLongitude", lat = "decimalLatitude", 
            size = 6, color = col$col)
#hull(map_leaflet(dat$ES, lon = "decimalLongitude", lat = "decimalLatitude", 
              #   size = 6))


#create grid----

#data.df <- subset(data.df, subset=(LONGITUDE >= -180 & LATITUDE >= -90))
ji <- function(xy, origin=c(0,0), cellsize=c(1,1)) {
  t(apply(xy, 1, function(z) cellsize/2+origin+cellsize*(floor((z - origin)/cellsize))))
}
#JI <- ji(cbind(data.df$LONGITUDE, data.df$LATITUDE))
JI <- ji(cbind(dat$ES$decimalLongitude, dat$ES$decimalLatitude))
dat$ES$X <- JI[, 1]
dat$ES$Y <- JI[, 2]
dat$ES$Cell <- paste(dat$ES$X, dat$ES$Y)
counts <- by(dat$ES, dat$ES$Cell, 
             function(d) c(d$X[1], d$Y[1], length(unique(d$name))))
counts.m <- matrix(unlist(counts), nrow=3)
rownames(counts.m) <- c("X", "Y", "Count")

count.max <- max(counts.m["Count",])
colors = sapply(counts.m["Count",], function(n) hsv(sqrt(n/count.max), .7, .7, .5))
plot(counts.m["X",] + 1/2, counts.m["Y",] + 1/2, 
     #cex=sqrt(counts.m["Count",]),
     pch = 15, col=colors,
     xlab="Longitude of cell center", ylab="Latitude of cell center",
     main="Species richness within one-degree grid cells")
map(add = TRUE) #we are a bit off...
#better plotting


#map grid----

library(mapview)
#rasters
#mapView()

#fetch some data----

dat <- read.csv("_data/data.csv")
head(dat)
tail(sort(table(dat$species)), 100)

Megachile willughbiella 
Nomioides fortunatus
Colletes nigricans
Hoplitis benoisti
Megachile pyrenaica
Sphecodes gibbus
Owen
Heriades rubicola
Dioxis cincta

Megachile apicalis
Anthidium septemdentatum





