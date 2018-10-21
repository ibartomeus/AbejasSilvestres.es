#fetch data, convert to 10*10 km and plot both, maybe interactively.


#To DO:
#- limpiar datos (gen_sp: sinominos, canarias: OK, mar: todo!)
#- Entrar datos en traitbase (por ahora Beefun OK + asensio OK + Out + Cap Creus? + Gredos?)
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

#occ_count(taxonKey=c(apidae_key, andrenidae_key,
 #                    halictidae_key, colletidae_key,
 #                    megachilidae_key, stenotritidae_key, 
 #                     melittidae_key), 
 #          georeferenced=TRUE, 
 #          country=c(spain_code, portugal_code)) #not working with vectors¿?

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #1947
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #706
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #858
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #587
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #1526
occ_count(taxonKey= stenotritidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #0 (great, one less)
occ_count(taxonKey= melittidae_key, 
          georeferenced=TRUE, 
          country=spain_code) #177

occ_count(taxonKey= apidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #587
occ_count(taxonKey= andrenidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #9
occ_count(taxonKey= halictidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #223
occ_count(taxonKey= colletidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #120
occ_count(taxonKey= megachilidae_key, 
          georeferenced=TRUE, 
          country=portugal_code) #119
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
dim(dat) #6859

#gbifmap(input = dat, region = c("Spain", "Portugal"))

#iNaturalist----
library(rinat)
bounds <- c(44.15, -10.13, 35.67, 4.76) #Spain
apidae <- get_inat_obs(taxon_name = "Apidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
andrenidae <- get_inat_obs(taxon_name = "Andrenidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
halictidae <- get_inat_obs(taxon_name = "Halictidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
colletidae <- get_inat_obs(taxon_name = "Colletidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
megachilidae <- get_inat_obs(taxon_name = "Megachilidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
melittidae <- get_inat_obs(taxon_name = "Melittidae", geo = TRUE, maxresults = 7000 , bounds = bounds)
length(apidae$scientific_name) #1247
length(andrenidae$scientific_name) #164 (most genus only)
length(halictidae$scientific_name) #132
length(colletidae$scientific_name) #22
length(megachilidae$scientific_name) #173
length(melittidae$scientific_name) #11

inat <- rbind(apidae, andrenidae, halictidae, colletidae, megachilidae)
head(inat)

#Traitbase----
#NOT implemented
library(traitbaser)
cnx <- connect("http://www.traitbase.info", "", "")
off <- resource(cnx, "species")

query(off)
query(off, conditions=buildCondition("species", "==", "Bombus")  )
query(off, limit=2, skip=0)
query(off, limit=2, skip=2)

#merge Gen sp plant lat long, date, credit----
colnames(dat)
head(dat)
head(inat)
colnames(inat) <- c("species"                  ,"datetime"                        
                    ,"description"                     ,"place_guess"                     
                    ,"decimalLatitude"                        , "decimalLongitude"                       
                    ,"tag_list"                        ,  "common_name"                     
                    ,"url"                              ,"image_url"                       
                    ,"user_login"                       ,"id"                              
                    ,"species_guess"                    ,"iconic_taxon_name"               
                    ,"taxon_id"                         ,"id_please"                       
                    , "num_identification_agreements"   , "num_identification_disagreements"
                    , "observed_on_string"              , "observed_on"                     
                    , "time_observed_at"                , "time_zone"                       
                    , "positional_accuracy"             , "private_place_guess"             
                    , "geoprivacy"                      , "coordinates_obscured"            
                    , "positioning_method"              , "positioning_device"              
                    , "out_of_range"                    , "user_id"                         
                    , "created_at"                      , "updated_at"                      
                    , "quality_grade"                   , "license"                         
                    , "sound_url"                       , "oauth_application_id"            
                    , "captive_cultivated")
inat$family <- NA
inat$sex <- NA
inat$recordedBy  <- inat$user_login
inat$identifiedBy <- inat$user_login
date <- as.POSIXlt(strptime(inat$observed_on, "%Y-%m-%d")) #convert to date class
inat$day <- date$mday #extract the day only
inat$month <- date$mon+1 #extract the day only
inat$year <- date$year + 1900 #extract the day only

d <- rbind(dat[,c("species", "decimalLatitude",  "decimalLongitude", "family",
            "year", "month",  "day", "recordedBy", "identifiedBy", "sex")], 
      inat[, c("species", "decimalLatitude",  "decimalLongitude", "family",
               "year", "month",  "day", "recordedBy", "identifiedBy", "sex")])

#Clean and merge Gbig and inat----
#Duplicates (ignore for now)

#species in canary islands
d2 <- subset(d, decimalLatitude > 35.8 & decimalLatitude < 43.88 & 
               decimalLongitude > - 10.11 & decimalLongitude < 4.56)

#species in the see (ignore for now?)

#species with only genus (Gbif, done using species column)
d3 <- d2[which(is.na(d2$species) == FALSE),]
unique(d3$species)
d4 <- d3[grep(" ", d3$species, fixed = TRUE, value = FALSE),]
unique(d4$species) #463 sp ... not bad...

#Load and merge Asensio----

asensio <- read.csv(file = "asensio/data/asensio_clean.csv")
head(asensio)
asensio$decimalLatitude <- asensio$Lat2
asensio$decimalLongitude <- asensio$Long2
asensio$species <- paste(asensio$genus, asensio$species)
asensio$family <- NA
asensio$recordedBy <- asensio$collector
asensio$identifiedBy <- asensio$taxonomist

asensio <- asensio[-grep(" sp ", asensio$species, fixed = TRUE, value = FALSE),]
asensio <- asensio[-grep("Sphecodes sp", asensio$species, fixed = TRUE, value = FALSE),]
asensio <- asensio[-grep("Amegilla sp", asensio$species, fixed = TRUE, value = FALSE),]
asensio <- asensio[-grep("[0-9]", asensio$species, value = FALSE),]

d5 <- rbind(d4, 
           asensio[, c("species", "decimalLatitude",  "decimalLongitude", "family",
                    "year", "month",  "day", "recordedBy", "identifiedBy", "sex")])

d5$species <- trimws(d5$species)
unique(d5$species) #835!!
dim(d5) #8818 occurrences...

#Load and merge Beefun----

library(BeeFunData)
data(all_interactions)
head(all_interactions)
data(sites)
head(sites)
data(traits_pollinators_estimated)
head(traits_pollinators_estimated)
beefun <- merge(all_interactions, sites)
beefun <- merge(beefun, traits_pollinators_estimated)

unique(beefun$family)
beefun <- subset(beefun, family %in% c("Andrenidae", "Apidae", "Megachilidae",
                                       "Colletidae", "Melittidae"))

head(beefun)
beefun$species <- beefun$Pollinator_gen_sp
unique(beefun$species)
beefun <- subset(beefun, !species %in% c("Osmia sp", "Panurgus sp",
                                         "Nomada sp", "Megachile sp",
                                         "Hoplitis sp", "Eucera sp",
                                         "Dasypoda sp", "Colletes sp",
                                         "Coelioxys sp", "Ceratina sp",
                                         "Ceratina sp", "Apidae NA",
                                         "Anthophora sp", "Andrena sp"))
beefun$decimalLatitude <- beefun$latitude
beefun$decimalLongitude <- beefun$longitude
beefun$family <- beefun$family
beefun$year <- 2015
beefun$month <- NA
beefun$day <- NA
beefun$recordedBy <- "Curro Molina"
beefun$identifiedBy <- "Oscar Aguado"
beefun$sex <- beefun$Pollinator_sex

head(beefun)
d6 <- rbind(d5, 
            beefun[, c("species", "decimalLatitude",  "decimalLongitude", "family",
                        "year", "month",  "day", "recordedBy", "identifiedBy", "sex")])

unique(d6$species) #846!!
dim(d6) #9622 occurrences...

#export

write.csv(d6, file = "_data/data.csv")


#Add Gredos----

dat <- read.csv("_data/data.csv")
head(dat)




#notes----

#https://github.com/ropensci/scrubr

#Maybe add spocc? https://cran.r-project.org/web/packages/spocc/vignettes/spocc_vignette.html


#Quick map points----
dat <- read.csv("_data/data.csv")
head(dat)
library(mapr)
dat$name <- dat$species
col <- unique(dat[, c("species", "family")]) 
col$id <- as.numeric(as.factor(col$family))
col$id <- ifelse(is.na(col$id), 7, col$id)
col <- merge(col, data.frame(id = 1:7, col = c(rainbow(6), "grey"))) 
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
JI <- ji(cbind(dat$decimalLongitude, dat$decimalLatitude))
dat$X <- JI[, 1]
dat$Y <- JI[, 2]
dat$Cell <- paste(dat$X, dat$Y)
counts <- by(dat, dat$Cell, 
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

#fetch some info----

dat <- read.csv("_data/data.csv")
head(dat)
tail(sort(table(dat$species)), 100)
hist(dat$month, las = 1, xlab = "mes del año", main = "abejas observadas por mes")
par(mar = c(4,8,3,2))
barplot(height = tail(sort(table(dat$species), decreasing = FALSE), 20), las = 2, horiz = TRUE,
        cex.names = 0.5)

#Notes----
#species requested
#Megachile willughbiella 
#Nomioides fortunatus
#Colletes nigricans
#Hoplitis benoisti
#Megachile pyrenaica
#Sphecodes gibbus
  #Owen
#Heriades rubicola
#Dioxis cincta

#Megachile apicalis
#Anthidium septemdentatum





