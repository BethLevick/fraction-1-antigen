########################################################################################################
                          ## Bethany Levick, University of Liverpool 2016 ##
                                  ## Analysis of F1 strain data ##
				## Generating rearranged data sets and reformatting data for analysis ##
########################################################################################################

########################################################################################################
##add a column of burrow specific isolate description
##i.e. if F1N and F1P found together (e.g. in different animals), ignore the F1P
dat$f.over <- dat$f.fix
for(i in 1:nrow(dat)){
if( dat$f.fix[i]=="F1P" ){
	tmp <- dat[dat$id==dat$id[i] & dat$Year==dat$Year[i],]
	
	if(nrow(tmp)>1){
	#print( unique(tmp$Year) )
		evs <- unique(tmp$f.fix)

		#if( "F1N" %in% evs ){
		if(  match( "F1N", evs, nomatch=0 )>0 ){
			print(evs)
			print( dat$id[i] )
			print( dat$f.over[i])
			print( tmp$Year )
			dat$f.over[i] <- "F1N"
			print( dat$f.over[i])
		}
		}
	}
}

## Generate binary column of plague/not plague, f1-/not f1-
dat$plague <- c(rep(NA,nrow(dat)))
dat$f1 <- c(rep(NA,nrow(dat)))

dat$plague[dat$F.type %in% c("F1+", "F1±", "F1-")] <- 1
dat$plague[is.na(dat$plague)] <- 0
## use fixed type for presence of F1- in burrow to override other plague
dat$f1[dat$f.over=="F1N"] <- 1
dat$f1[dat$f.over %in% c("F1P", "F0")] <- 0
########################################################################################################
## tidy up species isolated from
dat$s.fix <- c(rep(NA,nrow(dat)))

dat$s.fix[dat$Host.full.name=="Rhombomys opimus"] <- "RO"
dat$s.fix[dat$Host.full.name =="Fleas on R.opimus"] <- "FRO"
dat$s.fix[dat$Host.full.name =="Migrating fleas"] <- "FMI"
dat$s.fix[dat$Host.full.name %in% c("Ticks on R.opimus", "Migrating ticks")] <- "T"

########################################################################################################
########################################################################################################
## to make a map plot showing shift in distribution over time

latlong <- paste( dat$lat, dat$long, sep="" )	#paste lat and long to make unique position ids
latlong <- unique(latlong)	#keep unique pairs
latlong <- latlong[latlong!="NANA"]	#only keep non NA values

years <- as.numeric(unique(dat$Year))	#get unique years

map.list <- list()	## set up empty list

## for each unique year
for(k in 1:length(years)){
## set up data frame, lat col is first half of strings in vector, long is second half, then the year repeated for each pair
map.list[[k]] <- data.frame( lat=substring( latlong, 1, 7 ), long=substring( latlong, 8, 14 ), year=c(rep(years[k], length(latlong))) )

}
## take first data frame as setup
map <- map.list[[1]]
## paste all of the data frames from the list onto the placeholder
for(k in 1:length(map.list)){
map <- rbind(map, map.list[[k]])
}

## to add plague data
for(k in 1:nrow(map)){
##subset to where lat found
tmp <- dat[dat$lat==map$lat[k],]
## and matching long found
tmp1 <- tmp[tmp$long==map$long[k],]
## and year found
tmp2 <- tmp1[tmp1$Year==map$year[k],]
## sector record is from
if( length(unique(tmp2$Sector.WE>0)) ){
map$sector[k] <- unique(tmp2$Sector.WE)
}
## number of plague cases found
map$plague[k] <- length( which( tmp2$F.type %in% c("F1+", "F1±", "F1-") ) )
## number of records for this lat/long/year 
map$cases[k] <- nrow(tmp2)
## number of f1n cases found
map$f1n[k] <- (length(which(tmp2$F.type=="F1-")))

}

## convert all columns to numeric
for(k in 1:ncol(map)){
map[,k] <- as.numeric(as.character(map[,k]))
}

## proportion of plague cases that are f1n
map$propf1n <- map$f1n/map$plague
## replace NaN with NA, for clarity
map$propf1n[map$propf1n=="NaN"] <- NA
## plague as incidence of records
map$pla.inc <- map$plague/map$cases

## to plot map
## set up vector of colours, one poss val for each rounded proportion to 2dp
cls <- seq( 0.01, 1, length.out=100 )
## for each lat/long/year
for(k in 1:nrow(map)){
	## if the proportion that is f1- is not NA
	if( is.na( map$propf1n[k] )==FALSE ){
		val <- (floor(map$propf1n[k]*10))+1		## multiply by ten and floor, then add 1 (so 0=1) to find in vector
		#print(val)
		map$cl[k] <- cls[val]					## colour is represented by valth position in vector
	}else{
		map$cl[k] <- 0							##else return 0
	}
}
## convert to numeric
map$cl <- as.numeric(map$cl)
########################################################################################################
## to reference this to, need a matrix of actual burrow locations
## where each unique burrow is compared to each other
latlong <- paste( dat$lat, dat$long, sep="" )	#paste lat and long to make unique position ids
latlong <- unique(latlong)	#keep unique pairs
latlong <- latlong[latlong!="NANA"]	#only keep non NA values
burrow <- data.frame( latlong=latlong, lat=substring( latlong, 1, 7 ), long=substring( latlong, 8, 14 ) )
burrow$id <- paste( "burr", c(1:(nrow(burrow))), sep="" )

## match across by latlong pair to add the id to the main data frame
for(k in 1:nrow(dat)){
## for each latlong pair in the dat frame, subset burrow to where this is found
tmp <- burrow[burrow$lat==dat$lat[k],]
tmp1 <- tmp[tmp$long==dat$long[k],]
## copy across the id from the burrow frame
dat$id[k] <- tmp1$id
}
########################################################################################################
## to generate graphic
burrow$lat <- as.numeric(as.character(burrow$lat))
burrow$long <- as.numeric(as.character(burrow$long))

## the three points to the far east may be errors - remove from this
burrow <- burrow[burrow$long<7800000,]

pla <- map[map$plague>0,]
f1 <- map[map$f1n>0,]
########################################################################################################
## data frame of time line of cases
timeseries <- data.frame( year=c( min(dat$Year, na.rm=T):max(dat$Year, na.rm=T) ) )
timeseries$plague <- c(rep(NA,nrow(timeseries)))
timeseries$f1 <- c(rep(NA,nrow(timeseries)))

for(i in 1:nrow(timeseries)){
tmp <- dat[dat$Year==timeseries$year[i],]
timeseries$plague[i] <- length(which(tmp$F.type %in% c("F1+","F1±","F1-")))
timeseries$f1[i] <- length(which(tmp$F.type=="F1-"))
}
########################################################################################################
## data frame of average yearly lat and long points of plague and F1- plague
av.dir <- data.frame( year=c( min(dat$Year, na.rm=T):max(dat$Year, na.rm=T) ) )
av.dir$pla.ave <- c(rep(NA, nrow(av.dir)))
av.dir$pla.avn <- av.dir$pla.ave
av.dir$f1.ave <- av.dir$pla.ave
av.dir$f1.avn <- av.dir$pla.avn 

for(i in 1:nrow(av.dir)){
## subset to each year in question
tmp <- dat[dat$Year==av.dir$year[i],]
## find the mean longitude when plague present in this year
av.dir$pla.ave[i] <- mean( tmp$long[tmp$F.type %in% c("F1+","F1±","F1-")], na.rm=T )
## and latitude
av.dir$pla.avn[i] <- mean( tmp$lat[tmp$F.type %in% c("F1+","F1±","F1-")], na.rm=T )

## and the same for f1- plague strains
av.dir$f1.ave[i] <- mean( tmp$long[tmp$F.type=="F1-"], na.rm=T )
## and latitude
av.dir$f1.avn[i] <- mean( tmp$lat[tmp$F.type=="F1-"], na.rm=T )
}
########################################################################################################
## Occupancy data set
#occ.ts <- read.csv("occupancy-data.csv", header=T, stringsAsFactors=F)
## TODO read from .csv here 
## can compare to a yearly average
## or see if spring or autumn average is a better predictor?
## trim both dfs to 2000-2013
occf1.ts <- data.frame( year=c(2000:2013) )
occf1.ts$pla <- c(rep(NA,nrow(occf1.ts)))
occf1.ts$f1 <- c(rep(NA,nrow(occf1.ts)))
occf1.ts$avocc <- c(rep(NA,nrow(occf1.ts)))
occf1.ts$avocc.sp <- c(rep(NA,nrow(occf1.ts)))
occf1.ts$avocc.au <- c(rep(NA,nrow(occf1.ts)))

for(i in 1:nrow(occf1.ts)){
occf1.ts$pla[i] <- timeseries$plague[timeseries$year==occf1.ts$year[i]]
occf1.ts$f1[i] <- timeseries$f1[timeseries$year==occf1.ts$year[i]]

occf1.ts$avocc[i]  <- mean( occ.ts$mean.rodent[occ.ts$year==occf1.ts$year[i]], na.rm=T )
occf1.ts$avocc.sp[i] <- occ.ts$mean.rodent[occ.ts$year==occf1.ts$year[i] & occ.ts$season=="spring"]
occf1.ts$avocc.au[i] <- occ.ts$mean.rodent[occ.ts$year==occf1.ts$year[i] & occ.ts$season=="autumn"]
}

occf1.ts$propf1 <- occf1.ts$f1/occf1.ts$pla

########################################################################################################
## set up some subsets of data frames for demographic analysis
dat$sex <- c(rep(NA,nrow(dat)))

dat$sex[dat$Host.detail %in% c("AF", "AFL", "AFP", "AFP2", "YFV", "YF")] <- "F"

dat$sex[dat$Host.detail %in% c("AM", "YM")] <- "M"

## subset to where RO
dat.r <- dat[!is.na(dat$sex),]

## age
dat.r$age <- c(rep(NA,nrow(dat.r)))

dat.r$age[dat.r$Host.detail %in% c("AF", "AFL", "AFP", "AFP2", "AM")] <- "A"

dat.r$age[dat.r$Host.detail %in% c("YF", "YFV", "YM")] <- "Y"

## reduce to only females
dat.f <- dat.r[dat.r$sex=="F",]

dat.f$repstat <- dat.f$Host.detail

dat.f$repstat[dat.f$Host.detail %in% c("AF")] <- "AF"

dat.f$repstat[dat.f$Host.detail %in% c("AFL", "AFP", "AFP2")] <- "AFPL"

dat.f$repstat[dat.f$Host.detail %in% c("YF", "YFV", "YM")] <- NA
########################################################################################################
###########################################END##########################################################