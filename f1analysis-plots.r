########################################################################################################
                          ## Bethany Levick, University of Liverpool 2016 ##
                                  ## Analysis of F1 strain data ##
########################################################################################################

########################################################################################################
################################################SET UP##################################################
########################################################################################################
## setwd and open files
setwd( DIRECTORY )
## open strain data
dat <- read.table( "strains_14-06-04.csv", sep=",", stringsAsFactors=FALSE, header=T )
## Open occupancy data set
occ.ts <- read.csv("occupancy-data.csv", header=T, stringsAsFactors=F)
## Open trace element data
trace.all <- read.csv("traces-full.csv", header=T, stringsAsFactors=F )
tracesf <- read.csv("trace-yearly-deviance.csv", header=T)
########################################################################################################
## set up - data cleaning and rearranging
dat <- dat[,1:47]

### dat isn't in date order
## order by year column at least to make some more sense of it
dat <- dat[ order(dat[,1]), ]

## converting lat and long measurements to numeric
# tidy up column names
colnames(dat)[6] <- "lat"
colnames(dat)[7] <- "long"
# get rid of spaces
dat$lat <- gsub(" ", "", dat$lat)
dat$long <- gsub( " ", "", dat$long)
# coerce to numeric
dat$lat <- as.numeric(dat$lat)
dat$long <- as.numeric(dat$long)

## quick plot to check values
#plot( dat$long, dat$lat )		#unhash to test

## some values falling well outside range - give the nature of the dat these are likely mistakes
#which(dat$lat<2000000)
# rows 1028 and 1029, with values 454054
# suspect missing 0's on the end
dat$lat[1028] <- 4540540
dat$lat[1029] <- 4540540
## replace "na" strings with NA
dat[dat=="na"] <- NA
## Generate tidy F1 strain column
dat$f.fix <- dat$F.type
dat$f.fix[dat$f.fix=="F1+"] <- "F1P"
dat$f.fix[dat$f.fix=="F1±"] <- "F1P"
dat$f.fix[dat$f.fix=="F1-"] <- "F1N"
dat$f.fix[is.na(dat$f.fix)] <- "F0"
########################################################################################################
## set up data frames for use as detailed below
source("file-setup.r")
########################################################################################################
## Data frames set up in environment:
## dat, the main data frame, the original .csv arranged by date of record collection
## with subsets dat.r, where all host individuals are R.opimus
## and dat.f, where all host individuals are R.opimus females
## burrow, each unique burrows position
## map, recording the number of f1+/- strains isolated at a burrow in a particular year
## timeseries, the number of f1+/- strains isolated in each year
## av.dir, the average location of f1+/- strains in each year
## occ.ts, a season by season record of gerbil occupancy (gerbils per km2)
## occf1.ts, a year by year record of occupancy, plague and F1- strains over the whole pre-Balkhash
## pla, plague and f1 strains for each lat long position for each observation
########################################################################################################
#######################################PLOT GENERATION##################################################
########################################################################################################
## Epidemic curve plot (fig1)
tdata <- t( as.matrix( timeseries[6:nrow(timeseries),2:3] ) )


## plot as a grouped bar chart
par( cex.lab=2, cex.axis=2, mai=c(1, 1.05, 0.25, 0.20) )
barplot( tdata, beside=T, axes=F, bty="n", ylab="Isolates", xlab="Year", col=c("burlywood2", "cornflowerblue"), names.arg=c(2000:2013), yaxt="n",
	ylim=c(0,210), cex.names=1, border=NA )
	
grid( nx=0, ny=NULL, lty="dotted", col="black" )

barplot( tdata, beside=T, axes=F, bty="n", ylab="Isolates", xlab="Year", col=c("burlywood2", "cornflowerblue"), names.arg=c(2000:2013), yaxt="n",
	ylim=c(0,210), add=TRUE, cex.names=1, border=NA )
	
axis( side=2, at=seq(0,250,50), labels=seq(0,250,50), tick=F )

legend( "topleft", legend=c("Y. pestis: all strains", "F1- strains"), pch=c(22,22),
	pt.bg=c("burlywood2", "cornflowerblue"), col="black", pt.lwd=2, pt.cex=3, cex=2, bty="n" )

box( )
########################################################################################################
## Map of locations of all plague and F1- isolates (fig3)
> range( burrow$long, na.rm=T )
[1] 7426101 7742546
> range( burrow$lat, na.rm=T )
[1] 4343037 4643037

par( cex.lab=2, cex.axis=2, mai=c(1, 1.05, 0.25, 0.20) )
plot( burrow$long, burrow$lat, xlab="Longitude (degrees east)", ylab="Latitude (degrees north)", axes=F,
	type="n", xlim=c(7426000,7746000), ylim=c(4343000,4643000) )
## grid not working well with PLoS file specs	
#grid(nx = 32, ny = 30, col = "darkgray", lty = "dotted",
 #    lwd = par("lwd"), equilogs = TRUE)
							  
axis( side=1, at=c(7400000, 7500000, 7600000, 7700000), labels=c("74°", "75°", "76°", "77°"), tick=FALSE )
axis( side=2, at=c(4300000, 4400000, 4500000, 4600000), labels=c("43°", "44°", "45°", "46°"), tick=FALSE )


points( burrow$long, burrow$lat, pch=21, cex=3, bg="white", col="black", lwd=2)

##
points( pla$long, pla$lat, pch=21, cex=3, bg="burlywood2", col="black", lwd=2)

points( f1$long, f1$lat, pch=21, cex=3, bg="cornflowerblue", col="black", lwd=2 ) 


box(  )

legend( "bottomleft", legend=c("Plague never isolated", "Plague isolated, always F1+", "F1- plague isolated"), 
	pch=c(21,21,21), col=c("black", "black", "black"),
	pt.bg=c("white", "burlywood2", "cornflowerblue"), bty="n", pt.cex=3 ,pt.lwd=2.5, cex=2 )
########################################################################################################
## See separate file "gif-generation.r" for script to generate .gif images
########################################################################################################
###############################################ANALYSIS#################################################
########################################################################################################
########################################################################################################
## Quantify spatial variation of plague in general and of f1- strains

## range of plague cases
range( dat$long[dat$plague==1], na.rm=T )
# 7426101 8042546
range( dat$lat[dat$plague==1], na.rm=T )
# 4421627 4843037

## and of F1- plague
range( dat$long[dat$f1==1], na.rm=T )
# 7441233 7726421
range( dat$lat[dat$f1==1], na.rm=T )
# 4440989 4602132

## so a slightly narrowed range overall compared to plague
## plague and f1- strains, associations with year, latitude and longitude
dat.t <- dat[dat$Year>=2005,]
## convert lat and long to degrees
dat.t$lat <- dat.t$lat/100000
dat.t$long <- dat.t$long/100000
latpla <- glm( plague ~ lat + long + Year + (lat*Year) + (long*Year), data=dat.t, family="binomial" )
##drop1 - can we simplify the model
drop1(latpla)
## dropping lat*Year (ns in above model) has ns reduction in AIC
## drop for improving simplicity whilst not losing predictive ability
latpla1 <- glm( plague ~ lat + long + Year + (long*Year), data=dat.t, family="binomial" )
##drop1 - can we improve further
drop1(latpla1)
## all drops would be significant increase in AIC
## keep as minimal model

latf1 <- glm( f1 ~ lat + long + Year + (lat*Year) + (long*Year), data=dat.t, family="binomial" )
## drop1
drop1(latf1)
## given all terms NS and only NS improvement from dropping, unlikely to generate 
## significant terms (in a meaningful model)
## stop at this point
########################################################################################################
## Gerbil numbers and F1- strains (Fig 3)
## occupancy and f1 cases time series plot
## set up plot area, time series on top of each other
par( mfrow=c(2,1), cex.lab=2, cex.axis=2, mai=c(1,1,0.5,0.5) )
## set up plot for year and f1- 
plot( occf1.ts$year, occf1.ts$f1, type="n", 
	ylab="F1- strains",
	xlab="Year", xaxt="n", ylim=c(min(occf1.ts$f1, na.rm=T), max(occf1.ts$f1, na.rm=T)))
## add axis for years	
axis( side=1, at=c(2000:2014), cex.axis=1 )	
## add gridlines	
abline( v=c(2000:2013), h=seq(0,80, 40), col="lightgrey", lty=2 )
## plot on line of f1 numbers
lines( occf1.ts$year, occf1.ts$f1, col="cornflowerblue", lwd=2 )

## set up plot for year and occupancy
plot( occf1.ts$year, occf1.ts$avocc, type="n", 
	ylab="Gerbils per km2",
	xlab="Year", xaxt="n", ylim=c(min(occf1.ts$avocc, na.rm=T), max(occf1.ts$avocc, na.rm=T)))
## axis for years	
axis( side=1, at=c(2000:2014), cex.axis=1  )	
## gridlines	
abline( v=c(2000:2013), h=seq(0,0.6, 0.2), col="lightgrey", lty=2 )
## plot on line of 
lines( occf1.ts$year, occf1.ts$avocc, col="black", lwd=2 )

## CCF between the occupancy and proportion of plague strains identified that are F1-
## from first appearance of F1- strains (2005) (S3)
ccf( occf1.ts$avocc[6:nrow(occf1.ts)], occf1.ts$propf1[6:nrow(occf1.ts)],
	na.action=na.pass, main="Average occupancy and proportion of plague isolates with F1- 2005-2013" )
########################################################################################################
## CCF between the occupancy and F1 time series, from first appearance of F1- strains (2005) (S4)
ccf( occf1.ts$pla[6:nrow(occf1.ts)], occf1.ts$propf1[6:nrow(occf1.ts)], 
	na.action=na.pass, main="Plague and proportion that was F1-" )
########################################################################################################
## demographics X2 tests
## Chi square test of association: plague strain present and host sex
f.sex <- table( dat.r$sex, dat.r$f.fix )

## run test
chisq.test( f.sex )

## Chi square test of association: plague strain present and host demographic group
## including all age/sex combinations
f.age <- table( dat.r$age, dat.r$f.fix )

## run test
chisq.test( f.age )

## Chi square test of association: plague strain present and reproductive stage in females
f.rep <- table( dat.f$repstat, dat.f$f.fix )

## run test
chisq.test( f.rep )
########################################################################################################
## Host species X2 tests
## Chi square test of association: plague strain present and host species
f.spec <- table( dat$s.fix, dat$f.fix )

## run test
chisq.test( f.spec )

## Chi square test of association: location fleas found (on host or off host)
freq.fl <- table( dat$s.fix[dat$s.fix %in% c("FMI", "FRO")], dat$f.fix[dat$s.fix %in% c("FMI", "FRO")] )

## run test
chisq.test(freq.fl)
########################################################################################################
## F1- events following other plague events
## find how often no plague is followed by f1-/+
## get records with no plague
## can really only use burrows with multiple records of infection
## can use some similar code from last time


## label how many unique years we see a burrow in
dat$freq <- c(rep(NA,nrow(dat)))
for(i in 1:nrow(dat)){
dat$freq[i] <- length(unique(dat$Year[dat$id==dat$id[i]]))
}

## subset to burrows with at least 2 records
dat.2 <- dat[dat$freq>1,]
## 677 obs
## over 45 burrows
## of 209 unique burrows in the data set


bl <- unique(dat.2$id)
evt <- 0
for(i in 1:length(bl)){
evt <- evt + (length(unique(dat.2$Year[dat.2$id==bl[i]])) - 1)
}

## 91 transition events

## generate new data frame to store events
events <- data.frame( burrow=NA, e1=NA, e2=NA, y1=NA, y2=NA )
track <- 0
## for each burrow with >1 record
for(i in 1:length(bl)){
#print(i)
## subset to each unique burrow in turn
tmp <- dat.2[dat.2$id==bl[i],]
#print(nrow(tmp))
#print(unique(tmp$Year))
#print(tmp$f.fix)
	## for all years the burrow is seen except the last, as no following record
	yrs <- unique(tmp$Year)
	#print(yrs)
	for(j in 1:(length(yrs)-1)){
		tmp.1 <- tmp[tmp$Year==yrs[j],]
		tmp.2 <- tmp[tmp$Year==(yrs[j+1]),]
		#print(colnames(tmp.2))
		#print(nrow(tmp.2))
		#print( "Vector" )
		#print(tmp.1$f.fix)
		#print( "Vector" )
		#print(tmp.2$f.fix)
		## e1 is the event at j
		e1 <- NA
		## get f.fix records for this year
		e1set <- unique(tmp.1$f.fix)
			if( "F1N" %in% e1set ){
			e1 <- "F1N"
			}else if( "F1P" %in% e1set){
			e1 <- "F1P"
			}else if( "F0" %in% e1set ){
			e1 <- "NOP"
			}else{
			e1 <- NA
			}
		#print(unique(tmp$Year[j]))
		#print("e1")
		#print( "Unique" )
		print( e1set )
		#print( e1 )
		#####
		## e2 is the event at j+1
		e2 <- NA
		## get f.fix records for this year
		e2set <- unique(tmp.2$f.fix)
			if( "F1N" %in% e2set ){
			e2 <- "F1N"
			}else if( "F1P" %in% e2set){
			e2 <- "F1P"
			}else if( "F0" %in% e2set ){
			e2 <- "NOP"
			}else{
			e2 <- NA
			}
		#print(unique(tmp$Year[j+1]))
		#print("e2")
		#print( "Unique" )
		print( e2set )
		#print( e2 )
		# if records have been stored, send them back to the main data frame		
		if( !is.na(e1) & !is.na(e2) ){
		events <- rbind( events, c(bl[i], e1, e2, unique(tmp$Year)[j], unique(tmp$Year)[j+1] ) )
		track <- track + 1
		print(track)
		}
		}
	
}
## remove placeholder row
events <- events[-1,]
## paste events together to tabulate
events$comb <- paste( events$e1, events$e2, sep="" )

table( events$comb )
#F1NF1N F1NF1P F1NNOP F1PF1N F1PF1P F1PNOP NOPF1N NOPF1P NOPNOP 
#    14      7      2     20     27      7      2     11      1 

##chisq test of distribution for events with F1N as second event
chisq.test(c(14,20,2))
########################################################################################################
## see NOP F1N at burrow 1 and burrow 10
burrow.dat <- data.frame( id=unique(dat$id), lat=length(unique(dat$id)), long=length(unique(dat$id)) )
for( i in 1:nrow(burrow.dat)){
burrow.dat$lat[i] <- unique(dat$lat[dat$id==burrow.dat$id[i]])
burrow.dat$long[i] <- unique(dat$long[dat$id==burrow.dat$id[i]])
}
## convert latlong to find dist in km
burrow.dat$lat <- burrow.dat$lat/100000
burrow.dat$long <- burrow.dat$long/100000
## generate distance matrix
nams <- unique(dat$id)
namsf <- c("burr10", "burr1")

dmat <- matrix( ncol=length(nams), nrow=length(namsf), NA )

rownames(dmat) <- namsf
colnames(dmat) <- nams
## find distances between these 2 burrows and all other burrows
gcd.hf <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}
## from R bloggers Mario Pineda-Krch
## http://www.r-bloggers.com/great-circle-distance-calculations-in-r/

for(k in 1:length(namsf)){
x <- burrow.dat[burrow.dat$id==namsf[k],]
	for(i in 1:length(nams)){
	y <- burrow.dat[burrow.dat$id==nams[i],]
	dmat[k,i] <- gcd.hf( (x$long/100000), (x$lat/100000), (y$long/100000), (y$lat/100000) )
	}
}


## burrow 10
table( dat$Year[dat$id=="burr10"], dat$f.fix[dat$id=="burr10"] )
## so high f1n records after generally low place and 4 years after a no plague record
#       F0 F1N F1P
#  2001  2   0   0
#  2002  0   0   4
#  2003  1   0   0
#  2007  0  12   0
## last plague positive in 2002 - look at surrounding plague infections since 2003-2006
## burrows within a 1km radius
burr101km <- dmat[1,]
burr101km <- burr101km[burr101km<=1]
## remove self self
burr101km <- burr101km[names(burr101km)!="burr10"]
## 208 burrows within 1km of burrow 10
burr10set <- dat[dat$id %in% names(burr101km),]
## 1118 observations
## reduce to years we are interested in
burr10set <- burr10set[burr10set$Year %in% c(2003:2006),]
## 356 obs in the relevant years
## reduce to f1p cases
burr10set <- burr10set[burr10set$f.fix=="F1P",]
## 263 relevant 
## find distance to nearest case over time period
## over this time period only cases 2003-2005
yrs <- unique(burr10set$Year)

## in each year find nearest f1 positive plague case to burrow in question
clos <- data.frame( year=yrs, nearb=c(rep(NA,length(yrs))), dst=c(rep(NA,(length(yrs))) ) )

for(i in 1:nrow(clos)){
tmp <- burr10set[burr10set$Year==clos$year[i],]
burs <- unique(tmp$id)
print(burs)
burdist <- burr101km[names(burr101km) %in% burs]
#print( length(burdist) )
clos$nearb[i] <- names(burdist)[which(burdist==min(burdist, na.rm=T))]
clos$dst[i] <- min(burdist, na.rm=T)
}

#  year  nearb        dst
#1 2003 burr11 0.13016676
#2 2004  burr9 0.10275408
#3 2005 burr30 0.09448956

## burrow 31
table( dat$Year[dat$id=="burr31"], dat$f.fix[dat$id=="burr31"] )

#       F0 F1N F1P
#  2002  1   0   3
#  2003  1   0   0
#  2007  0   5   5
#  2008  0   1   0

## so only time we see F1N following no plague is alongside F1P
## still see where nearest F1P was previously
## look 2003-2006
burr311km <- dmat[2,]
burr311km <- burr311km[burr311km<=1]
## remove self self
burr311km <- burr311km[names(burr311km)!="burr31"]
## 82 burrows within 1km of burrow 10
burr31set <- dat[dat$id %in% names(burr311km),]
## 423 observations
## reduce to years we are interested in
burr31set <- burr31set[burr31set$Year %in% c(2003:2006),]
## 116 obs in the relevant years
## reduce to f1p cases
burr31set <- burr31set[burr31set$f.fix=="F1P",]
## 57 relevant 
## find distance to nearest case over time period
## over this time period only cases 2003-2005
yrs <- unique(burr31set$Year)

## in each year find nearest f1 positive plague case to burrow in question
clos <- data.frame( year=yrs, nearb=c(rep(NA,length(yrs))), dst=c(rep(NA,(length(yrs))) ) )

for(i in 1:nrow(clos)){
tmp <- burr31set[burr31set$Year==clos$year[i],]
burs <- unique(tmp$id)
#print(burs)
burdist <- burr311km[names(burr311km) %in% burs]
#print( length(burdist) )
clos$nearb[i] <- names(burdist)[which(burdist==min(burdist, na.rm=T))]
clos$dst[i] <- min(burdist, na.rm=T)
}

#  year  nearb         dst
#1 2003 burr53 0.007856104
#2 2004  burr9 0.080341828
#3 2005 burr12 0.081336594
########################################################################################################
## Trace elements
## plot trace element data from the 5 sectors 
## as average deviation from the mean across all sectors that year (for each micronutrient)
## format trace element names for labels
formt <- c("Ca", "Co", "Cu", "Fe", "Mn", "Zn")

secs <- unique(tracesf$sector)
traces <- c("ca", "cu", "co", "fe", "mn", "zn")

## trace elements in each sector (fig4)
par( mfrow=c(2,3), mai=c(0.6,0.6,0.25,0.25), cex.lab=1.5, cex.axis=1 )

for(i in 1:length(traces)){
## subset to current trace element
tmp <- tracesf[tracesf$traceel==traces[i],]
## order by the sector number
tmp <- tmp[order(tmp$sector),]

#print(tmp)

## set up plot area
plot( 0,0,type="n",xlim=c(1,length(secs)), ylim=c(min(c(tmp$y11.lo, tmp$y12.lo), na.rm=T), max(c(tmp$y11.hi, tmp$y12.hi), na.rm=T)), xaxt="n",
main=formt[i], xlab="Sector", ylab=paste( formt[i], "Concentration", sep=" ")  )

## add line from upper to lower limit
segments( c(1:length(secs)), tmp$y11.lo, 
	c(1:length(secs)), tmp$y11.hi, col="darkgrey", lty=1, lwd=0.75 )
## add averages for y11
points( c(1:length(secs)), tmp$y11, cex=2, pch=16, col="cornflowerblue" )
## add lo ci
points( c(1:length(secs)), tmp$y11.lo, cex=1, pch=16, col="cornflowerblue" )
## add hi ci
points( c(1:length(secs)), tmp$y11.hi, cex=1, pch=16, col="cornflowerblue" )

## add averages for y12
## add line from upper to lower limit
segments( c(1:length(secs)), tmp$y12.lo, 
	c(1:length(secs)), tmp$y12.hi, col="darkgrey", lty=1, lwd=0.75 )
## add averages for y12
points( c(1:length(secs)), tmp$y12, cex=2, pch=16, col="burlywood2" )
## add lo ci
points( c(1:length(secs)), tmp$y12.lo, cex=1, pch=16, col="burlywood2" )
## add hi ci
points( c(1:length(secs)), tmp$y12.hi, cex=1, pch=16, col="burlywood2" )

## add x axis labels
#print(tmp$sector)
axis( side=1, at=c(1:length(secs)), labels=as.character(tmp$sector) )

legend( "topleft", pch=c(16,16), col=c("cornflowerblue", "burlywood2"), legend=c("2011", "2012"), bty="n")
}


######################################################################################
## use map data frame to get plage and f1 data
pla.secs <- data.frame( sector=c(7934,9123,10512,10531,10544,11742), av.pla.i=c(rep(NA,6)), av.f1.i=c(rep(NA,6)) )

for(i in 1:nrow(pla.secs)){
tmp <- map[map$sector==pla.secs$sector[i],]
#print(tmp)
## v plague incidence in all records
pla.secs$av.pla.i[i] <- sum(tmp$plague, na.rm=T)/sum(tmp$cases, na.rm=T)
## av f1 incidence across plague cases
pla.secs$av.f1.i[i] <- sum(tmp$f1n, na.rm=T)/sum(tmp$plague, na.rm=T)
## total number of cases
pla.secs$total.case[i] <- sum(tmp$cases, na.rm=T)
## total number of plague positives
pla.secs$total.plague[i] <- sum(tmp$plague, na.rm=T)
}

##########################################################################################
## consider this as ratios between certain elements
## iron:copper and iron:cobalt
## using the subset sector data frame generated above
pla.secs$fecu11 <- c(rep(NA,nrow(pla.secs)))
pla.secs$fecu12 <- c(rep(NA,nrow(pla.secs)))
pla.secs$feco11 <- c(rep(NA,nrow(pla.secs)))
pla.secs$feco12 <- c(rep(NA,nrow(pla.secs)))
for(i in 1:nrow(pla.secs)){
tmp <- tracesf[tracesf$sector==pla.secs$sector[i],]
pla.secs$fecu11[i] <- tmp$y11[tmp$traceel=="fe"]/tmp$y11[tmp$traceel=="cu"]
pla.secs$fecu12[i] <- tmp$y12[tmp$traceel=="fe"]/tmp$y11[tmp$traceel=="cu"]
pla.secs$feco11[i] <- tmp$y11[tmp$traceel=="fe"]/tmp$y11[tmp$traceel=="co"]
pla.secs$feco12[i] <- tmp$y12[tmp$traceel=="fe"]/tmp$y11[tmp$traceel=="co"]
}
pla.secs$dummy <- c(1:nrow(pla.secs))

##  ratios of fe:cu and fe:co, and plague in each sector (fig5)
par(mfrow=c(1,3))
plot( pla.secs$dummy, pla.secs$fecu11, ylim=c(0,1500), type="n", xlab="Sector", ylab="Fe : Cu Ratio", xaxt="n" )
axis( side=1, at=pla.secs$dummy, labels=as.character(pla.secs$sector) )
points( pla.secs$dummy, pla.secs$fecu11, col="cornflowerblue", pch=16, cex=2 )
points( pla.secs$dummy, pla.secs$fecu12, col="burlywood2", pch=16, cex=2 )

plot( pla.secs$dummy, pla.secs$total.plague, pch=16, cex=2, xlab="Sector", ylab="Plague positive records", xaxt="n")
axis( side=1, at=pla.secs$dummy, labels=as.character(pla.secs$sector) )

plot( pla.secs$dummy, pla.secs$feco11, ylim=c(0,1500), type="n", xlab="Sector", ylab="Fe : Co Ratio", xaxt="n" )
axis( side=1, at=pla.secs$dummy, labels=as.character(pla.secs$sector) )
points( pla.secs$dummy, pla.secs$feco11, col="cornflowerblue", pch=16, cex=2 )
points( pla.secs$dummy, pla.secs$feco12, col="burlywood2", pch=16, cex=2 )

##########################################################################################
## Kendall's rank correlations

## fix NaN
trace.all$av.pla.i[trace.all$av.pla.i=="NaN"] <- 0
trace.all$av.f1.i[trace.all$av.f1.i=="NaN"] <- NA

## each of the average trace element concentrations and plague
cor.test( rank(trace.all$avca, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall" )
cor.test( rank(trace.all$avcu, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avco, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avfe, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avmn, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avzn, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
## each of the average trace element concentrations and F1- plague
cor.test( rank(trace.all$avca, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall" )
cor.test( rank(trace.all$avcu, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avco, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avfe, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avmn, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )
cor.test( rank(trace.all$avzn, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )
## each of the interesting ratios and plague
cor.test( rank(trace.all$fecuav, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall" )
cor.test( rank(trace.all$fecoav, na.last=TRUE), rank(trace.all$av.pla.i, na.last=TRUE), method="kendall"  )
## each of the interesting ratios and F1- plague
cor.test( rank(trace.all$fecuav, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall" )
cor.test( rank(trace.all$fecoav, na.last=TRUE), rank(trace.all$av.f1.i, na.last=TRUE), method="kendall"  )

## try to plot some of these differently to understand whats going on
par(mfrow=c(2,3), cex.lab=1.5, cex.axis=1.5 )
plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avca)[order(rank(trace.all$avca))], rank(trace.all$av.pla.i)[order(rank(trace.all$avca))], col="red", lwd=2)
legend( "topleft", bty="n", legend="Ca Tau 0.2, P 0.82")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avcu)[order(rank(trace.all$avcu))], rank(trace.all$av.pla.i)[order(rank(trace.all$avcu))], col="blue", lwd=2)
legend( "topleft", bty="n", legend="Cu Tau 0.2, P 0.82")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avco)[order(rank(trace.all$avco))], rank(trace.all$av.pla.i)[order(rank(trace.all$avco))], col="purple", lwd=2)
legend( "topleft", bty="n", legend="Co Tau 0.2, P 0.82")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avmn)[order(rank(trace.all$avmn))], rank(trace.all$av.pla.i)[order(rank(trace.all$avmn))], col="brown", lwd=2)
legend( "topleft", bty="n", legend="Mn Tau 0, P 1")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avzn)[order(rank(trace.all$avzn))], rank(trace.all$av.pla.i)[order(rank(trace.all$avzn))], col="green", lwd=2)
legend( "topleft", bty="n", legend="Zn Tau 0.4, P 0.83")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="Plague prevalence rank")
lines( rank(trace.all$avfe)[order(rank(trace.all$avfe))], rank(trace.all$av.pla.i)[order(rank(trace.all$avfe))], col="orange", lwd=2)
legend( "topleft", bty="n", legend="Fe Tau -0.2, P 0.82")

## try to plot some of these differently to understand whats going on
par(mfrow=c(2,3), cex.lab=1.5, cex.axis=1.5 )
plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avca)[order(rank(trace.all$avca))], rank(trace.all$av.f1.i)[order(rank(trace.all$avca))], col="red", lwd=2)
legend( "topleft", bty="n", legend="Ca Tau 0, P 1")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avcu)[order(rank(trace.all$avcu))], rank(trace.all$av.f1.i)[order(rank(trace.all$avcu))], col="blue", lwd=2)
legend( "topleft", bty="n", legend="Cu Tau 0.4, P 0.48")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avco)[order(rank(trace.all$avco))], rank(trace.all$av.f1.i)[order(rank(trace.all$avco))], col="purple", lwd=2)
legend( "topleft", bty="n", legend="Co Tau 0.4, P 0.48")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avmn)[order(rank(trace.all$avmn))], rank(trace.all$av.f1.i)[order(rank(trace.all$avmn))], col="brown", lwd=2)
legend( "topleft", bty="n", legend="Mn Tau 0.2, P 0.82")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avzn)[order(rank(trace.all$avzn))], rank(trace.all$av.f1.i)[order(rank(trace.all$avzn))], col="green", lwd=2)
legend( "topleft", bty="n", legend="Zn Tau 0.6, P 0.23")

plot( 0,0, xlim=c(1,6), ylim=c(1,6), xlab="Micronutrient Rank", ylab="F1- Plague prevalence rank")
lines( rank(trace.all$avfe)[order(rank(trace.all$avfe))], rank(trace.all$av.f1.i)[order(rank(trace.all$avfe))], col="orange", lwd=2)
legend( "topleft", bty="n", legend="Fe Tau 0, P 1")

########################################################################################################
###########################################END##########################################################