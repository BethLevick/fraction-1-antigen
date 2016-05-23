########################################################################################################
                          ## Bethany Levick, University of Liverpool 2016 ##
                                  ## Analysis of F1 strain data ##
	## Code to generate images to be compiled as animated .gif files of outbreak (S5 and S6) ##
########################################################################################################
## To generate the .gifs: the images generated below are saved as separate files 
## then these are passed to Image Magick to create the .gif images
## Image Magik have some decent tutorials on their website at http://www.imagemagick.org/Usage/anim_basics/

## Set WD
## would recommend this not be Dropbox/G Drive/Similar to avoid it filling with a load of plots!
setwd( DIRECTORY )
########################################################################################################
## vector years will have been generated previously in the set up at the top of f1analysis-plots.r
## through this vector, generate a map image of each year and save to the working drive
for(i in 1:length(years)){
	## generate a file name
	name <- paste( "tsmap", years[i], ".png", sep="" )
	## subset map list to relevant year
	pla <- map.list[[i]][map.list[[i]]$plague>0,]
	f1 <- map.list[[i]][map.list[[i]]$f1>0,]

	## open to generate a png
	png(name)
	## set plot parameters
	par( cex.lab=2, cex.axis=2, mai=c(1, 1.05, 0.25, 0.20) )
	## plot out all burrow locations using burrow locations data frame
	plot( burrow$long, burrow$lat, xlab="Longitude (degrees east)", ylab="Latitude (degrees north)", axes=F,
		xlim=c(7426000,7746000), ylim=c(4343000,4643000), type="n", main=years[i] )
	## add grid	
	grid(nx = 32, ny = 30, col = "grey58", lty = "dotted",
		 lwd = par("lwd"), equilogs = TRUE)
	## plot burrow locations in white	 
	points( burrow$long, burrow$lat, pch=21, cex=3, bg="white", col="black", lwd=2 )
	## add lat long axis labels							  
	axis( side=1, at=c(7400000, 7500000, 7600000, 7700000), labels=c("74°", "75°", "76°", "77°"), tick=FALSE )
	axis( side=2, at=c(4300000, 4400000, 4500000, 4600000), labels=c("43°", "44°", "45°", "46°"), tick=FALSE )

	## add burrows with plague
	points( pla$long, pla$lat, pch=21, cex=3, bg="burlywood2", col="black", lwd=2)
	## add burrows with f1 plague
	points( f1$long, f1$lat, pch=21, cex=3, bg="cornflowerblue", col="black", lwd=2 ) 
	## add a box
	box( col="antiquewhite3" )
	## add the legend
	legend( "bottomleft", legend=c("Plague never isolated", "Plague isolated, always F1+", "F1- plague isolated"), 
	pch=c(21,21,21), col=c("black", "black", "black"),
	pt.bg=c("white", "burlywood2", "cornflowerblue"), bty="n", pt.cex=3 ,pt.lwd=2.5, cex=2 )
	## close the device to start the next plot
	dev.off()
}

##################################################################
## repeat with just plague records, no F1

for(i  in 1:length(years)){
#print(years[i])
	## generate a file name
	name <- paste( "tsmap", years[i], ".png", sep="" )
	## subset map list to relevant year
	pla <- map.list[[i]][map.list[[i]]$plague>0,]
	f1 <- map.list[[i]][map.list[[i]]$f1>0,]

	## open to generate a png
	png(name)
	## set plot parameters
	par( cex.lab=2, cex.axis=2, mai=c(1, 1.05, 0.25, 0.20) )
	## plot out all burrow locations using burrow locations data frame
	plot( burrow$long, burrow$lat, xlab="Longitude (degrees east)", ylab="Latitude (degrees north)", axes=F,
		xlim=c(7426000,7746000), ylim=c(4343000,4643000), type="n", main=years[i] )
	## add grid	
	grid(nx = 32, ny = 30, col = "grey58", lty = "dotted",
		 lwd = par("lwd"), equilogs = TRUE)
	## plot burrow locations in white	 
	points( burrow$long, burrow$lat, pch=21, cex=3, bg="white", col="black", lwd=2 )
	## add lat long axis labels							  
	axis( side=1, at=c(7400000, 7500000, 7600000, 7700000), labels=c("74°", "75°", "76°", "77°"), tick=FALSE )
	axis( side=2, at=c(4300000, 4400000, 4500000, 4600000), labels=c("43°", "44°", "45°", "46°"), tick=FALSE )

	## add burrows with plague
	points( pla$long, pla$lat, pch=21, cex=3, bg="burlywood2", col="black", lwd=2)
	## add a box
	box( col="antiquewhite3" )
	## add the legend
	legend( "bottomleft", legend=c("Plague never isolated", "Plague isolated"), 
	pch=c(21,21,21), col=c("black", "black"),
	pt.bg=c("white", "burlywood2"), bty="n", pt.cex=3 ,pt.lwd=2.5, cex=2 )
	## close the device to start the next plot
	dev.off()
}
########################################################################################################
###########################################END##########################################################