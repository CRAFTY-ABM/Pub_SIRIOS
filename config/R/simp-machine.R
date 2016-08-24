###############################################################################
# Machine=specific SIMP definition
#
# NOTE: Changes in super-level parameters that are used to derive further
# parameters need to trigger a re-evaluation of the derived parameters!
#
# Project:		SIRIOS
# Last update: 	23/08/2016
# Author: 		Sascha Holzhauer
################################################################################

### Clean/Remove existing definitions ##########################################
rm(list=ls(name=globalenv(), pattern="[^{preserve}]"), envir=globalenv())

### Project Root ###############################################################
project			<- "Path To project code folder"

if (!exists("preserve")) {
	preserve <- list()
	preserve$run = 0
}
#### Load default SIMP #########################################################
source(paste(project, "/config/R/simpBasic.R", sep=""))
simp$dirs$project <- project

#### Set path to itself ########################################################
simp$simpDefinition <- paste(simp$dirs$project, "config/R/simp-machine.R", sep="")

### Directories ###############################################################
		
simp$dirs$data 				<- paste(simp$dirs$project, "data/", sep="")
simp$dirs$simp				<- paste(simp$dirs$project, "./config/R/", sep="")

simp$dirs$outputdir			<- "Absolute Path to Output of Figures etc."
simp$dirs$outputdirWS		<- paste(simp$dirs$project, "/Output/", sep="")
		
simp$dirs$output$simulation	<- paste(simp$dirs$outputdirWS, "Data/", sep="")
simp$dirs$output$data		<- paste(simp$dirs$project, "Data/", sep="")
simp$dirs$output$rdata		<- paste(simp$dirs$outputdir, "RData/", sep="") 
simp$dirs$output$raster		<- paste(simp$dirs$outputdir, "Raster/", sep="") 
simp$dirs$output$figures	<- paste(simp$dirs$outputdir, "Figures/", sep="")
simp$dirs$output$reports	<- paste(simp$dirs$outputdir, "Reports/", sep="")
simp$dirs$output$tables		<- paste(simp$dirs$outputdir, "/Tables/", sep="")
simp$dirs$output$csv		<- paste(simp$dirs$outputdir, "/CSV/", sep="")
simp$dirs$output$runinfo	<- paste(simp$dirs$project, "config/SIRIOS_Runs_SH.ods", sep="/")

futile.logger::flog.info("Current working directory: %s",
			getwd(),
			name = "sirios.simp")
