#######################################################################
# Prameter Creation Script (PSC) for Runs parameter CSV file.
# Enables switching on/off variations per parameter.
#
# Input:		Parameter meta definitions
# Output:		Runs.csv
#
# Project:		TEMPLATE
# Setting:		TEMPLATE
# Last update: 	02/09/2015
# Author: 		Sascha Holzhauer
#######################################################################


run = simp$batchcreation$startrun  - 1
aftParamId = 0
aftParamIdScenario = -1

d <- data.frame(stringsAsFactors=FALSE)                             # number of variations
for (institution in simp$batchcreation$institutions) {
for (percentageTakeOvers in simp$batchcreation$percentage_takeovers) {
	for (searchability in simp$batchcreation$searchabilities) {
		for (scenario in simp$batchcreation$scenarios) {  										#4
			for (regionalisation in simp$batchcreation$regionalisations) { 		#1-3
				for (variationstage in simp$batchcreation$variationstages) {
				for (allocprob in simp$batchcreation$allocprobs) {
				for (competition in simp$batchcreation$competitions) {	
					gridded <- expand.grid(simp$batchcreation$placeholders)
					for (i in 1:nrow(gridded)) {
						
						aftParamId = aftParamIdScenario	
						for (giStage in simp$batchcreation$gi_stages) {
							for (guStage in simp$batchcreation$gu_stages) {
								
			aftParamId = aftParamId + 1
			run = run + 1		
			data <- list()
								
			data["run"] 			<- run
			data["Scenario"] 		<- scenario
			data["Version"] 		<- simp$sim$version
			data["World"] 			<- simp$sim$worldname
			data["Regionalisation"] <- regionalisation
			
			data["aftParamId"] 		<- aftParamId
			
			for (j in 1:ncol(gridded)) {
				for (k in 1:length(get(colnames(gridded)[j]))) {
					data[names(get(colnames(gridded)[j]))[k]] <- get(colnames(gridded)[j])[[k]][gridded[i,j]]
				}
			}
			
			data["Insititutions_xml"] 				<- institution
			
			data["RegionCsvFile"] 	<- paste("/", simp$batchcreation$versiondirs$worldfile, "/worlds/",
					simp$sim$worldname, "/regionalisations/", regionalisation, ".csv", sep="")
			
			data["DemandFolder"]	<- simp$batchcreation$versiondirs$demandfolder
			
			data["Agent_xml"] 		<- paste("/", simp$batchcreation$versiondirs$agentdef, "/agents/FunctionalRoles_", 
					variationstage, ".xml", sep="")
			
			data["Competition_xml"] <- paste("/", simp$batchcreation$versiondirs$competition, "/competition/", 
					competition, sep="")
			
			data["Allocation_xml"]  <- paste("/", simp$batchcreation$versiondirs$allocation, "/allocation/",
					simp$batchcreation$allocation, sep="")
			
			data["Allocation_percentageCell"] 		<- searchability
			data["Allocation_percentageTakeovers"] 	<- percentageTakeOvers
			
			data["frPas"]							<- simp$batchcreation$frPas
			
			data["SocialNetwork_xml"] 				<- simp$batchcreation$socialnetwork
			
			data["LARAmodel_xml"]					<- simp$batchcreation$laramodel
		
			data["allocprob"]						<- allocprob
			
			data["pathToData"]						<- simp$batchcreation$pathToData
			d <- rbind(d, as.data.frame(data, stringsAsFactors=FALSE))
							}
						}
					}
				}
				}
				}
				}
			}
		}
	}
	aftParamIdScenario = -1
}

filename = paste(simp$batchcreation$inputdatadir , '/Runs_DoE.csv', sep='')
shbasic::sh.ensurePath(filename, stripFilename = TRUE)
futile.logger::flog.info("Write Run.csv file %s...",
		filename,
		name = "template.create.runsparam")
write.csv(d, filename, row.names = FALSE)
