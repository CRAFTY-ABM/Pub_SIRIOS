################################################################################
# Version specific SIMulation Properties:
#
# Project:		NetSens
# Last update: 	29/10/2015
# Author: 		Sascha Holzhauer
################################################################################

# General SIMulation Properties ################################################

if (!exists("simp")) simp <- craftyr::param_getDefaultSimp()

simp$sim$worldname 				<- "WorldX"
simp$sim$version				<- "_setA/_RegGlobMax"
simp$sim$parentf				<- ""
simp$sim$folder					<- simp$sim$version 
simp$sim$scenario				<- "B1"
simp$sim$regionalisation		<- "3"
simp$sim$regions				<- c("DE13", "DE14", "DE27")
#simp$sim$runids					<- c("0-0")
#simp$sim$id 					<- "0-0"
simp$sim$endtick				<- 2040
simp$sim$aftsceario				<- "B"

### Directories ################################################################
simp = shbasic::shbasic_adjust_outputfolders(simp, pattern = "%VFOLDER%", value = simp$sim$folder)


### Figure Settings ############################################################
simp$fig$resfactor		<- 3
simp$fig$outputformat 	<- "png"
simp$fig$init			<- craftyr::output_visualise_initFigure
simp$fig$numfigs		<- 1
simp$fig$numcols		<- 1
simp$fig$height			<- 1000
simp$fig$width			<- 1500
simp$fig$splitfigs		<- FALSE
simp$fig$facetlabelsize <- 14
simp$fig$maptitle		<- "WorldX-AFTs"

### Batch Run Creation Settings #################################################
simp$batchcreation$pathToData				<- "../.."
simp$batchcreation$scenarios				<- c("B1")
simp$batchcreation$aftscenarios				<- c("A")
simp$batchcreation$startrun 				<- 280
simp$batchcreation$regionalisations			<- c("3")
simp$batchcreation$modes					<- c("plain", "complex")
simp$batchcreation$percentage_takeovers 	<- c(3) 
simp$batchcreation$competitions 			<- c("Competition_linear_08.xml")
simp$batchcreation$institutions				<- c("./institutions/Institutions_RegionalProvisionB.xml")
simp$batchcreation$multifunctionality 		<- c("mono")
simp$batchcreation$multiprodfilename		<- NULL
simp$batchcreation$allocation				<- c("GiveUpGiveInAllocationRandom.xml")
simp$batchcreation$frPas					<- "_setA/_RegGlobMax/institutions/pas/RegionalSupplyInstPasE.xml"

simp$batchcreation$variationstages 			<- c("homo")
simp$batchcreation$socialnetwork 			<- "None"
simp$batchcreation$searchabilities			<- c(30)
simp$batchcreation$inputdatadir 			<- sprintf("%s/data/%s", simp$dirs$project, simp$sim$folder)
simp$batchcreation$agentparam_tmpldir		<- paste(simp$batchcreation$inputdatadir, "/agents/templates/", sep="")
simp$batchcreation$gu_stages				<- c("medium")
simp$batchcreation$gi_stages				<- c("medium")
simp$batchcreation$placeholders				<- c(0)

simp$batchcreation$heterogeneitySD			<- c(0.3)
simp$batchcreation$guprobs					<- c(0.2)
simp$batchcreation$allocprobs				<- c(0.2)

simp$batchcreation$aftParamIds				<- 0:0

												


Run_Threshold 		<-  list(ThresholdTimberRegional = c(0.005, 0.06, 0.2), ThresholdCerealRegional = c(0.005, 0.06, 0.2), 
						ThresholdMeatRegional = c(0.005, 0.06, 0.2), ThresholdRecreationRegional  = c(0.005, 0.06, 0.2),
						ThresholdTimberGlobal = c(0.005, 0.06, 0.2), ThresholdCerealGlobal = c(0.005,0.06, 0.2), 
						ThresholdMeatGlobal = c(0.005, 0.06, 0.2), ThresholdRecreationGlobal  = c(0.005, 0.06, 0.2))

Run_FactorsGlobal 	<- list(FactorCerealGlobal = c(0.2, 0.5, 1.0, 0.4, 0.8), FactorMeatGlobal = c(0.2, 0.5, 1.0, 0.4, 0.8), 
							FactorTimberGlobal = c(0.2, 0.5, 1.0, 0.4, 0.8),
							FactorCerealRegional = c(0.2, 0.5, 1.0, 0.4, 0.8), FactorMeatRegional = c(0.2, 0.5, 1.0, 0.4, 0.8), 
							FactorTimberRegional = c(0.2, 0.5, 1.0, 0.4, 0.8))

Run_Noise			<- list(GlobalInstNoise = c(0.0, 0.2), InstitutionNoise = c(0.0, 0.2))

Run_Runtime 		<- list(GlobalInstActionRT = c(1,3,4), RegionalInstActionRT = c(1,3,4))

Run_MonitorDelay	<- list(MonitorDelay = c(0,2,1), MonitorDelayRegion = c(0,2,1))

Run_GlobInst		<- list(GlobalInstitutions_xml = c("institutions/global/GlobalInstitutionsB.xml",
				"institutions/global/GlobalInstitutionsC.xml"),
							Insititutions_xml = c("./institutions/Institutions_RegionalProvisionB.xml",
									"./institutions/Institutions_RegionalProvisionC.xml")
		)

Run_InstBTs = list(
		BT_xml = 
				c("/.//agents/BehaviouralTypes.xml, ./institutions/Institutions_BehaviouralTypesG.xml",
						"/.//agents/BehaviouralTypes.xml, ./institutions/Institutions_BehaviouralTypesF.xml",
						"/.//agents/BehaviouralTypes.xml, ./institutions/Institutions_BehaviouralTypesH.xml"),
		GlobalInstBT = 
				c("./institutions/global/GlobalInstitutionsBehaviouralTypesD.xml",
						"./institutions/global/GlobalInstitutionsBehaviouralTypesC.xml",
						"./institutions/global/GlobalInstitutionsBehaviouralTypesF.xml")																
)

Run_frPas			<- list(frPas = c("_setA/_RegGlobMax/institutions/pas/RegionalSupplyInstPasD.xml", 
				"_setA/_RegGlobMax/institutions/pas/RegionalSupplyInstPasF.xml"))

Run_PACconsiderRegional <- list(PACconsiderRegional = c(0.0, 1.0, 0.4))

Run_PACconsiderGlobal <- list(PACconsiderGlobal = c(0.0, 1.0, 0.4))

simp$batchcreation$placeholders				<- list(
		Run_Threshold 			= c(2,3),
		Run_FactorsGlobal 		= c(1,2),
		Run_Noise				= c(1),
		Run_Runtime 			= c(1,3),
		Run_MonitorDelay		= c(1),
		Run_PACconsiderGlobal 	= c(3),
 		Run_PACconsiderRegional = c(3),
		Run_GlobInst			= c(1,2),
		Run_InstBTs				= c(3),
		Run_frPas				= c(2))

simp$batchcreation$laramodel				<- "../lara/RegionalLaraModel.xml"
	
simp$batchcreation$versiondirs$production	<- "../_BLwX/"
simp$batchcreation$versiondirs$competition	<- "../_BLwX/"
simp$batchcreation$versiondirs$allocation	<- "../"
simp$batchcreation$versiondirs$worldfile	<- "../"
simp$batchcreation$versiondirs$agentdef 	<- "../"
simp$batchcreation$versiondirs$btdef		<- "./"
simp$batchcreation$versiondirs$demandfolder	<- "./demand/demand05"

simp$batchcreation$multiprodfilename 	<- NULL

# change for capital data
simp$dirs$param$getparamdir <- function(simp, datatype = NULL) {
	
	return <- paste(simp$dirs$data, if (is.null(datatype)) { 
				simp$sim$folder
			} else if (datatype %in% c("capitals")) {
						paste("_setA", "worlds", simp$sim$worldname,
								if(!is.null(simp$sim$regionalisation)) paste("regionalisations", 
											simp$sim$regionalisation, sep="/"), "capitals", sep="/")
					} else if (datatype %in% c("demand")) {
						paste("_setA/_RegGlobMAx/demand/demand05", sep="/")
					} else if (datatype %in% c("agentparams")) {
						paste(simp$sim$folder, "worlds", simp$sim$worldname, sep="/")
					} else if (datatype %in% c("productivities")) {
						paste(simp$sim$folder, "../_BLwX", "production", sep="/")
					} else if (datatype %in% c("competition")) {
						paste(simp$sim$folder	, "competition", sep="/")
					} else if (datatype %in% c("runs")) {
						simp$sim$folder
					},
			sep="/")
}