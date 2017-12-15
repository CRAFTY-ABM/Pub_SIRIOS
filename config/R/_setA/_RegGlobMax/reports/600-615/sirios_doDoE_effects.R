##########################################################################################
# Create DoE Effect figure (Set focus and set uninteresting)
# Set switch 'nointeractioneffects' to TRUE to show only single effects
# (applies to focus set only).
#
# Project:		Pub_SIRIOS
# Last update: 	23/08/2016
# Author: 		Sascha Holzhauer
# Instructions:	Run maschine-specific SIMP file first (see craftyr documentation)
##########################################################################################

# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_RegGlobMax"	

simp$sim$task		<- "DoE-600" # Name of surounding folder, usually a description of task 

setsimp <- simp
setsimp$sim$id <- "set600-615"

preserve <- list()
preserve$task 		<- simp$sim$task

nointeractioneffects <-  TRUE

# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/common", sep="/"))
# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

library(plyr)

runs = 600:615
rseeds = 0:19

simp$fig$height			<- 1300
simp$fig$width			<- 1300
simp$fig$outputformat	<- "png"

simp$sim$scenario				<- "A1"

metriccolnamesAll = c("VarChangesLu", "ConsPatches_NC", "ConsConnectivity","MaxUnderSupply", "MaxOverSupply",
		"RegUnderSupply_Cereal", "RegUnderSupply_Meat", "RegUnderSupply_Timber",
		"VarChangesCells", "DivLuShannon", "ConsPatches_C", "ConsProp_C", "ConsProp_NC","NumActions",
		#"UnderSupply_Meat", "UnderSupply_Cereal", "UnderSupply_Timber",  "NumActionsNC",
		#"OverSupply_Total", "OverSupply_Meat", "OverSupply_Cereal", "OverSupply_Timber",
		"DivSupplyPerRegSimpson", "DivLuPerRegSimpson", "DivSupplyAcrossRegSimpson", "EffSupply")

metricColnamesFocus = c("VarChangesLu", "VarChangesCells",
		"MaxUnderSupply", "MaxOverSupply",  
		"RegUnderSupply_Timber",
		"ConsPatches_NC", "ConsProp_NC", "ConsConnectivity", "NumActions",  "DivSupplyPerRegSimpson"
		#"DivLuPerRegSimpson", "DivSupplyPerRegSimpson")
			# "UnderSupply_Cereal", "UnderSupply_Timber","UnderSupply_Meat", "NumActionsNC",
		)
paramcolnames = c("TriggerThreshold", "SubsidyRate", "ActionLifetime", "Precedence")

substitServices <- c("I" = 1, "J" = 2)
substitPresedence <- c("B" = 1, "C" = 2)

############### END of Parameter Section ######################

data <- shbasic::sh_tools_loadorsave(SIP = setsimp, OBJECTNAME = "data_metrics", 
		PRODUCTIONFUN = function() { 
	data <- data.frame()
	for (run in runs) {
		for (rseed in rseeds) {
			# run = runs[1]; rseed = rseeds[1]
		
			simp$sim$runids 	<- c(paste(run, rseed, sep="-"))			# run to deal with
			simp$sim$id			<- c(paste(run, rseed, sep="-"))
		
			input_tools_load(simp, "data_metrics")
		
			runparams <- craftyr::input_csv_param_runs(simp, paramid = TRUE)

			agg_metrics <- data.frame(
				VarChangesLu 	= sum(data_metrics[data_metrics$Metric == "VarChangesLu", "Value"]),
				ConsPatches_NC 	= mean(data_metrics[data_metrics$Metric == "ConsPatches_NC_Cereal-NC_Livestock", "Value"]),
				MaxUnderSupply  = max(0, abs(data_metrics[data_metrics$Metric == "MaxUnderSupply_Cereal-Meat-Timber", "Value"])),
				MaxOverSupply  	= max(0, data_metrics[data_metrics$Metric == "MaxOverSupply_Cereal-Meat-Timber", "Value"]),
			
				VarChangesCells	= sum(data_metrics[data_metrics$Metric == "VarChangesCells", "Value"]),
				DivLuShannon	= mean(data_metrics[data_metrics$Metric == "DivLuShannon", "Value"]),
				ConsPatches_C 	= mean(data_metrics[data_metrics$Metric == "ConsPatches_C_Cereal-C_Livestock", "Value"]),
				ConsProp_C  	= mean(data_metrics[data_metrics$Metric == "ConsProp_C", "Value"]), 
				ConsProp_NC		= mean(data_metrics[data_metrics$Metric == "ConsProp_NC", "Value"]), 
				ConsConnectivity= mean(data_metrics[data_metrics$Metric == "ConsConnectivity_NC_Cereal-NC_Livestock", "Value"]),
			
				# correct under/oversupply data:
				UnderSupply_Total = abs(mean(data_metrics[data_metrics$Metric == "UnderSupplyPercent_Total", "Value"])-100),  
				UnderSupply_Meat  = abs(mean(data_metrics[data_metrics$Metric == "UnderSupplyPercent_Meat", "Value"])-100),     
				UnderSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "UnderSupplyPercent_Cereal", "Value"])-100),
				UnderSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "UnderSupplyPercent_Timber", "Value"])-100),
			
				OverSupply_Total = abs(mean(data_metrics[data_metrics$Metric == "OverSupplyPercent_Total", "Value"])-100),  
				OverSupply_Meat  = abs(mean(data_metrics[data_metrics$Metric == "OverSupplyPercent_Meat", "Value"])-100),     
				OverSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "OverSupplyPercent_Cereal", "Value"])-100),
				OverSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "OverSupplyPercent_Timber", "Value"])-100),
			
				RegUnderSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Cereal", "Value"])-100),
				RegUnderSupply_Meat = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Meat", "Value"])-100),
				RegUnderSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Timber", "Value"])-100),
			
				DivSupplyPerRegSimpson = mean(data_metrics[data_metrics$Metric == "DivSupplyPerRegSimpson", "Value"]),
				DivLuPerRegSimpson  = mean(data_metrics[data_metrics$Metric == "DivLuPerRegSimpson", "Value"]),
				DivSupplyAcrossRegSimpson  = mean(data_metrics[data_metrics$Metric == "DivSupplyAcrossRegSimpson", "Value"]),
				EffSupply  		= mean(data_metrics[data_metrics$Metric == "EffSupply", "Value"]),
			
				NumActions		= metric_agg_actions_number(simp),
				NumActionsNC	= metric_agg_actions_number(simp, pattern="NC"),
			
				ID 				= simp$sim$id,
				TriggerThreshold= runparams[,"ThresholdCerealGlobal"],
				ActionLifetime	= runparams[,"GlobalInstActionRT"],
				SubsidyRate		= runparams[,"FactorTimberGlobal"],
				MonitorDelay	= runparams[,"MonitorDelay"],
				ConsiderPACglob = runparams[,"PACconsiderGlobal"],
				ConsiderPACreg  = runparams[,"PACconsiderRegional"],
				Noise			= runparams[,"GlobalInstNoise"],
			
				Services			= regmatches(runparams[,"BT_xml"], 
						regexpr("(?<=Institutions_BehaviouralTypes)(.*)(?=\\.xml)", runparams[,"BT_xml"], perl=TRUE)),
				Precedence		= regmatches(runparams[,"GlobalInstitutions_xml"], 
						regexpr("(?<=GlobalInstitutions)(.*)(?=\\.xml)", runparams[,"GlobalInstitutions_xml"], perl=TRUE))
			)		
			data <- rbind(data, agg_metrics)
		}
	}
	return(data)
})

# Substitute letters by numbers:
data$Services <- substitServices[data$Services]
data$Precedence <- substitPresedence[data$Precedence]


dexp <- shdoe::shdoe_param_getDefaultDexp()


metriclabels <-  read.csv(file="../../reports/KeyTranslations.csv", header=FALSE, stringsAsFactors = FALSE)
metriclabels <- setNames(metriclabels[,2], metriclabels[,1])


############### Set of focus metrics

# normalize data (divison by max/(mean)):
d <- apply(data[, metricColnamesFocus], MARGIN=2, FUN = function(x) max(abs(x)))
normd <- t(apply(data[, metricColnamesFocus], MARGIN=1, FUN= function(x) x/d))
colnames(normd) <- metricColnamesFocus


fxlist <- list()
for (i in metricColnamesFocus) {
	print(i)
	fx <- shdoe::shdoe_analyse_effectSizes(normd[,i], dexp, 
			data[,paramcolnames], id = "Testdata", confidence= 0.9)
	fx$response <- i
	fxlist <- append(fxlist, list(fx))
}
fx <- do.call(rbind, fxlist)

# extract single effects from first response var (whose row names are not numbered)
substitutions <- shdoe::shdoe_get_paramname_substitutions(simp, varnames = unique(rownames(fx))[!grepl(":",unique(rownames(fx))) & 
						!grepl("[0-9]", unique(rownames(fx)))], preventPlotmathParsing=TRUE)


if (nointeractioneffects) {
	indices <- !grepl(":", rownames(fx))
	effectdata <- setNames(fx$effects[indices], paste(rownames(fx)[indices], ":Effect", sep=""))
	substitutions <- c(substitutions, "Effect" = "Effect")
	simp$fig$height			<- 920
	simp$fig$width			<- 920
	filename 				<-  "crafty_netsens_analysis_doe_effects_3params_600-615_metricFocus_nointeraction_v1"
	numcol					<- 2
} else {
	indices <- rep(TRUE, length(rownames(fx)))
	effectdata <- setNames(fx$effects, rownames(fx))
	filename 				<-  "crafty_netsens_analysis_doe_effects_3params_600-615_metricFocus_v1"
	numcol					<-  3
}

shdoe::shdoe_visualise_interaction_effects(dexp = simp, 
		effects = effectdata, 
		errors = fx$errors[indices],
		pvalues = fx$pvalues[indices],
		response = fx$response[indices], 
		substitutions = substitutions, 
		filename = filename,
		ggplotaddons = list(ggplot2::theme(legend.position="bottom"),
				ggplot2::scale_colour_discrete(guide=ggplot2::guide_legend(ncol=numcol, title.position="top", 
								title.hjust=0),
						name = "Metrics", labels = metriclabels)
		)
)

############### Set of 'uninteresting' metrics

metricColnamesFocus = metriccolnamesAll[!metriccolnamesAll %in% metricColnamesFocus]

# normalize data (divison by mean):
d <- apply(data[, metricColnamesFocus], MARGIN=2, FUN = function(x) max(abs(x)))
normd <- t(apply(data[, metricColnamesFocus], MARGIN=1, FUN= function(x) x/d))
colnames(normd) <- metricColnamesFocus


fxlist <- list()
for (i in metricColnamesFocus) {
	print(i)
	fx <- shdoe::shdoe_analyse_effectSizes(normd[,i], dexp, 
			data[,paramcolnames], id = "Testdata", confidence= 0.9)
	fx$response <- i
	fxlist <- append(fxlist, list(fx))
}
fx <- do.call(rbind, fxlist)

# extract single effects from first response var (whose row names are not numbered)
substitutions <- shdoe::shdoe_get_paramname_substitutions(simp, varnames = unique(rownames(fx))[!grepl(":",unique(rownames(fx))) & 
						!grepl("[0-9]", unique(rownames(fx)))], preventPlotmathParsing=TRUE)

shdoe::shdoe_visualise_interaction_effects(dexp = simp, 
		effects = setNames(fx$effects, rownames(fx)), 
		errors = fx$errors,
		pvalues = fx$pvalues,
		response = fx$response, 
		substitutions = substitutions, 
		filename = "crafty_netsens_analysis_doe_effects_3params_600-615_metricUninteresting_v1",
		ggplotaddons = list(ggplot2::theme(legend.position="bottom"),
				ggplot2::scale_colour_discrete(guide=ggplot2::guide_legend(ncol=3),
						name = "Response\nVariables", labels = metriclabels)
		)
)	
