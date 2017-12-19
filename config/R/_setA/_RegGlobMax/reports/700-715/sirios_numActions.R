##########################################################################################
# Create Sensitivity Analysis plot for Subsidy rate
#
# Project:		Pub_SIRIOS
# Last update: 	24/08/2016
# Author: 		Sascha Holzhauer
# Instructions:	Run maschine-specific SIMP file first (see craftyr documentation)
##########################################################################################
# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_RegGlobMax"	

# Name of surounding folder, usually a description of task 
simp$sim$task		<- "SA" 
preserve <- list()
preserve$task 		<- simp$sim$task


# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/common", sep="/"))
# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

metriccolnames = c(
		"NumActions",
		"NumGlobActions",
		"NumGlobActionsNC",
		"NumGlobActionsCerealNC",
		"NumGlobActionsCerealC",
		"NumGlobActionsLivestockNC", 
		"NumGlobActionsLivestockC",
		"NumGlobActionsForester",
		"NumRegActions",
		"NumRegActionsNC",
		"NumRegActionsCerealNC",
		"NumRegActionsCerealC",
		"NumRegActionsLivestockNC", 
		"NumRegActionsLivestockC",
		"NumRegActionsForester")


setBmetrics <- c("NumRegActions",
		"NumRegActionsNC",
		"NumRegActionsCerealNC",
		"NumRegActionsCerealC",
		"NumRegActionsLivestockNC", 
		"NumRegActionsLivestockC",
		"NumRegActionsForester")
		
simp$sim$scenario <- "B1"

runs = c(700:715)


setsimp <- simp
setsimp$sim$id <- "set700-715"

simp$fig$height <- 700
simp$fig$width <- 1000
simp$fig$linewidth 	<- 1

rseeds = c(0:9)

############### END of Parameter Section ######################

library(plyr)

data <- shbasic::sh_tools_loadorsave(SIP = setsimp, OBJECTNAME = "data_metrics_num", 
		PRODUCTIONFUN = function() { 
	data <- data.frame()
	for (run in runs) {
		for (rseed in rseeds) {
			# run = runs[10]; rseed = rseeds[1]; simp=setsimp
			
			simp$sim$runids 	<- c(paste(run, rseed, sep="-"))			# run to deal with
			simp$sim$id			<- c(paste(run, rseed, sep="-"))
		
			numactions <- data.frame(
					Metric = "NumActions",
					Value  = metric_agg_actions_number(simp),
					Tick   = 2025) # arbitrary

			numGlobActions <- data.frame(
					Metric = "NumGobActions",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa"),
					Tick   = 2025) # arbitrary
			
			numactionsNC <- data.frame(
					Metric = "NumGlobActionsNC",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_NC"),
					Tick   = 2025) # arbitrary
			
			numactionsCerealC <- data.frame(
					Metric = "NumGlobActionsCerealC",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_CCereal"),
					Tick   = 2025) # arbitrary

			numactionsLivestockC <- data.frame(
					Metric = "NumGlobActionsLivestockC",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_CLivestock"),
					Tick   = 2025) # arbitrary
			
			numactionsCerealNC <- data.frame(
					Metric = "NumGlobActionsCerealNC",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_NCCereal"),
					Tick   = 2025) # arbitrary
			
			numactionsLivestockNC <- data.frame(
					Metric = "NumGlobActionsLivestockNC",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_NCLivestock"),
					Tick   = 2025) # arbitrary
			
			numactionsForester <- data.frame(
					Metric = "NumGlobActionsForester",
					Value  = metric_agg_actions_number(simp, pattern="GlobalCompFuncSubsidyPa_Forester"),
					Tick   = 2025) # arbitrary
			
			numRegActions <- data.frame(
					Metric = "NumRegActions",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa"),
					Tick   = 2025) # arbitrary

			numactionsRegNC <- data.frame(
					Metric = "NumRegActionsNC",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_NC"),
					Tick   = 2025) # arbitrary
			
			numactionsRegCerealC <- data.frame(
					Metric = "NumRegActionsCerealC",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_CCereal"),
					Tick   = 2025) # arbitrary

			numactionsRegLivestockC <- data.frame(
					Metric = "NumRegActionsLivestockC",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_CLivestock"),
					Tick   = 2025) # arbitrary
			
			numactionsRegCerealNC <- data.frame(
					Metric = "NumRegActionsCerealNC",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_NCCereal"),
					Tick   = 2025) # arbitrary
			
			numactionsRegLivestockNC <- data.frame(
					Metric = "NumRegActionsLivestockNC",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_NCLivestock"),
					Tick   = 2025) # arbitrary
			
			numactionsRegForester <- data.frame(
					Metric = "NumRegActionsForester",
					Value  = metric_agg_actions_number(simp, pattern="RegionalCompFuncSubsidyPa_Forester"),
					Tick   = 2025) # arbitrary

			data_metrics <- rbind(numGlobActions, numactions, numactionsNC, numactionsCerealC, numactionsLivestockC, 
					numactionsCerealNC, numactionsLivestockNC, numactionsForester,
				      numRegActions, numactionsRegNC, numactionsRegCerealC, numactionsRegLivestockC, 
					numactionsRegCerealNC, numactionsRegLivestockNC, numactionsRegForester)
			
			#runparams <- craftyr::input_csv_param_runs(simp, paramid = TRUE)
			data_metrics$RunId <- run
			data_metrics$Rseed <- rseed

			data <- rbind(data, data_metrics)
		}
	}
	return(data)
})

data_agg <- plyr::ddply(data, c("RunId","Rseed"), function(data_metrics) data.frame(
		
		NumActions			= mean(data_metrics[data_metrics$Metric == "NumActions", "Value"]),
		NumGlobActions			= mean(data_metrics[data_metrics$Metric == "NumGlobActions", "Value"]),
		NumGlobActionsNC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsNC", "Value"]),
	
		NumGlobActionsForester		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsForester", "Value"]),
			
		NumGlobActionsCerealC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsCerealC", "Value"]),
		NumGlobActionsLivestockC	= mean(data_metrics[data_metrics$Metric == "NumGlobActionsLivestockC", "Value"]),

		NumGlobActionsCerealNC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsCerealNC", "Value"]),
		NumGlobActionsLivestockNC	= mean(data_metrics[data_metrics$Metric == "NumGlobActionsLivestockNC", "Value"]),

		NumRegActions			= mean(data_metrics[data_metrics$Metric == "NumRegActions", "Value"]),
		NumRegActionsNC			= mean(data_metrics[data_metrics$Metric == "NumRegActionsNC", "Value"]),
	
		NumRegActionsForester		= mean(data_metrics[data_metrics$Metric == "NumRegActionsForester", "Value"]),
			
		NumRegActionsCerealC		= mean(data_metrics[data_metrics$Metric == "NumRegActionsCerealC", "Value"]),
		NumRegActionsLivestockC		= mean(data_metrics[data_metrics$Metric == "NumRegActionsLivestockC", "Value"]),

		NumRegActionsCerealNC		= mean(data_metrics[data_metrics$Metric == "NumRegActionsCerealNC", "Value"]),
		NumRegActionsLivestockNC	= mean(data_metrics[data_metrics$Metric == "NumRegActionsLivestockNC", "Value"])
))



data_melted <- reshape2::melt(data_agg, id.vars = c("RunId", "Rseed"),
		variable.name = "Metric",  value.name = "Value")


data_selected <- data_melted[data_melted$Metric %in% metriccolnames,]
data_selected$Facet <- "Normalised Set A"
data_selected[data_selected$Metric %in% setBmetrics, "Facet"] <- "Normalised Set B"

metriclabels <-  read.csv(file="../../reports/KeyTranslations_SA_TriggeringWoFacet.csv", header=FALSE, stringsAsFactors = FALSE)
metriclabels <- setNames(metriclabels[,2], metriclabels[,1])

colours <- RColorBrewer::brewer.pal(length(unique(data_selected[data_selected$Facet == "Normalised Set A","Metric"])), 
		"Set1")
colours <- c(colours,colours)

visualise_lines(simp, data_selected, x_column = "RunId", y_column="Value", title = NULL,
		colour_column = "Metric", colour_legendtitle = "Metric", colour_legenditemnames = metriclabels,
		facet_column = "Facet", facet_ncol = 1, filename = paste("NumActions_DoE", setsimp$sim$id, 
				setsimp$sim$id, sep="_"),
		alpha = simp$fig$alpha, ggplotaddons = list(
				ggplot2::guides(fill=FALSE),
				ggplot2::scale_fill_manual(values=colours),
				ggplot2::scale_color_manual(values=colours, labels=metriclabels, guide=ggplot2::guide_legend(ncol=2)),
				#viridis::scale_color_viridis(discrete=TRUE, labels=metriclabels, guide=ggplot2::guide_legend(ncol=2)),
				ggplot2::facet_wrap(as.formula(paste("~",  "Facet")), ncol = 2, scales="free_y"),
				ggplot2::theme(legend.position = "bottom")), showsd = TRUE,
		returnplot = FALSE)
