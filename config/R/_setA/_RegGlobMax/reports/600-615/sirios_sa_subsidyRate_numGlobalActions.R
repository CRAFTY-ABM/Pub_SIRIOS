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
		"NumGlobActionsForester")


setBmetrics <- c("NumActions")
		
simp$sim$scenario <- "B1"

runs = c(600:615)


setsimp <- simp
setsimp$sim$id <- "set600-615"

simp$fig$height <- 700
simp$fig$width <- 1000
simp$fig$linewidth 	<- 1

rseeds = c(0:19)

############### END of Parameter Section ######################

library(plyr)

data <- shbasic::sh_tools_loadorsave(SIP = setsimp, OBJECTNAME = "data_metrics_globNum", 
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
			
			data <- rbind(numGlobActions, numactions, numactionsNC, numactionsCerealC, numactionsLivestockC, 
					numactionsCerealNC, numactionsLivestockNC, numactionsForester)
			
			#runparams <- craftyr::input_csv_param_runs(simp, paramid = TRUE)
			data$RunId <- run
			data$Rseed <- rseed
		}
	}
	return(data)
})

data_agg <- plyr::ddply(data, c("RunId","Rseed"), function(data_metrics) data.frame(
		
		NumActions			= mean(data_metrics[data_metrics$Metric == "NumActions", "Value"]),
		NumGobActions			= mean(data_metrics[data_metrics$Metric == "NumGlobActions", "Value"]),
		NumGlobActionsNC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsNC", "Value"]),
	
		NumGlobActionsForester		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsForester", "Value"]),
			
		NumGlobActionsCerealC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsCerealC", "Value"]),
		NumGlobActionsLivestockC	= mean(data_metrics[data_metrics$Metric == "NumGlobActionsLivestockC", "Value"]),

		NumGlobActionsCerealNC		= mean(data_metrics[data_metrics$Metric == "NumGlobActionsCerealNC", "Value"]),
		NumGlobActionsLivestockNC	= mean(data_metrics[data_metrics$Metric == "NumGlobActionsLivestockNC", "Value"])
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
		facet_column = "Facet", facet_ncol = 1, filename = paste("NumGlobalActions_DoE", setsimp$sim$id, 
				setsimp$sim$id, sep="_"),
		alpha = simp$fig$alpha, ggplotaddons = list(
				ggplot2::guides(fill=FALSE),
				ggplot2::scale_fill_manual(values=colours),
				ggplot2::scale_color_manual(values=colours, labels=metriclabels, guide=ggplot2::guide_legend(ncol=2)),
				#viridis::scale_color_viridis(discrete=TRUE, labels=metriclabels, guide=ggplot2::guide_legend(ncol=2)),
				ggplot2::facet_wrap(as.formula(paste("~",  "Facet")), ncol = 2, scales="free_y"),
				ggplot2::theme(legend.position = "bottom")), showsd = TRUE,
		returnplot = FALSE)
