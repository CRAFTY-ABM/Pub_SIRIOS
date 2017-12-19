##########################################################################################
# Create Sensitivity Analysis plot for Subsidy rate
#
# Project:		Pub_SIRIOS
# Last update: 	18/12/2017
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

# sort(unique(as.character(colnames(data_agg))))

metrics = matrix(c(
		1, "ConsConnectivity",
		#1, "ConsPatches_C",
		#1, "ConsPatches_NC",
		1, "ConsProp_C",
		1, "ConsProp_NC",
		2, "DivLuPerRegSimpson",
		2, "DivLuShannon",
		2, "DivSupplyAcrossRegSimpson",
		2, "DivSupplyPerRegSimpson",
		2, "EffSupply",
		1, "MaxOverSupply",
		2, "MaxUnderSupply",  
		3, "NumActions",
		2, "NumActionsNC",
		3, "OverSupply_Cereal",
		3, "OverSupply_Meat",
		3, "OverSupply_Timber",
		3, "OverSupply_Total",
		3,"RegUnderSupply_Cereal",
		3,"RegUnderSupply_Meat",
		3,"RegUnderSupply_Timber",
		4,"UnderSupply_Cereal",
		4,"UnderSupply_Meat",
		4,"UnderSupply_Timber",
		4,"UnderSupply_Total",
		4, "VarChangesLu",
		4, "VarChangesCells"), ncol=2, byrow = T
)

set1metrics <- metrics[metrics[,1]=="1",2]
set2metrics <- metrics[metrics[,1]=="2",2]
set3metrics <- metrics[metrics[,1]=="3",2]
set4metrics <- metrics[metrics[,1]=="4",2]

metriccolnames <- metrics[,2]

simp$sim$scenario				<- "B1"

runs = 675:699
rseeds = 0:4

setsimp <- simp
setsimp$sim$id <- "set675-699"

simp$fig$height <- 1200
simp$fig$width <- 1500
simp$fig$linewidth 	<- 1

############### END of Parameter Section ######################

library(plyr)

data <- shbasic::sh_tools_loadorsave(SIP = setsimp, OBJECTNAME = "data_metrics", 
		PRODUCTIONFUN = function() { 
	data <- data.frame()
	for (run in runs) {
		for (rseed in rseeds) {
			# run = runs[7]; rseed = rseeds[1]
			
			simp$sim$runids 	<- c(paste(run, rseed, sep="-"))			# run to deal with
			simp$sim$id			<- c(paste(run, rseed, sep="-"))
			
			input_tools_load(simp, "data_metrics")
		
			numactions <- data.frame(
					Metric = "NumActions",
					Value  = metric_agg_actions_number(simp),
					Tick   = 2025) # arbitrary
			
			numactionsNC <- data.frame(
					Metric = "NumActionsNC",
					Value  = metric_agg_actions_number(simp, pattern="NC"),
					Tick   = 2025) # arbitrary
			
			data_metrics <- rbind(data_metrics, numactions, numactionsNC)
			
			runparams <- craftyr::input_csv_param_runs(simp, paramid = TRUE)
			data_metrics$TriggeringThreshold = runparams[,"ThresholdCerealGlobal"]
			data_metrics$Rseed <- rseed
		
			data <- rbind(data, data_metrics)
		}
	}
	return(data)
})

data_agg <- plyr::ddply(data, c("TriggeringThreshold","Rseed"), function(data_metrics) data.frame(
		
		# data_metrics = data[data$SubsidyRate == 0.3 & data$Rseed==0,]
		ConsPatches_NC 	= mean(data_metrics[data_metrics$Metric == "ConsPatches_NC_Cereal-NC_Livestock", "Value"]),
		
		VarChangesCells	= sum(data_metrics[data_metrics$Metric == "VarChangesCells", "Value"]),
		DivLuShannon	= mean(data_metrics[data_metrics$Metric == "DivLuShannon", "Value"]),
		ConsPatches_C 	= mean(data_metrics[data_metrics$Metric == "ConsPatches_C_Cereal-C_Livestock", "Value"]),
		ConsProp_C  	= mean(data_metrics[data_metrics$Metric == "ConsProp_C", "Value"]), 
		ConsProp_NC		= mean(data_metrics[data_metrics$Metric == "ConsProp_NC", "Value"]), 
		ConsConnectivity= mean(data_metrics[data_metrics$Metric == "ConsConnectivity_NC_Cereal-NC_Livestock", "Value"]),
		
		# correct under/oversupply data:
		UnderSupply_Total = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentUnder_Total", "Value"])-100),  
		UnderSupply_Meat  = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentUnder_Meat", "Value"])-100),     
		UnderSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentUnder_Cereal", "Value"])-100),
		
		OverSupply_Total = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentOver_Total", "Value"])-100),  
		OverSupply_Meat  = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentOver_Meat", "Value"])-100),     
		OverSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentOver_Timber", "Value"])-100),
		
		RegUnderSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Cereal", "Value"])-100),
		RegUnderSupply_Meat = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Meat", "Value"])-100),
		RegUnderSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "RegionalUnderSupplyPercent_Timber", "Value"])-100),
		
		DivSupplyPerRegSimpson = mean(data_metrics[data_metrics$Metric == "DivSupplyPerRegSimpson", "Value"]),
		DivLuPerRegSimpson  = mean(data_metrics[data_metrics$Metric == "DivLuPerRegSimpson", "Value"]),
		DivSupplyAcrossRegSimpson  = mean(data_metrics[data_metrics$Metric == "DivSupplyAcrossRegSimpson", "Value"]),
		EffSupply  		= mean(data_metrics[data_metrics$Metric == "EffSupply", "Value"]),
		
		
		VarChangesLu 	= sum(data_metrics[data_metrics$Metric == "VarChangesLu", "Value"]),
		MaxOverSupply  	= max(0, data_metrics[data_metrics$Metric == "MaxOverSupply_Cereal-Meat-Timber", "Value"]),
		MaxUnderSupply  = max(0, abs(data_metrics[data_metrics$Metric == "MaxUnderSupply_Cereal-Meat-Timber", "Value"])),
		
		OverSupply_Cereal = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentOver_Cereal", "Value"])-100),
		UnderSupply_Timber = abs(mean(data_metrics[data_metrics$Metric == "SupplyPercentUnder_Timber", "Value"])-100),
		
		NumActions		= mean(data_metrics[data_metrics$Metric == "NumActions", "Value"]),
		NumActionsNC	= mean(data_metrics[data_metrics$Metric == "NumActionsNC", "Value"])
))

replaceNAcolumns <-  c("UnderSupply_Total", "UnderSupply_Meat", "UnderSupply_Cereal",
		"OverSupply_Total", "OverSupply_Meat", "OverSupply_Cereal", "OverSupply_Timber",
		"RegUnderSupply_Timber", "RegUnderSupply_Meat", "RegUnderSupply_Cereal")
data_agg[,replaceNAcolumns] <- apply(data_agg[,replaceNAcolumns], 2, function(x){replace(x, is.na(x), 0)})


# devide by means across rseeds:
d <- apply(data_agg[, -match(c("TriggeringThreshold"), colnames(data_agg))], 
		MARGIN=2, FUN = function(x) abs(if (is.na(mean(x[1:length(rseeds)])) || mean(x[1:length(rseeds)]) != 0) 
								mean(x[1:length(rseeds)]) else max(colMeans(matrix(x, nrow=length(rseeds))))))
normd <- as.data.frame(t(apply(data_agg[, -match(c("TriggeringThreshold"), colnames(data_agg))], 
		MARGIN=1, FUN= function(x) x/d)))
normd$TriggeringThreshold <- data_agg$TriggeringThreshold
normd$Rseed <- data_agg$Rseed

data_melted <- reshape2::melt(normd, id.vars = c("TriggeringThreshold", "Rseed"),
		variable.name = "Metric",  value.name = "Value")


data_selected <- data_melted[data_melted$Metric %in% metriccolnames,]
# data_selected = data_selected[data_selected$Rseed == 0 & data_selected$SubsidyRate == 0,]


data_selected$Facet <- paste("Normalised Set", metrics[match(data_selected$Metric, metrics[,2]),1])

#metriclabels <-  read.csv(file="../../reports/KeyTranslations_SA_Triggering.csv", header=FALSE, stringsAsFactors = FALSE)
#metriclabels <- setNames(metriclabels[,2], metriclabels[,1])
metriclabels <- sort(setNames(paste(metrics[,1], metrics[,2]), metrics[,2]))

colours <- c(RColorBrewer::brewer.pal(length(set1metrics), "Set1"),
		RColorBrewer::brewer.pal(length(set2metrics), "Set1"),
		RColorBrewer::brewer.pal(length(set3metrics), "Set1"),
		RColorBrewer::brewer.pal(length(set4metrics), "Set1"))

colours <- setNames(colours, c(set1metrics, set2metrics, set3metrics, set4metrics))

data_selected$Metric <- factor(data_selected$Metric, levels = metrics[order(paste(metrics[,1], metrics[,2])),2])

data_selected <- data_selected[order(paste(data_selected$Facet, data_selected$Metric)),]
visualise_lines(simp, data_selected, x_column = "TriggeringThreshold", y_column="Value", title = NULL,
		colour_column = "Metric", colour_legendtitle = "Metric", colour_legenditemnames = NULL,
		facet_column = "Facet", facet_ncol = 1, filename = paste("SA_AllMetrics", setsimp$sim$id, 
				setsimp$sim$id, sep="_"),
		alpha = simp$fig$alpha, ggplotaddons = list(
				ggplot2::guides(fill=FALSE),
				ggplot2::scale_fill_manual(values=colours),
				ggplot2::scale_color_manual(values=colours, labels=metriclabels, guide=ggplot2::guide_legend(ncol=4)),
				#viridis::scale_color_viridis(discrete=TRUE, labels=metriclabels, guide=ggplot2::guide_legend(ncol=2)),
				ggplot2::facet_wrap(as.formula(paste("~",  "Facet")), ncol = 2, scales="free_y"),
				ggplot2::theme(legend.position = "bottom")), showsd = TRUE,
		returnplot = FALSE)
