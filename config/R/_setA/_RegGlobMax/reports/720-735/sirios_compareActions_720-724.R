##########################################################################################
# Create action & metrics figure to compare runs
#
# Project:		Pub_SIRIOS
# Last update: 	10/12/2017
# Author: 		Sascha Holzhauer
# Instructions:	Run machine-specific SIMP file first (see craftyr documentation)
##########################################################################################

source(simp$simpDefinition)
simp$sim$folder 	<- "_setA/_RegGlobMax"
setwd(paste(simp$dirs$simp, simp$sim$folder, sep="/"))
source(paste(simp$dirs$simp, simp$sim$folder, "simp.R", sep="/"))

# Choose run to compare (ADAPT):
preserve$firstrunid 	<- 720
preserve$lastrunid	<- 724
preserve$run		<- 720
preserve$seeds		<- c(1) #c(0:19)	

scoreDoNothing		<- 0.5
addPerceivedGap		<- TRUE
addMetrics			<- TRUE

metrics2show		<- c("VarChangesLu", "SupplyPercentOver_Total", "SupplyPercentUnder_Total")
metrics4dots 		<- c("SupplyPercentUnder_Total", "SupplyPercentOver_Total")
		
monitoredServices 	<- c("Cereal", "Meat", "Timber")

regionConversion = c("DE13" = "Region A", "DE14" = "Region B", "Global" = "Global") #  "DE27" = "Region C",
runIdDescriptions	<- c("720-1" = "Short Action Runtime", "724-1" =  "Long Action Runtime")

#prefixes <- c("RegionalCompFuncSubsidyPa", "GlobalCompFuncSubsidyPa")
prefixes <- c("CompFuncSubsidyPa")

aftcolors <- setNames(simp$colours$AFT,
		simp$mdata$aftNames[match(names(simp$colours$AFT), names(simp$mdata$aftNames))]) 
aftcolors <- aftcolors[!is.na(names(aftcolors))]
actionfillcolours <- setNames(rep(aftcolors, times=length(prefixes)),
		paste(rep(prefixes, each=length(aftcolors)), gsub("_", "",names(aftcolors)), sep="_"))

customiseActionNames <- function(names) {
	names <- gsub("CompFuncSubsidyPa", "Subsidy:", names)
	#names <- gsub("RegionalCompFuncSubsidyPa", "Regional subsidy:", names)
	names <- gsub("_C", " intensive ", names)
	names <- gsub("_NC", " extensive ", names)
	names <- gsub("DoNothing", "Do nothing", names)
	names <- gsub("_", " ", names)
	return(names)
}

customiseActionDataComp <- function(dataActionsComp) {
	dataActionsComp <- dataActionsComp[as.character(dataActionsComp$Region) %in% regionConversion,]
	dataActionsComp$Region <- levels(dataActionsComp$Region)[dataActionsComp$Region]
	dataActionsComp[dataActionsComp$Agent %in% c("GlobalSubsidisingInst"), "Region"] <- "Global"
	
	dataActionsComp$Agent <- as.character(dataActionsComp$Agent)
	dataActionsComp[dataActionsComp$Agent %in% c("RegProvInst"), "Agent"] <- "Regional Institution"
	dataActionsComp[dataActionsComp$Agent %in% c("GlobalSubsidisingInst"), "Agent"] <- "Global Institution"
	dataActionsComp$Agent <- as.factor(dataActionsComp$Agent)
	
	dataActionsComp$Region <- as.factor(as.character(dataActionsComp$Region))
	return(dataActionsComp)
}

customiseMonitorData <- function(monitordata) {
	monitordata <- monitordata[monitordata$Region %in% c("Global"), ]
	
	monitordata <- plyr::ddply(monitordata, c("Region","Mode", "Runid"), function(df) {
		#df <- monitordata[monitordata$Mode == metrics4dots[1] & as.character(monitordata$Region) == "Global" & as.character(monitordata$Runid) == paste(preserve$firstrunid, preserve$seeds, sep="-") ,]
		if (unique(df$Mode) %in% metrics4dots) {
			df <- df[order(df$Tick),]
			for(i in 1:length(df$Tick)) {
				if ((i == 1 || (df$Tick[i-1] != df$Tick[i]-1)) & (i == length(df$Tick) || df$Tick[i+1] != df$Tick[i]+1)) {
					df[i, "Value"] <- NA
				}
				
				if (i > 1 && (df$Tick[i-1] != df$Tick[i]-1)) {
					row <- df[i,]
					row$Tick = df$Tick[i] - 1
					row["Value"] <- NA
					df <- rbind(df, row)
				}
			}
		}
		df
	})
	
	return(monitordata)
}

simp$fig$height <- 700
simp$fig$width <- 1000
filenameprefix <-  "InstitutionalActionMetrics_ThresholdSubsidy_Pub-R2"

############### END of Parameter Section ######################

metriclabelsRaw <-  read.csv(file="./reports/KeyTranslations2.csv", header=FALSE, stringsAsFactors = FALSE)
metriclabels <- setNames(paste("", metriclabelsRaw[,2]), metriclabelsRaw[,1])
metriccolours <- setNames(paste("", metriclabelsRaw[,3], sep=""), metriclabelsRaw[,1])

for (seed in preserve$seeds) {
	# for testing: seed = 19
	simps <- list()
	for(i in c(preserve$firstrunid,preserve$lastrunid)) {
		for(r in seed) {
			s <- simp
			s$sim$id					<- paste(i, "-", r, sep="")
			s$sim$shortid				<- paste(i, "-", r, sep="")
			s$sim$runids				<- c(paste(i, "-", r, sep=""))
			simps <- append(simps, list(s))
		}
	}
	
	simp$sim$endtick	<- 2040
	simp$sim$runids		<- paste(preserve$run, "-", preserve$seed, sep="")
	simp$sim$id			<- simp$sim$runids
	
	#####
	#####
	
	monitordata <- data.frame()
	dataActionsComp <- data.frame()
	dataSupplyComp <- data.frame()
	perceivedGap <- data.frame()
	metrics <- data.frame()
	
	for (simp in simps) {
		# for testing: simp = simps[[1]]
		
		# load actions data:
		input_tools_load(simp, "dataActions")
		dataActionsComp <- rbind(dataActionsComp, dataActions)
		
		# get supply/demand data:
		convert_aggregate_demand(simp)
		convert_aggregate_supply(simp, celldataname = "dataAgg")
		
		# store percental supply (regarding demand)
		input_tools_load(simp, objectName="csv_aggregated_demand")
		input_tools_load(simp, objectName="csv_aggregated_supply")
		colnames(csv_aggregated_demand)[colnames(csv_aggregated_demand) == "variable"] <- "Service"
		dataAggregatedPercentalSupply <- merge(csv_aggregated_supply, csv_aggregated_demand)
		
		# add totals of demand:
		data <- dataAggregatedPercentalSupply			
		sum <- aggregate(data[, c("Demand", "TotalProduction")], list(
						Tick	= data$Tick,
						Region	= data$Region,
						ID		= data$ID
				),
				FUN=sum)
		sum$Service <- "Total"
		
		world <- aggregate(data[, c("Demand", "TotalProduction")], list(
						Tick	= data$Tick,
						Service	= data$Service,
						ID		= data$ID
				),
				FUN=sum)
		world$Region <- "Global"
		
		dataAggregatedPercentalSupply <- rbind(data, sum, world)
		dataAggregatedPercentalSupply$PercentalSupply <- 100 * dataAggregatedPercentalSupply$TotalProduction / 
				dataAggregatedPercentalSupply$Demand
		dataSupplyComp <- rbind(dataSupplyComp, dataAggregatedPercentalSupply)
		
		# Misperception
		input_tools_load(simp, objectName="csv_PerceivedSupplyDemandGapTimber")
		input_tools_load(simp, objectName="csv_PerceivedSupplyDemandGapCereal")
		perceivedGap <- rbind(perceivedGap,
				csv_PerceivedSupplyDemandGapTimber,
				csv_PerceivedSupplyDemandGapCereal)
		
		
		# Metrics
		input_tools_load(simp, "data_metrics")
		
		metricsdata <-  data.frame()
		for (metric2show in metrics2show) {
			# metric2show = metrics2show[2]
			metricdata = data_metrics[data_metrics$Metric == metric2show,]
			colnames(metricdata)[colnames(metricdata)=="Metric"] <- "Mode"
			metricdata$Value <- metricdata$Value /
					metricdata[metricdata$Tick == min(metricdata$Tick, na.rm=T), "Value"] * 100
			if(nrow(metricdata) > 0) {
				metricdata$Region <- "Global"
				metricdata$Service <- metriclabels[metric2show]
				metricdata$Runid <- simp$sim$id
				metricdata$TotalProduction <- NA
				metricdata$Demand <- NA
				metricdata$Facet <- "Metrics"
				metricsdata <- rbind(metricsdata, metricdata)
			}
		}		
		
		metrics <- rbind(metrics, metricsdata)
	}
	
	# Process accumulated actions data:
	dataActionsComp <- hl_actions_fillDoNothing(simp, dataActionsComp, score = scoreDoNothing)
	
	# Process accumulated supply data:
	colnames(dataSupplyComp)[colnames(dataSupplyComp) == 
					"PercentalSupply"] <- "Value"
	colnames(dataSupplyComp)[colnames(dataSupplyComp) == 
					"ID"] <- "Runid"
	dataSupplyComp$Facet <- paste("Supply-demand gap")
	dataSupplyComp$Mode <- "PERCEIVED"
	dataSupplyComp$Value <- dataSupplyComp$Value-100 
	
	## Add perceived Gap:
	if (addPerceivedGap) {
		perceivedgap <- reshape2::melt(data = perceivedGap, id.vars = c("Tick", "Region", "Runid", "Service"), 
				measure.vars = c("VALUE_REAL", "VALUE_PERCEIVED"),
				variable.name = "Mode", value.name ="Value")
		perceivedgap$Mode <- sapply(strsplit(as.character(perceivedgap$Mode), "_"), function(x) x[2])
		perceivedgap$Region <- paste(perceivedgap$Region, "Gap perception")
		
		monitordata <- merge(dataSupplyComp, perceivedgap, all = TRUE)
	} else {
		monitordata <- dataSupplyComp
	}
	
	## Add metrics:
	if (addMetrics) {
		monitordata <- rbind(monitordata, metrics)
	}
	monitordata$Runid <-  as.factor(monitordata$Runid)
	if (!is.null(runIdDescriptions)) {
		levels(dataActionsComp$Runid) <- runIdDescriptions[levels(dataActionsComp$Runid)]
		levels(monitordata$Runid) <- runIdDescriptions[levels(monitordata$Runid)]
		dataActionsComp$Runid <- factor(dataActionsComp$Runid, levels=runIdDescriptions)
		monitordata$Runid <- factor(monitordata$Runid, levels = runIdDescriptions)
	}
	
	dataActionsComp$Action <- gsub("Global", "", dataActionsComp$Action)
	dataActionsComp$Action <- gsub("Regional", "", dataActionsComp$Action)
	dataActionsComp$Action <- factor(dataActionsComp$Action)
	
	# customise action labels
	actionLabels <- setNames(unique(dataActionsComp$Action), unique(dataActionsComp$Action))
	if (exists("customiseActionNames")) actionLabels <- customiseActionNames(actionLabels)
	names(actionLabels) <- unique(dataActionsComp$Action)
	
	dataActionsComp$Facet <- as.factor("Action scores")
	monitordata <- monitordata[monitordata$Service %in% c(monitoredServices, metriclabels),]
		
	dataActionsComp$Region <- levels(dataActionsComp$Region)[dataActionsComp$Region]
	dataActionsComp <- dataActionsComp[dataActionsComp$Region %in% names(regionConversion),]
	dataActionsComp$Region <- as.factor(regionConversion[as.character(dataActionsComp$Region)])
	
	if (exists("customiseActionDataComp")) dataActionsComp <- customiseActionDataComp(dataActionsComp)
	
	monitordata <- monitordata[monitordata$Region %in% names(regionConversion),]
	monitordata$Facet <- as.factor(monitordata$Facet)
	
	# test <- monitordata
	maxsupplydata 	<- monitordata[monitordata$Mode %in% metrics4dots, ]	
	if (exists("customiseMonitorData")) monitordata <- customiseMonitorData(monitordata)
	
	actfillcolours 	= c("DoNothing" = "grey", actionfillcolours)
	actfillcolours <- actfillcolours[order(names(actfillcolours))]
	
	# TODO
	
	#actfillcolours <- setNames(actfillcolours, levels(dataActionsComp$Action[!is.na(dataActionsComp$Action)]))
	#actfillcolours <- actfillcolours[1:length(levels(dataActionsComp$Action[!is.na(dataActionsComp$Action)]))]

	actiondata <- dataActionsComp[dataActionsComp$Selected == 1,]
	actfillcolours <- actfillcolours[names(actfillcolours) %in% unique(actiondata$Action[!is.na(actiondata$Action)])]

	visualise_actions(simp, 
			actiondata		= dataActionsComp,
			monitordata 	= monitordata,
			onlyselected 	= TRUE,
			y_column_measure = "Value",
			colour_column 	= "Service",
			size_column 	= NULL,
			facet_column 	= "Facet",
			lineseparator_column_actions = "Region",
			linetype_column_measures = "Region",
			linetype_column_actions = "Region",
			actionfillcolours 	= actfillcolours,
			monitorcolours 	= c(simp$colours$Service, setNames(metriccolours[metrics2show], metriclabels[metrics2show]),
					"Total" = "black"),
			filename = paste(filenameprefix, preserve$firstrunid, 
					preserve$lastrunid, seed, sep="_"),
			ggplotaddons = list(
					ggplot2::facet_grid(as.formula("Facet ~ Runid"), scales="free_y"),
					ggplot2::scale_fill_manual(name= "Action", values = actfillcolours, labels = actionLabels, na.translate = FALSE),
					ggplot2::scale_x_continuous(breaks= scales::pretty_breaks(n = 3)),
					ggplot2::guides(shape = ggplot2::guide_legend("Institution type", order = 1),
									linetype = ggplot2::guide_legend(order = 2),
									fill = ggplot2::guide_legend(order = 3, override.aes=list(colour=actfillcolours)),
									colour=ggplot2::guide_legend("Metric/Service", order = 4)),
					ggplot2::theme(legend.key.size=grid::unit(0.8, "lines"),
							axis.text=ggplot2::element_text(size=ggplot2::rel(0.6)))
					,ggplot2::geom_point(data = maxsupplydata, ggplot2::aes_string(x="Tick", y="Value"), 
							colour=metriccolours[maxsupplydata$Mode], alpha=0.2),
							ggplot2::theme(legend.key.size = grid::unit(4, "mm"), 
									legend.spacing.y = grid::unit(0.1, "cm"),
									legend.box.spacing = grid::unit(0.4, "cm")),
							ggplot2::labs(y="Values")
		)
	)
}
