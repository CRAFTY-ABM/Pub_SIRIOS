#######################################################################
# ApplicationScript for Storing CRAFTY ouptut as R data (both
# raw and aggregated).
#
# Project:		CRAFTY_NetSens
# Last update: 	23/06/2016
# Author: 		Sascha Holzhauer
#######################################################################

# Only contained when the particular script is only executed on a specific maschine!
# Otherwise. the maschine=specific file needs to be executed before.

source("/home/users/0033/uk052959/LURG/models/CRAFTY_CoBRA_NetSens/0.2.0_2016-05-26_18-03/config/R/simp-machine_cluster.R")
require(methods)

option_list <- list(
			optparse::make_option(c("-k", "--firstrun"), action="store", default= "0",
				help="First run to process (lowest run id to process)"),
			optparse::make_option(c("-n", "--numrun"), action="store", default= "1",
				help="Total Number of runs (1 more than highest run id to process - needs to respect available variables for CRAFTY invocation)"),
			optparse::make_option(c("-o", "--seedoffset"), action="store", default= "0",
				help="First random seed to process"),
			optparse::make_option(c("-r", "--numrandomseeds"), action="store", default= "1",
				help="Number of random seed to process, beginning with seed offset"),
			optparse::make_option(c("-p", "--noreport"), action="store_true",
				help="If given, no report is generated"))
opt	<- optparse::parse_args(optparse::OptionParser(option_list=option_list))


# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_RegGlobMax"	

simp$sim$task		<- paste(opt$run, opt$seed, sep="-") # Name of surounding folder, usually a description of task 

preserve <- list()
preserve$task 		<- simp$sim$task

# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/common", sep="/"))
# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

library(plyr)

runs = as.numeric(opt$firstrun):(as.numeric(opt$numrun)-1)
rseeds = as.numeric(opt$seedoffset):(as.numeric(opt$seedoffset) + as.numeric(opt$numrandomseeds) - 1)
for (run in runs) {
	for (rseed in rseeds) {
		# run = 229; rseed = 9
		preserve$run = run
		preserve$seed = rseed
		
		
		simp$sim$scenario				<- "B1"
		simp$sim$runids 	<- c(paste(run, rseed, sep="-"))			# run to deal with
		simp$sim$id			<- c(paste(run, rseed, sep="-"))
		
		#######################################################################
		futile.logger::flog.threshold(futile.logger::INFO, name='crafty')
		
		simp$sim$rundesclabel	<- "Runs"
		
	
		###########################################################################
		### Calculate Metrics
		###########################################################################
		simp$sim$regions	<- "Unknown"
		simp$sim$filepartorder <- c("runid", "D", "tick", "D", "regions", "D", "datatype", "D", "dataname")
		
		
		metrics <- rbind(
				metric_rasters_changedcells(simp, aft = NULL, dataname = "raster_landUseIndex"),
				metric_rasters_changes(simp, dataname = "raster_landUseIndex"),
				metric_aggaft_diversity_shannon(simp, dataname = "dataAggregateAFTComposition"),
				metric_rasters_global_patches(simp, dataname = "raster_landUseIndex", 
						directions = 8, relevantafts = c("NC_Cereal", "NC_Livestock")),
				metric_rasters_global_patches(simp, dataname = "raster_landUseIndex", 
						directions = 8, relevantafts = c("C_Cereal", "C_Livestock")),
				metrics_rasters_connectivity(simp, afts = c("NC_Cereal", "NC_Livestock"),
						dataname = "raster_landUseIndex"))
		
		simp$sim$regions	<- c("DE13", "DE14", "DE27")
		convert_aggregate_supply(simp, celldataname = "dataAgg")
		convert_aggregate_demand(simp, demanddataname = "csv_aggregated_demand", sourcedataname = "dataAggregateSupplyDemand")
		
		metrics <- rbind(metrics,		
				metric_aggaft_proportions(simp, afts = c("NC_Cereal", "NC_Livestock"), aftsname = "NC", 
						dataname = "dataAggregateAFTComposition"),
				metric_aggaft_proportions(simp, afts = c("C_Cereal", "C_Livestock"), aftsname = "C", 
						dataname = "dataAggregateAFTComposition"),
				metric_agg_supplydemand_maximum(simp, services=c("Cereal", "Meat", "Timber"), 
						datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = TRUE,
						consideroversupply = FALSE),
				metric_agg_supplydemand_maximum(simp, services=c("Cereal", "Meat", "Timber"), 
						datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = FALSE,
						consideroversupply = TRUE),
				# undersupply
				metric_agg_supplydemand_percentage(simp, service = "Total", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = TRUE,
						consideroversupply = FALSE),
				metric_agg_supplydemand_percentage(simp, service = "Cereal", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = TRUE,
						consideroversupply = FALSE),
				metric_agg_supplydemand_percentage(simp, service = "Meat", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = TRUE,
						consideroversupply = FALSE),
				metric_agg_supplydemand_percentage(simp, service = "Timber", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = TRUE,
						consideroversupply = FALSE),
				# oversupply
				metric_agg_supplydemand_percentage(simp, service = "Total", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = FALSE,
						consideroversupply = TRUE),
				metric_agg_supplydemand_percentage(simp, service = "Cereal", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = FALSE,
						consideroversupply = TRUE),
				metric_agg_supplydemand_percentage(simp, service = "Meat", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = FALSE,
						consideroversupply = TRUE),
				metric_agg_supplydemand_percentage(simp, service = "Timber", datanamedemand = "csv_aggregated_demand",
						datanamesupply = "csv_aggregated_supply",
						considerundersupply = FALSE,
						consideroversupply = TRUE),
				metric_agg_supplyperreg_simpson(simp, region = NULL, 
						datanamesupply = "csv_aggregated_supply"),
				metric_agg_supplyaccrossreg_simpson(simp, service = NULL, 
						datanamesupply = "csv_aggregated_supply"),
				metric_aggaft_diversity_simpson(simp, region = NULL,
						dataname = "dataAggregateAFTComposition"),
				metric_agg_regionalsupply_efficiency(simp, service = NULL, 
						datanamesupply = "csv_aggregated_supply",
						datanameaft = "dataAggregateAFTComposition",
						filenamepostfix="_mono_medium")
		)
		
		data <- do.call(rbind, lapply(simp$sim$regions, function(r) 
							metric_agg_supplydemand_percentage(simp, service = "Cereal", region = r, datanamedemand = "csv_aggregated_demand",
									datanamesupply = "csv_aggregated_supply",
									considerundersupply = TRUE, consideroversupply = FALSE)))
		metrics <- rbind(metrics, data.frame(aggregate(data.frame(Value=data$Value), by=list(Tick= data$Tick), FUN=mean), 
						Metric="RegionalUnderSupplyPercent_Cereal"))
		
		data <- do.call(rbind, lapply(simp$sim$regions, function(r) 
							metric_agg_supplydemand_percentage(simp, service = "Meat", region = r, datanamedemand = "csv_aggregated_demand",
									datanamesupply = "csv_aggregated_supply",
									considerundersupply = TRUE, consideroversupply = FALSE)))
		metrics <- rbind(metrics, data.frame(aggregate(data.frame(Value=data$Value), by=list(Tick= data$Tick), FUN=mean), 
						Metric="RegionalUnderSupplyPercent_Meat"))
		
		data <- do.call(rbind, lapply(simp$sim$regions, function(r) 
							metric_agg_supplydemand_percentage(simp, service = "Timber", region = r, datanamedemand = "csv_aggregated_demand",
									datanamesupply = "csv_aggregated_supply",
									considerundersupply = TRUE, consideroversupply = FALSE)))
		metrics <- rbind(metrics, data.frame(aggregate(data.frame(Value=data$Value), by=list(Tick= data$Tick), FUN=mean), 
						Metric="RegionalUnderSupplyPercent_Timber"))
		
		
		data_metrics <-  metrics
		input_tools_save(simp, "data_metrics")
	}
}

###########################################################################
### Draw Changes in Land Use (not useful when only one runid is executed)
###########################################################################
#datas <- data.frame()
#for (run in runs) {
#	simp$sim$runids <- c(paste(run, rseed, sep="-"))
#	simp$sim$id 	<- c(paste(run, rseed, sep="-"))
#	input_tools_load(simp, "data_landuse_changes")
#	datas <- rbind(datas, data)
#}
#visualise_lines(simp, datas, "Changes", title = "Changes in Land Use",
#		colour_column = "Runid") 