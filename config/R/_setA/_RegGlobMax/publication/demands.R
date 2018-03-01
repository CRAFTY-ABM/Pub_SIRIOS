source("C:/Data/LURG/workspace/CRAFTY_ImpressionsEU/config/R/simp-machine_T440pLURG2LURG.R")
source(simp$simpDefinition)

# ADAPT:
simp$sim$folder 	<- "_setA"	

setwd(paste(simp$dirs$simp, sep="/"))
source(paste(simp$dirs$simp, simp$sim$folder, "simp.R", sep="/"))

simp$sim$runids 	<- c("15-0")			# run to deal with
simp$sim$id			<- "15-0"		 	# ID to identify specific data collections (e.g. regions)
simp$sim$task		<- "15-0"			# Name of surounding folder, usually a description of task 
simp$sim$endtick	<- 2040

simp$sim$runids 	<- c("95-0")			# run to deal with
simp$sim$id			<- "95-0"		 	# ID to identify specific data collections (e.g. regions)
simp$sim$task		<- "95-0"			# Name of surounding folder, usually a description of task 


futile.logger::flog.threshold(futile.logger::INFO, name='craftyr')


#demand <- hl_aggregate_demand(simp,  ggplotaddons = ggplot2::theme(legend.position="none"), returnplot=T)

dataname = "csv_aggregated_demand"
data <- data.frame()
for (scenario in simp$sim$allscenarios) {
	simp$sim$id			<- scenario
	simp$sim$scenario <- scenario
	convert_aggregate_demand(simp)
	input_tools_load(simp, dataname)
	d <- get(dataname)
	d$Scenario <- scenario
	data <- rbind(data, d)
}

# aggregate regions:
data <- aggregate(subset(data, select=c("Demand")),
		by = list(Tick=data[, "Tick"], Scenario=data$Scenario,
				Service=data[,"variable"]),
		FUN=sum)

p <- visualise_lines(simp, data, y_column="Demand", title = "",
		colour_column = "Service",
		linetype_column = "Scenario",
		# TODO use simp$mdata$services or document setting NULL
		colour_legenditemnames = simp$mdata$conversion$services,
		filename = paste("AggregateServiceDemandAlLScenarios", sep="_"),
		alpha=0.7)