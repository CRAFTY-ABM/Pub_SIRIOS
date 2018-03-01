#############################################
## Determine IPCC Scenario Trends:

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

scenario <- "B1"
simp$sim$id			<- scenario
simp$sim$scenario <- scenario
convert_aggregate_demand(simp)
input_tools_load(simp, dataname)
d <- get(dataname)
d$Scenario <- scenario
data <- rbind(data, d)


# aggregate regions:
data <- aggregate(subset(data, select=c("Demand")),
		by = list(Tick=data[, "Tick"], Scenario=data$Scenario,
				Service=data[,"variable"]),
		FUN=sum)

data <- reshape2::dcast(data, formula=Tick~Service, value.var="Demand")
rownames(data) <- data$Tick
data$Tick <- NULL
preserve <-  list()
preserve$datanormed <- do.call(rbind,apply(data, MARGIN=1, FUN = function(x) x/data[1,]))

##############


source("C:/Data/LURG/workspace/CRAFTY_CoBRA_NetSens/config/R/simp-machine_T440p.R")
simp$sim$folder 	<- "_setA/_RegGlobMax"
setwd(paste(simp$dirs$simp, simp$sim$folder, "createbatch", sep="/"))
source("../simp.R")
simp$sim$parentf	<- "_setA"
simp$sim$scenario <- "B1"

#demandFolder = paste(simp$dirs$output$data, simp$sim$parentf, "/worlds/", simp$sim$world, 
#				"/regionalisations/",simp$sim$regionalisation, "/", simp$sim$scenario, "/", sep="")
demandFolder = paste(simp$dirs$output$data, simp$sim$folder, "/demand/demand05", "/", sep="")
datanormed <- preserve$datanormed
shbasic::sh.ensurePath(demandFolder)

## DE13
startdemands <- data.frame(
		"Cereal"	= 1300,
		"Meat"		= 330, 
		"Recreation"= 180, 
		"Timber"	= 9000)

demands <- do.call(rbind,apply(datanormed, MARGIN=1, FUN = function(x) x*startdemands))
demands$Year <- rownames(demands)
write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE13.csv", sep=""),
		row.names = FALSE)

## DE14
startdemands <- data.frame(
		"Cereal"	= 950,
		"Meat"		= 550, 
		"Recreation"= 110, 
		"Timber"	= 9400)

demands <- do.call(rbind,apply(datanormed, MARGIN=1, FUN = function(x) x*startdemands))
demands$Year <- rownames(demands)
write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE14.csv", sep=""),
		row.names = FALSE)

## DE27
startdemands <- data.frame(
		"Cereal"	= 950,
		"Meat"		= 630, 
		"Recreation"= 90, 
		"Timber"	= 10000)

demands <- do.call(rbind,apply(datanormed, MARGIN=1, FUN = function(x) x*startdemands))
demands$Year <- rownames(demands)
write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE27.csv", sep=""),
		row.names = FALSE)