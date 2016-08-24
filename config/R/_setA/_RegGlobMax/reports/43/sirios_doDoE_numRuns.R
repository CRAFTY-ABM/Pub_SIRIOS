##########################################################################################
# Post-process CRAFTY-CoBRA DoE output
#
# Project:		Pub_SIRIOS
# Last update: 	08/08/2016
# Author: 		Sascha Holzhauer
# Instructions:	Run maschine-specific SIMP file first (see craftyr documentation)
##########################################################################################

# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_RegGlobMax"	
simp$sim$task		<- "DoE" 

# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/common", sep="/"))

# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

library(plyr)

runs = 43
rseeds = 0:99
metricsdata <- data.frame()

# read metrics data
for (run in runs) {
	for (rseed in rseeds) {
		# for testing: run = 43; rseed = 99
		simp$sim$scenario				<- "A1"
		simp$sim$runids 	<- c(paste(run, rseed, sep="-"))
		simp$sim$id			<- c(paste(run, rseed, sep="-"))
		
		input_tools_load(simp, "data_metrics")
		agg_metrics <- data_metrics[data_metrics$Tick == 2040,]
		metricsdata <- rbind(metricsdata, cbind(agg_metrics, RandomSeed = rseed))
	}
}

dexp <- shdoe::shdoe_param_getDefaultDexp()

mdata <- reshape2::dcast(metricsdata,  RandomSeed  ~ Metric, value.var = "Value")

simp$fig$width	= 900
simp$fig$height = 1200

metriclabels <-  read.csv(file="../../reports/KeyTranslations.csv", header=FALSE, stringsAsFactors = FALSE)
metriclabels <- setNames(metriclabels[,2], metriclabels[,1])

dexp$doe$visual$gammaend <- 0.3

# plot required number of runs
output_visualise_initFigure(simp, outdir = paste(simp$dirs$output$figures, "lines", sep="/"), 
		filename = "RequiredNumberOfRunsV2_2040", ensurePath = TRUE)
shdoe::shdoe_visualise_requiredNumberOfRuns(data = list(mdata), 
		dexp = dexp,
		runconfig = NULL,
		columns = colnames(mdata)[-1],
		facetcolumns = NULL,
		ggplotaddon = list(ggplot2::theme(legend.position="bottom"),
				ggplot2::scale_colour_discrete(guide=ggplot2::guide_legend(title.position="top", ncol=2),
						name = "Response\nVariables", labels = metriclabels),
				ggplot2::scale_linetype_discrete(guide = ggplot2::guide_legend(title.position="top"), name = "Confidence Intervals", 
						labels = unique(dexp$doe$confintervals))))	
dev.off()

# print results to console:
shdoe_analyse_requiredNumberOfRuns(dexp, data = data[data$Metric == "VarChangesLu", "Value"], 
		name = "Example", gamma = 0.03, confidence = 0.99)

shdoe::shdoe_analyse_gamma4All(data = list(mdata), dexp, numruns = 5,
		confidence = 0.99,  # originally b
		name = "Gamma")

# output LaTeX tables as files
dexp$dirs$output$tables <- simp$dirs$output$tables
shdoe::shdoe_table_gammas(dexp, data = list(mdata), confidence = 0.95, numruns = c(3,5,7,8,10,20), name = 		NULL, label="crafty.netsens.doe.gamma")
