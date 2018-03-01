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


simp$sim$scenario				<- "B1"
simp$sim$id <- "740-1"

results <- data.frame()
for (id in c(740:780)) {
	for (rs in c(0:19)) {
		simp$sim$id <- paste(id, rs, sep="-")		
		input_tools_load(simp, "csv_preAllocTable")
		data <- csv_preAllocTable
		data$Comp <- as.numeric(levels(csv_preAllocTable$Comp))[csv_preAllocTable$Comp]
		data$num <- data$Below + data$Above
		
		r <- list()
		r$meancomp <- sum(data$num * data$Comp) / sum(data$num)
		r$sdcomp <- sd(rep(data$Comp, times=data$num))
		r$id = id
		r$rs = rs
		
		results = rbind(results, r)
	}
}

nosubsidies <- results[results$id == 740,]
print(paste("Average Mean: ", mean(nosubsidies$meancomp)))
print(paste("Average SD: ", mean(nosubsidies$sdcomp)))

#ggplot2:gplot(results, ggplot2:aes(x="id", y="meancomp"))

craftyr::visualise_lines(simp, results, x_column = "id", y_column="meancomp", title = NULL,
		filename = paste("SA_CompMean", simp$sim$id, 
				simp$sim$id, sep="_"), showsd = TRUE)


