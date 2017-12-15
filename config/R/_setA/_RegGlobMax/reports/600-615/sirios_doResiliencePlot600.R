##########################################################################################
# Create resilience plot
# - Calculate effects from DoE data (effects of parameter on metric)
# - Weight effects with varying metric weights
# - Identify parameter with highest effect
#
# Project:	Pub_SIRIOS
# Last update: 	08/12/2017
# Author: 	Sascha Holzhauer
# Instructions:	Run maschine-specific SIMP file first (see craftyr documentation)
##########################################################################################

# Usually also in simp.R, but required here to find simp.R
simp$sim$folder 	<- "_setA/_RegGlobMax"	
simp$sim$task		<- "DoE" # Name of surrounding folder, usually a description of task 

preserve <- list()
preserve$task 		<- simp$sim$task

# simp$dirs$simp is set by maschine-specific file:
setwd(paste(simp$dirs$simp, simp$sim$folder, "cluster/common", sep="/"))

# usually, the setting/scenario specific simp.R is two levels above:
source("../../simp.R")

# considered metrics:
set <- c("UnderSupply_Total", "OverSupply_Total", "VarChangesLu", "NumActions")

paramcolnames = c("TriggerThreshold", "SubsidyRate") #, "ActionLifetime", "Precedence")

# define resolution:
weights <- list(set1 = seq(0,100,25),
		set2 = seq(0,100,25),
		set3 = seq(0,100,5),
		set4 = seq(0,100,5))
weights <- expand.grid(weights[[1]], weights[[2]], weights[[3]], weights[[4]])

confidenceInterval = 0.9

# define signs (according to metrics in 'set'):
setSigns <- c(-1,-1,-1,-1)

simp$fig$width = 1500
simp$fig$height =1000
simp$fig$resfactor = 3

############### END of Parameter Section ######################

library(plyr)


# load data
storeid <- simp$sim$id
simp$sim$id <- "set600-615"
input_tools_load(simp, "data_metrics")
data <- data_metrics
simp$sim$id <- storeid

# Substitute letters by numbers:
substit <- c("G" = 1, "F" = 2)
data$Services <- substit[data$Services]
substit <- c("B" = 1, "C" = 2)
data$Precedence <- substit[data$Precedence]

dexp <- shdoe::shdoe_param_getDefaultDexp()

metriclabels <- read.csv(file="../../reports/KeyTranslations.csv", header=FALSE, stringsAsFactors = FALSE)
metriclabels <- setNames(metriclabels[,2], metriclabels[,1])

# normalize data (divison by max):
d <- apply(data[, set], MARGIN=2, FUN = function(x) max(abs(x)))
normd <- t(apply(data[, set], MARGIN=1, FUN= function(x) x/d))
colnames(normd) <- set

# Calculate effects:
fxlist <- list()
for (i in set) {
	fx <- shdoe::shdoe_analyse_effectSizes(normd[,i], dexp, 
			data[,paramcolnames], id = "Testdata", confidence= confidenceInterval)
	fx$response <- i
	fxlist <- append(fxlist, list(fx))
}
fx <- do.call(rbind, fxlist)

# Weight effects with varying metric weights:
pos_effectsums <- apply(weights, MARGIN=1, FUN = function(x) 
			 setSigns[1] * fx[fx$response == set[1], "effects"]*x[1] + 
			(setSigns[2] * fx[fx$response == set[2], "effects"]*x[2]) + 
			(setSigns[3] * fx[fx$response == set[3], "effects"]*x[3]) + 
			(setSigns[4] * fx[fx$response == set[4], "effects"]*x[4]))
rownames(pos_effectsums) <-  rownames(fxlist[[1]])

neg_effectsums <- apply(weights, MARGIN=1, FUN = function(x) 
			setSigns[1] * fx[fx$response == set[1], "effects"]*(-1)*x[1] + 
			(setSigns[2] * fx[fx$response == set[2], "effects"]*(-1)*x[2]) + 
			(setSigns[3] * fx[fx$response == set[3], "effects"]*(-1)*x[3]) + 
			(setSigns[4] * fx[fx$response == set[4], "effects"]*(-1)*x[4]))
rownames(neg_effectsums) <-  paste("-",rownames(fxlist[[1]]))

effectsums <- rbind(pos_effectsums, neg_effectsums)

# Identify parameter with highest effect:
params <- apply(effectsums, MARGIN=2, FUN = function(x) names(x)[match(max(x), x)])


data <- data.frame(params, weights[,1], weights[,2], weights[,3], weights[,4])
colnames(data) <- c("Parameter", set[1], set[2], set[3], set[4])

# create figure:
simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "lines", sep="/"), 
		filename = paste("ResilienceParams_600-615", sep="_"))

g <- ggplot2::ggplot(data, ggplot2::aes_string(set[3], set[4])) +
		ggplot2::facet_grid(facets = paste(set[1], set[2], sep="~")) +
		ggplot2::geom_point(ggplot2::aes(colour = Parameter), size=.7) + 
		ggplot2::coord_fixed(ratio = 1, xlim = NULL, ylim = NULL) +
		ggplot2::scale_x_continuous(metriclabels[set[3]], sec.axis = ggplot2::dup_axis(name = metriclabels[set[1]])) +
		ggplot2::scale_y_continuous(metriclabels[set[4]], sec.axis = ggplot2::dup_axis(name = metriclabels[set[2]]))
		#ggplot2::labs(x = metriclabels[set[3]], y = metriclabels[set[4]])
print(g)
dev.off()
