source(simp$simpDefinition)
simp$sim$folder 	<- "_setA/_RegGlobMax"
setwd(paste(simp$dirs$simp, simp$sim$folder, sep="/"))
source(paste(simp$dirs$simp, simp$sim$folder, "simp.R", sep="/"))

# Choose run to compare (ADAPT):
preserve$firstrunid 	<- 700
preserve$lastrunid	<- 715
preserve$seeds		<- c(0:9)	

uActions <- c("")

for (run in preserve$firstrunid:preserve$lastrunid) {
	for (seed in preserve$seeds) {
		simp$sim$endtick	<- 2040
		simp$sim$runids		<- paste(run, "-", seed, sep="")
		
		simp$sim$id		<- paste(run, "-", seed, sep="")
		simp$sim$shortid	<- paste(run, "-", seed, sep="")


		input_tools_load(simp, "dataActions")
		
		if (!is.null(unique(dataActions$Action))) {
			uActions <- unique(c(uActions, unique(as.character(dataActions$Action))))
		}
	}
}

uActions
