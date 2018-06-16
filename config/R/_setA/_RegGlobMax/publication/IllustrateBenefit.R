library(ggplot2)

simp$sim$runid <- "800-0"
simp$colours$Service 		<- c(	"-1" = "black",
		"Meat" 			= "darkorchid1",
		"Cereal" 	 	= "orange1",
		"Cereal_Subsidised"= "dodgerblue",
		"Recreation" 	= "royalblue2",
		"Timber" 		= "green4",
		"Biofuel" 		= "indianred1")

srcfilename = "Competition_linear_11" 
srcfilepath = dirname(paste(simp$dirs$data, simp$sim$folder,
				hl_getBaseDirAdaptation(simp),
				hl_getRunParameter(simp, "Competition_xml"), 
				sep="/"))
xrange = c(-0.5,0.5)
yrange = c(-1,1)

simp$fig$height = 400
filename = "competitionFunctions"
runidcolumnname="run"
returnplot = FALSE
	
	functions <- hl_getCompetitionFunctions(simp, srcfilename = srcfilename, srcfilepath = srcfilepath,
			runidcolumnname = runidcolumnname)
	functions$Recreation <- NULL
	
	functions$Cereal_Subsidised = function (x) {
		data = list(
				curve.class = "com.moseph.modelutils.curve.LinearFunction", 
				curve.a = "0.025",
				curve.b = "3.26", 
				.attrs.service = "SubsidisedCereal") 
		x * (get_xmlfunction_parameter_numeric(data, parameter = "curve.b", 
						default = 1)) + get_xmlfunction_parameter_numeric(data, 
						"curve.a", 0) + 0.15
	}
	p1 <- visualise_competition_funcs(simp, functions, xrange, yrange, filename = filename, returnplot = returnplot,
			#ggplotAddons = scale_y_continuous(limits = c(-1.5, 1.5)))
)
	if (returnplot) return(p1)