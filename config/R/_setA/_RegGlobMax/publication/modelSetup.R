source(simp$simpDefinition)

simp$sim$folder 	<- "_setA/_RegGlobMax"
preserve$run		<- 80

setwd(paste(simp$dirs$simp, sep="/"))
source(paste(simp$dirs$simp, simp$sim$folder, "simp.R", sep="/"))

simp$sim$runids 	<- c(paste(preserve$run, "-0", sep=""))	# run to deal with
simp$sim$id			<- c(paste(preserve$run, "-0", sep=""))	# ID to identify specific data collections (e.g. regions)
simp$sim$task		<- c(paste(preserve$run, "-0", sep=""))	# Name of surounding folder, usually a description of task 
simp$sim$endtick	<- 2020



#Regions
input_tools_load(simp, "csv_LandUseIndex_rbinded")
cdata <- get("csv_LandUseIndex_rbinded")
celldata <- cdata[cdata$Tick == 2010,]

simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "param", sep="/"), 
		filename = "RegionSetup")

simp$fig$outlinesize		<- 0.5
simp$fig$countryshapes$alpha	<- 1
simp$fig$countryshapes$colour = "black"

simp$fig$width = 750
simp$fig$height = 400

aftNames			<- c("-1" = "Unmanaged", "0" = 'Intensive Cereal', "1" = 'Extensive Cereal', 
		"2" = 'Intensive Livestock', "3" = 'Extensive Livestock',
		"4" = 'Forester') #"5" = 'VLI_Livestock'


g <- input_shapes_countries(simp, 
		filename_shapes = "C:/Data/LURG/workspace/CRAFTY_CoBRA_NetSens/config/R/_setA/WorldX/GermanySouthWest.shp", 
		countries2show = NULL, countrycodedatacolumn = "NUTS_ID")

map <- visualise_cells_printPlots(simp, celldata, idcolumn = "Tick", valuecolumn = "LandUseIndex",
		title = "", filenamepostfix = "", legendtitle = "Land Use",
		factorial= TRUE, omitaxisticks = FALSE, ncol = if (!is.data.frame(celldata)) length(celldata) else 1, 
		coloursetname="AFT", legenditemnames = aftNames,
		theme = visualisation_raster_legendonlytheme, returnplot = TRUE,
		ggplotaddon = g
		)
print(map)

# Capitals
futile.logger::flog.threshold(futile.logger::INFO, name='craftyr')

simp$csv$cname_x <- "x"
simp$csv$cname_y <- "y"

simp$fig$width = 370
simp$fig$height = 300

hl_param_capital_map(simp, capitals = simp$mdata$capitals, 
		filenameorder = c("regions", "U", "datatype"),
		returnplot = FALSE, ggplotaddon = ggplot2::theme(legend.position="bottom"))
	
# AFTS
simp$fills$AFT
afts <- hl_plotAgentProductionParameters(simp, filenameprefix = "AftProduction_",
		filenamepostfix = "_multi_medium", ggplotaddons = ggplot2::theme(legend.position="none"))


############


simp$fig$init(simp, outdir = paste(simp$dirs$output$figures, "param", sep="/"), 
filename = "ModelSetup")
simp$debug$fig		<- 1
output_visualise_multiplot(sip = simp, cols=2, compFuncs, demand, map, afts)
simp$fig$close()


