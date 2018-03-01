source("C:/Data/LURG/workspace/CRAFTY_CoBRA_NetSens/config/R/simp-machine_T440p.R")
simp$sim$folder 	<- "_setA/_RegGlobMax"
setwd(paste(simp$dirs$simp, simp$sim$folder, "createbatch", sep="/"))
source("../simp.R")
simp$sim$parentf	<- "_setA"

#demandFolder = paste(simp$dirs$output$data, simp$sim$parentf, "/worlds/", simp$sim$world, 
#				"/regionalisations/",simp$sim$regionalisation, "/", simp$sim$scenario, "/", sep="")
demandFolder = paste(simp$dirs$output$data, simp$sim$folder, "/demand/demand04", "/", sep="")

shbasic::sh.ensurePath(demandFolder)

demands <- data.frame(
		"Year" 		= seq(simp$sim$starttick, simp$sim$endtick),
		"Cereal"	= seq(1300, 1400, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Meat"		= seq(330, 200, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Recreation"= seq(180, 200, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Timber"	= seq(9000, 11000, length.out= simp$sim$endtick - simp$sim$starttick + 1))

write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE13.csv", sep=""),
		row.names = FALSE)

# more timber demand in DE27, less in DE13
# more cereal demand in DE13, less in DE27
demands <- data.frame(
		"Year" 		= seq(simp$sim$starttick, simp$sim$endtick),
		"Cereal"	= seq(950, 1000, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Meat"		= seq(550, 300, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Recreation"= seq(110, 110, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Timber"	= seq(9400, 9500, length.out= simp$sim$endtick - simp$sim$starttick + 1))

write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE14.csv", sep=""),
		row.names = FALSE)



demands <- data.frame(
		"Year" 		= seq(simp$sim$starttick, simp$sim$endtick),
		"Cereal"	= seq(350, 400, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Meat"		= seq(630, 500, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Recreation"= seq(90, 100, length.out= simp$sim$endtick - simp$sim$starttick + 1),
		"Timber"	= seq(10000, 11000, length.out= simp$sim$endtick - simp$sim$starttick + 1))

write.csv(demands, file = paste(demandFolder,
				simp$sim$regionalisation, "_", simp$sim$scenario, "_demands_DE27.csv", sep=""),
		row.names = FALSE)