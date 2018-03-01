source(simp$simpDefinition)
simp$sim$folder 	<- "_setA/_RegGlobMax"
source(paste(simp$dirs$simp, simp$sim$folder, "simp.R", sep="/"))
setwd(paste(simp$dirs$simp, simp$sim$folder, "publication", sep="/"))

preserve$run = 80
preserve$seed = 0

simp$sim$scenario				<- "A1"
simp$sim$runids 	<- c(paste(preserve$run, preserve$seed, sep="-"))			# run to deal with
simp$sim$id			<- c(paste(preserve$run, preserve$seed, sep="-"))

knitr::knit('craftyr_paramtables.Rmd')
knitr::pandoc('craftyr_paramtables.md', format='docx')

browseURL("craftyr_paramtables_utf8.docx")	