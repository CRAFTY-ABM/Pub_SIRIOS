# CRAFTY SIRIOS (Simulation of Responsive Institutions On multiple Scales)

## Purpose

The CRAFTY SIRIOS (Simulation of Responsive Institutions On multiple Scales) model is designed to investigate dynamic action by formal institutions in land use change across scales and large spatial extents. SIRIOS can be used to explore the effects of human behaviour on land use transitions under a range of socio-economic and environmental scenarios. CRAFTY SIROS is based on the agent-based LUC modelling framework CRAFTY CoBRA, which is designed to be flexible, capable of handling a large variety of data, and applicable in a wide range of empirical or theoretical settings.

## Instructions

### Running the model

It is recommended to import the model into eclipse, but it runs as a stand-alone JAVA application, too.
To run the model in eclipse, right-click on the launcher file in ./config/launcher and choose "Run as..." > "Crafty_Pub_SIRIOS RegGlobMax 195". To run different runIDs, copy and modify the run configuration via "Run as..." > "Run Configurations...".


### Post-processing with craftyr

*craftyr* is an R package for the post-processing of CRAFTY results and can be installed with R commands ``devtools::install_bitbucket("S-Holzhauer/shbasic")`` and ``devtools::install_bitbucket("geoslurg/craftyr@sirios_rec1")`` (see package’s vignettes for further information).

 
## Further Information

This model code and data is tailored to the experiments presented in 
Holzhauer, S.; Brown, C. & Rounsevell, M. Modelling dynamic effects of multi-scale institutions on land use change, submitted to Regional Environmental Change. The Electronic Supplementary Material 2 included a ODD protocol of the model.


## Contact

If you have any further questions don't hesitate to contact Sascha.Holzhauer@ed.ac.uk 