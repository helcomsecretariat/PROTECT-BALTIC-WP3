For parallelisation, the workflow is:
	1. Run setup.R to generate spec_list.csv, which generates a list of all species in the dataset and keeps track of which have initiated running
	2. In RStudio, run ensemble.R using "Source as background job". This will run each species on a separate core

OR
	2. Run runpar.bat, which is a batch script to run the ensemble script in a loop on as multiple parallel processes
		NOTE: OneDrive won't let me put batch script on the drive so here it is runpar.txt - you should change to runpar.bat when you copy it 

I would suggest just running as background jobs in RStudio when testing the scripts, then use the batch script when ready to run for a lot of species
