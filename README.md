# gp-personality
Changes in Big Five personality traits and life satisfaction over the transition to grandparenthood.

For the complete preprint including the supplemental materials, see *gp-manuscript-papaja.pdf*.
For the OSF project page, see [*link here*](https://osf.io/75a4r/).  

## Instructions to Reproduce

To reproduce the preprint, please follow these instructions:

### Securing the Data

This manuscript uses the Longitudinal Internet Studies for the Social Sciences (LISS) from the Netherlands and the Health and Retirement Study (HRS) from the United States. These are de-identified archival data available in the public domain. However, uploading the data or processed versions of it is not permitted. Access to the LISS can be gained at https://www.lissdata.nl/access-data/, and to the HRS at https://hrsdata.isr.umich.edu/data-products/conditions-of-use/. For the HRS, I've used the RAND versions of the data sets (https://hrsdata.isr.umich.edu/data-products/rand/).  
Download the data into the folder structure shown below (*raw*) or use your own folder structure and change the paths of the R-scripts described in the next sections accordingly. Please note that the organizations behind both LISS and HRS occasionally release updated versions of older datasets (e.g., to correct errors in specific variables). In the future, this might necessitate changing single file names when reading in the newly downloaded data in R. Although older file versions might not be available on the websites, access can be requested via email.  

### Folder Structure  

data  

* raw
    + HRS 
    + LISS 
        - Background Variables
        - Family and Household
        - Health
        - Housing
        - Income
        - Leisure
        - Personality
        - Religion
        - Work and Schooling
* processed
    + HRS 
    + LISS 

### Package Management

To bolster future reproducibility, we used the *renv* package ([*link here*](https://rstudio.github.io/renv/index.html)) to manage version control of the used packages and dependencies. We recommend attempting to reproduce this manuscript using the *renv* approach described below, but we also provide more basic instructions without *renv* at the bottom of this document. Please follow these steps:  

1. Restart R. Sometimes a fresh install is best, ideally with version 4.0.4. Windows users need to make sure that [Rtools4](https://cran.r-project.org/bin/windows/Rtools/rtools40.html) is installed.  
2. Download the following files and move them into the folder on your computer where the subfolder *data* is now contained:  
    + *gp-personality.Rproj*: All paths are defined in relation to the R-Project
    + *gp-manuscript-papaja.Rmd*: The main R markdown script that renders the final PDF manuscript
    + *gp-manuscript-papaja-appendix.Rmd*: Additional R markdown script that renders the supplemental materials (executed from the main script)
    + *gp-participant-flowchart.png*: Figure that was created externally and is loaded into R in the main script
    + *apa.csl*: Necessary for APA 7th edition which is not yet natively supported
    + *references-zotero.bib*: Contains all the references used in the manuscript
    + *references-r.bib*: Contains additional references on the R-packages used here
    + *renv.lock*: Contains information on all package and dependency versions
3. Open *gp-personality.Rproj* in RStudio. 
4. Install *renv* via the R console:  
`install.packages("renv")`  
4. Execute the following commands in the R console to 'import' all R packages as the correct versions from the *renv.lock* file:  
`renv::activate()`  
`renv::restore()`  
When prompted, answer affirmatively in the console ("y" / "Y"). This may take 15-30 minutes. At this stage, I encountered an error on Windows machines that the package *nloptr* could not be installed. This can be fixed by running `renv::equip()`, answering "y" in the console, and then running `renv::restore()` again (as explained [here](https://stackoverflow.com/questions/60779096/error-installing-packages-using-renvrestore); Rtools required!).  
You can check that the package management was successful, by executing `renv::restore()` again which should now say "The library is already synchronized with the lockfile." (or by running `renv::diagnostics()`). 

### Generating the Analysis Samples

1. Create another subfolder called *scripts* and download the following files into it:  
    + gp-compilesample-imp-HRS.R  
    + gp-compilesample-imp-LISS.R  
    + gp-compilesample-psm-HRS.R  
    + gp-compilesample-psm-LISS.R  
2. To load and clean the data and conduct multiple imputations on the covariate data, execute *gp-compilesample-imp-HRS.R* and *gp-compilesample-imp-LISS.R*. This will take several hours on most machines. Remember to run all scripts from within the R-project.  
3. Download the Excel sheet *gp-covariates-overview.xlsx* into the superordinate folder (where the R-project is). This file is needed for the covariate data frame created in the next two scripts.  
4. To perform the propensity score matching creating the final four analysis samples, execute *gp-compilesample-psm-HRS.R* and *gp-compilesample-psm-LISS.R* (this only takes a couple of minutes). The analysis samples are now saved in the *processed* subfolders.  

### Reproducing the Manuscript

The manuscript was generated using the *papaja* (Preparing APA Journal Articles) R package by Frederik Aust and Marius Barth. See https://github.com/crsh/papaja/ for instructions on how to install this package and http://frederikaust.com/papaja_man/ for more specific instructions on how to write APA manuscripts using *papaja*. Another requirement is a TeX distribution for which I used *tinytex* which can easily be installed via R.  
Please follow these steps to reproduce the manuscript. If you run into errors you can’t fix easily, it is generally a good idea to re-install R and RStudio and restart from above (make sure to delete old library folders, too). I used R version 4.0.4. If you still run into errors on Windows machines please make sure that you have [Rtools4](https://cran.r-project.org/bin/windows/Rtools/rtools40.html) properly installed, and that you run RStudio as administrator.  

1. Create the final PDF by opening *gp-manuscript-papaja.Rmd* and clicking "Knit" and answering "Yes" when prompted to install further Markdown-related updates. This might take 30 minutes on the first try depending on your system. After a fresh install, this takes especially long because TeX packages are initially loaded. Chunks with computationally intense estimations are cached which means that a second "Knit" with the same setup will take considerably less time (< 1 min.) because these computations are skipped and pasted in from before (if the code is unchanged), and because TeX is all set. Make sure that you generated the analysis samples prior to this step as described in the previous section.  

### Approach Without the *renv* Package  

1. If you attempted the *renv* approach first, run `renv::deactivate()` and delete all files in the superordinate folder (where the R-Project is) except for the one listed below and the subfolders *data* and *scripts*. If not, download the following files:  
    + *gp-personality.Rproj*: All paths are defined in relation to the R-Project
    + *gp-manuscript-papaja.Rmd*: The main R markdown script that renders the final PDF manuscript
    + *gp-manuscript-papaja-appendix.Rmd*: Additional R markdown script that renders the supplemental materials (executed from the main script)
    + *gp-participant-flowchart.png*: Figure that was created externally and is loaded into R in the main script
    + *apa.csl*: Necessary for APA 7th edition which is not yet natively supported
    + *references-zotero.bib*: Contains all the references used in the manuscript
    + *references-r.bib*: Contains additional references on the R-packages used here
2. Open *gp-personality.Rproj* first in RStudio and then open *gp-manuscript-papaja.Rmd*  
3. Install *tinytex* or use another TeX distribution. You might get an error message if you try to install *tinytex* but already have a different TeX distribution on your system. In this case you might need to use this distribution (or deinstall it; see [*link here*](http://frederikaust.com/papaja_man/introduction.html#software-requirements)). *tinytex* can be installed via the R console:  
`if(!"tinytex" %in% rownames(installed.packages())) install.packages("tinytex")`  
`tinytex::install_tinytex()`  
Mac users might get an error message at this point that a certain directory is not writable. I was able to fix this by following the steps outlined [*here* (link)](https://github.com/yihui/tinytex/issues/24/).  
4. Install *papaja* (see [*link here*](https://github.com/crsh/papaja/)) which is not available on CRAN for the time being. Execute the following commands in the R console:  
`if(!"remotes" %in% rownames(installed.packages())) install.packages("remotes")`  
`remotes::install_github("crsh/papaja@devel")`  
When installing the packages, you might get asked for updates of packages or "Do you want to install from sources the packages which need compilation?". This can unfortunately sometimes require a bit of trial & error between the different options until successful installation. Usually, answering "Yes" / "All" throughout works fine. It can also be helpful to "Restart R" (under "Session") between steps.  
5. Install package *citr* which is currently not on CRAN (https://github.com/crsh/citr/). To install, execute in the R console:  
`devtools::install_github("crsh/citr")`  
If this fails (on Windows), try the following steps: "Restart R" (under Session), and then type into the console `install.packages("cachem")`. Choose Option 3 (no update) when prompted. Then re-try the install command for *citr*.  
6. Install all other uninstalled packages specified in *gp-manuscript-papaja.Rmd* under "Chunk 1: setup". RStudio offers a button (see the banner below "Run" or "Knit") to install all missing packages that are loaded in the script.  
7. Create the final PDF from *gp-manuscript-papaja.Rmd* by clicking "Knit". This might take 30 minutes on the first try depending on your system. After a fresh install, this takes especially long because TeX packages are initially loaded. Chunks with computationally intense estimations are cached which means that a second "Knit" with the same setup will take considerably less time (< 1 min.) because these computations are skipped and pasted in from before (if the code is unchanged), and because TeX is all set. Make sure that you generated the analysis samples prior to this step as described in the previous section.  
8. On some Windows machines, I've run into errors at **Step 7** caused by certain Unicode characters contained in the script. Please make sure that you opened *gp-manuscript-papaja.Rmd* using the proper encoding: choose through the menu "File -> Reopen with Encoding -> UTF-8". If Knitting still fails, one last resort could be to choose a different TeX engine (see [*link here*](https://github.com/crsh/papaja/issues/133/)). In *gp-manuscript-papaja.Rmd* replace the line  
`output            : papaja::apa6_pdf`  
with  
```
output            :
   papaja::apa6_pdf: 
         latex_engine: xelatex
```

