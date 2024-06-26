# Script to check if the assessment works well before pushing the assessment 
# on ices-github (https://github.com/ices-taf).
# The steps to follow to run the assessment on TAF (https://taf.ices.dk/app/about) :
# 
# 1/ check the scripts and data (using this script)
# 2/ send an email to Colin Millar (colin.millar@ices.dk) to create a repo on
# (https://github.com/ices-taf)
# 3/ send an email to Colin Millar (colin.millar@ices.dk) once all the scripts
# and data are on repo to make the link with the App (https://taf.ices.dk/app/about)
# 4/ run the assessment on the App (https://taf.ices.dk/app/about) and check the
# log and outputs

library(icesTAF)

# remove all outputs
clean()

#  ==== preparing data and packages to run the script ====
# taf.bootstrap()
# software = FALSE in case there is no need to check with github packages' version
# otherwise it should be TRUE. (For the first check, it should be TRUE).
# when software = FALSE, taf.bootsrap only check data.bib
taf.bootstrap(software = FALSE) 

#  ==== running the assessment from scratch  ====
sourceAll()

#  ==== remove outputs and keep the code to download package and import data  ==== 
clean()

