#!/bin/sh

#LSF_DOCKER_VOLUMES='/storage1/fs1/christopherking/Active/ActFastData:/storage/ /storage1/fs1/christopherking/Active/lavanya/cognition_check:/research' bsub -G 'compute-christopherking' -n 4 -R 'rusage[mem=16GB] span[hosts=1]' -M 16GB -q general -a 'docker(cryanking/toy_container)' /bin/bash


## this assumes rclone "remote" is your box connection
## preferably decypt the secret before running container so that you don't have to interact with it

rclone copy remote:/2019_10_Gregory_Cognative_dysfunction /research
cp /storage/ActFast_BJ_Data/Mortality_update/metav.csv /research/metav.csv
cp /storage/ActFast_Epic_Flow/Data\ 202004/CDS\ data/CDS\ ADT.csv /research/CDS\ ADT.csv
cp /storage/ActFast_Epic_Flow/Data\ 202004/CDS\ data/CDS\ Demographics.csv /research/CDS\ Demographics.csv
cp /storage/ActFast_Epic_Flow/Data\ 202004/CDS\ data/CDS\ Visits.csv /research/CDS\ Visits.csv

## alternative_procedures.R creates a temporary file inside storage 
cp /storage/ActFast_Intermediates/matched_proc_codes.rdata /research
cp /storage/ActFast_Intermediates/CPAP_to_surg.csv /research

