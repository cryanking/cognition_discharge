# cognition_discharge
This project is to identify the predictive value of preoperative cognition testing (called AD8 and SBT) for postoperative outcomes, including discharge to other than home, readmission within 30 days, ICU admission, length of stay, and several clinical outcomes. The analysis plan request is found in Analysis plan clinical outcomes 4.26.21 (1).docx


## TODO
- add dialysis to adjusters -> done
- add functional status ~ dyspnea to adjusters -> done
- add sex to adjusters -> done
- residence -> homogenous? Not sure what I was looking for
- strange death rates: maybe not na_false in 2022 after merge -> done
- little's mcar test
- why these threhsolds
  -- problem with joint measurement
  -- AD8 >3 is rare (2%), similar rate for SBT > 11, so not a ton of information
  DONE -- generated 2d table -> heatmap
  DONE -- generated adjusted 2d table
  NOTE: these results look weakerLR results: no to yes in abnormal is moving between the "midpoints" which is a larger difference potentially
  DONE: add cv loop to reduce over-fitting
  TODO: bootstrap loop for AUC
DONE Analysis: marginal diffference in proportions with CI, adjusted marginal diffference
DONE: Analysis: CI on OR clustering on procedure category
DONE: Ananlysis: absolute AUC with CI (plus confounder only model)
DONE: Analysis: y-index PPV, NPC, S, S before and after adding CI

Adding more adjusters -> numeric failure of vuong test for AD8 vs SBT
Reviewing mortality data: there is no clear pattern in the difference between the eras. It is consistent across years and is spread across multiple surgery types. The old cohort's mortality data is the same as used in a prior project that was found to be problematic. https://pubmed.ncbi.nlm.nih.gov/32389391/  I verified that the updated data was used.
 
# plan
- bootstrap:
- xgb with all covariates (default params or params leared on real data)
- predict population at each level of ad8 and sbt -> heat map + table with CI
- add heatmap of N = joint distribution, use arguments( Colv = NA, Rowv = NA) to avoid reordering
-old docker image lacks xgboost, added



# Process
- alternative_procedures.R
- pull_data.sh
- merging_actdata_to_cog.R 

# Status / TODO
- peek at epic data
    - cpap_flows.R pulls AD8, SBT to "/research/ActFast_Intermediates/epic_cpap_flows.csv"
    - preop_static.R pulls most covariates to "/research/ActFast_Intermediates/epic_preop_before_labs_text_notes.csv
    - pre_post_flow.R pulls los, readmit, ICU to "/research/ActFast_Intermediates/epic_flo_outcomes.csv"
    - rearrange.R pulls death from '/research/ActFast_Big/Epic Through 2020_11/Report 2.csv'
    - ActFastData/ActFast_Big/Epic Through 2020_11/Report 4.csv has procedure codes (by encounter CSN)
- the dockerfile has the annoying slowness of having to pull packages, could add a blank report and build

Key identifiers for the data are 
- EMPI (a unique identifier for each patient)
- PAN (a billing identifier - usually a hospitalization but sometimes longer. PAN should be nested in EMPI)
- PatientID or Surg_PatientID (an identifier for an anesthesia event, either a clinic visit or a surgery)

# description of data files
## metavision era
- 'CDS ADT.csv' : a record of each admission for a patient in (I think) a 90 day window around surgery. Identifiers are REFERENCE_NO (aka EMPI) and REG_NO (aka PAN)
- '2020_02_MV_LoS.csv' : a record of the admission and discharge time for the hospitalization around each surgery. Identifiers are PAN, EMPI, and PatientID (surgery event)
- '2020_01_15_Gregory_Cognative_Dysfunction_Data.xlsx' : a record of clinic visit data and discharge status (in the Other sheet). Identifiers are CPAPPatientID (clinic event not surgery event)
- "2020_01_King_ProcedureCodes_MostRecentCPAP.xlsx" : among other things, has the most recent CPAPPatientID for a Surg_PatientID


# description from my POV
4 types of ids: 
- EMPI/Ref_no, --> common to the patient irrespective of the visit
- MRN, 
- PAN/visitIDcode/Reg_no --> provides one hospitization event. PAN is different for each hospitilization event

must map PAN to EMPI or Reg_no to EMPI
To find readmissions, EMPI will be helpful.

'2020_02_MV_LoS.csv':  
- Pt_id --> specific to surgery
- PAN --> specific to hospitilization
- EMPI --> specific to person

We talked about 
- datasets(different identifiers)
- some people who do not appear on both datasets?
- links needed to bring in the extra procedure codes that brought many more patients in.

- signals with procedure codes has 1000 matchings.
- Actfast proc2 has more matchings with procedure codes i.e. around 2660
- newest preop location has 90 days data of procedures.
- file="/research/ActFast_Intermediates/matched_proc_codes.rdata" , Actfast_proc2, actfast_proc_late, procedure_data --> must use this file for further analysis.

- All_ids --> has Patient_id, EMPI, visitIDcode, ID code, DOS, DOB, case_id, person_id, stay_id, Aneststop, no_signals #Reference file for id's
- ActFast_proc2 --> Ref_no, PAN, ICD_procedurecode, description #older I2 data/stops 2016
- ActFastRoll2--> Ref_no, MRN, PAN, Anesthesia_start, Anesthesia_stop #older I2 data/stops 2016
- ActFastRoll--> MRN, PAN, DOS, Ref_no, Visit no, Reg_no, Anesthesia_start, Anesthesia_stop #post 2016 data
- ActFast_Proc--> Ref_no, Reg_no, ICD_procedure_code, description #post 2016 data
- Procedure_data--> Ref_no, Reg_no, Facility_concept ID, ICD version NO, ICD_procedure code, description. # recent I2 query

