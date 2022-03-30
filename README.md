# cognition_discharge
This project is to identify the predictive value of preoperative cognition testing (called AD8 and SBT) for postoperative outcomes, including discharge to other than home, readmission within 30 days, ICU admission, length of stay, and several clinical outcomes. The analysis plan request is found in Analysis plan clinical outcomes 4.26.21 (1).docx

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

