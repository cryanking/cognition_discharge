# cognition_discharge
This project is to identify the predictive value of preoperative cognition testing (called AD8 and SBT) for postoperative outcomes, including discharge to other than home, readmission within 30 days, ICU admission, length of stay, and several clinical outcomes. The analysis plan request is found in Analysis plan clinical outcomes 4.26.21 (1).docx

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

