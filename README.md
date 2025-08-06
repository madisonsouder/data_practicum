# Declining Vaccination Rates and Reported Cases of Vaccine Preventable Diseases
## Source
The first set of data, Vaccination Coverage and Exemptions among Kindergartners, was published by the Centers for Disease Control and Prevention. It was collected by National Center for Immunization and Respiratory Diseases. The data is updated yearly and the earliest records are from the 2009 - 2010 school year. The data was access on October 31th, 2024 and download as a CSV file.

Centers for Disease Control and Prevention. (2024). *Vaccination Coverage and Exemptions among Kindergartners* [Data Set]. Centers for Disease Control and Prevention.\
https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data

An additional five data sets were access from the World Health Organization. Data sets on vaccination compliance for DTP, Polio, Measles, Rubella, and Hepitatis B were collected by the World Health Organization and the United Nations Children's Fund. The data is updated yearly. The data sets for DTP, Polio, Measles, and Rubella start in 1980. The Hepitatis B data sets stars in 1993. These data sets were downloaded as excel files on June 3, 2025. 

World Health Organization. (2024). *Diphtheria tetanus toxoid and pertussis (DTP) vaccination coverage* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/diphtheria-tetanus-toxoid-and-pertussis-(dtp)-vaccination-coverage?CODE=USA&ANTIGEN=&YEAR=

World Health Organization. (2024). *Poliomyelitis vaccination coverage* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/poliomyelitis-vaccination-coverage?CODE=USA&ANTIGEN=&YEAR=

World Health Organization. (2024). *Measles vaccination coverage* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/measles-vaccination-coverage?CODE=USA&ANTIGEN=&YEAR=

World Health Organization. (2024). *Rubella vaccination coverage* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/rubella-vaccination-coverage?CODE=USA&YEAR=

World Health Organization. (2024). *Hepatitis B vaccination coverage* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/hepatitis-b-vaccination-coverage?CODE=USA&ANTIGEN=&YEAR=

Six data sets on reported cases of vaccine-preventable disease were accessed from the World Health Organization. These data about reported cases of Diptheria, Total Tetanus, Pertussis, Measles, Mumps, and Rubella. The data is collected as a joint effort between the World Health Organization and the United Nations Children's Fund and updated annually. Data on Diptheria cases, Total Tetanus cases, Pertussis cases, and Measles cases start in 1980. Data on Mumps cases starts in 2000 and data on Rubella starts in 1997. The data was downloaded as excel files on June 7th, 2025. 

World Health Organization. (2024). *Diphtheria reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/diphtheria-reported-cases-and-incidence?CODE=USA&YEAR=

World Health Organization. (2024). *Tetanus reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/tetanus-reported-cases-and-incidence?CODE=USA&DISEASE=TTETANUS&YEAR=

World Health Organization. (2024). *Pertussis reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/pertussis-reported-cases-and-incidence?CODE=USA&YEAR=

World Health Organization. (2024). *Measles reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/measles-reported-cases-and-incidence?CODE=USA&YEAR=

World Health Organization. (2024). *Mumps reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/mumps-reported-cases-and-incidence?CODE=USA&YEAR=

World Health Organization. (2024). *Rubella reported cases and incidence* [Data Set]. World Health Organization.\
https://immunizationdata.who.int/global/wiise-detail-page/rubella-reported-cases-and-incidence?CODE=USA&YEAR=
### Collection
The CDC collect the kindergarten vaccination data from multiple sources and using multiple techniques:
-  1-Stage Cluster Sample
-  1-Stage Stratified Sample
-  2-Stage Cluster Sample
-  Census
-  Census (Pub.), Not Conducted (Pvt.)
-  Census (Pub.), Stratified 2-Stage Cluster Sample (Pvt.)
-  Census (Pub.), Vol. Response (Pvt.)
-  Convenient
-  Non-Random Cluster Sample
-  Not Conducted
-  Random Sample
-  Simple Random Sample
-  Stratified 1-Stage Cluster Sample
-  Stratified 1-Stage Sample (Pub.), Census (Pvt.)
-  Stratified 2-Stage Cluster Sample
-  Vol. Response
-  Vol. Response (Pub.), Census (Pvt.)
-  Vol. Response (Pub.), Not Conducted (Pvt.)

The additional data was collected using the WHO/UNICEF Joint Reporting Form on Immunization (JRF). 
### Extraction
All datasets were a one time download. The data downloaded from the CDC is a csv file. The data downloaded from the WHO is an excel file. 
## Data Cleaning
### Progam
All datasets were cleaned and examined using R through RStudio.
### Cleaning
**CDC Vaccination Coverage and Exemptions among Kindergartners**
1. Not Applicable (NAs) entries read as strings were convert to standard NA values.
2. All NAs in the Estimate column were omitted. This column main data being analyzed
3. Vaccine/Exemption entries listed as MMR (PAC) were ommitted. MMR (PAC) data was limited and not mentioned in the CDC metadata.
4. Typos in the Estimate (%) and Survey Type column were fixed
   - NR and Nreq were changed to NReq for consistency
   - Survey type entries were changed to title case
   - Extra spaces were removed
   - Prv. was changed to Pvt. for consistency
   - Public was changed to Pub.
   - Private was changed to Pvt.
   - Voluntary Response was changed to Vol. Response
5. The data type of the Estimate column was changed to numeric
6. The data was seperated by Geography Type:
   - State
   - National

**WHO Vaccination Coverage Data**
1. The five vaccination data sets were combined using rbind
2. Columns were changed to lowercase and renamed:
   - GROUP -> region
   - CODE -> region_abbr
   - NAME -> location
   - YEAR -> year
   - ANTIGEN -> antigen
   - ANTIGEN_DESCRIPTION -> dose_type
   - COVERAGE_CATEGORY -> estimation_category
   - COVERAGE_CATEGORY_DESCRIPTION -> estimation_type
   - TARGET_NUMBER -> target_number
   - 	DOSES -> doses
   - COVERAGE -> coverage
3. Entries in the region column labeled as "Exported:" were removed
4. The columns target_number and doses were removed
5. NAs in the coverage column were removed
6. HepB, birth dose total and HepB, birth dose (given within 24 hours of birth) were average and combined as HepB, birth dose
7. The data was seperate into three dataframes by dose, 1st dose, 2nd dose, and 3rd dose.

**WHO Reported Cases Data**
1. The six vaccination data sets were combined using rbind and then pivoted long ways
2. All columns names where changed to lowercase
3. Entries in the country/region column labeled as "Exported:" were removed
4. The data type for the cases column and the year column was changed to numeric
All the reasoning were included in RStudio as comments.
## Data Nuances
### Numerical Data
**CDC Vaccination Coverage and Exemptions among Kindergartners**
There are four numerical data columns. The following is their unit:
  - Estimate (%) - percent
  - Population Size - integer
  - Percent Surveyd - percent
  - Number of Exemptions - integer
An additional column was added called Not_required. This is a logical column to indicate where the vaccine is required in that state or not. This information was extracted from the Estimate (%) column.

**WHO Vaccination Coverage Data**
There are two numerical data columns:
  - year - integer
  - coverage - percent

**WHO Reported Cases Data**
  - year - integer
  - cases - integer 
### Column Names
**CDC Vaccination Coverage and Exemptions among Kindergartners**
  - Vaccine/Exemption - The specifc vaccine or if the entry is an exemption
  - Dose - Specifies the type of exemption
  - Geography Type - If the data is state specific or national
  - Geography - The specific state or if it is a national average
  - School Year - School year in which the data was collected in this format: Start of school year - end of school year
  - Estimate (%) - The percent of students who recieved this vaccine or have an exmeptions
  - Population Size - The size of the population in the geography type
  - Percent Surveyed - The percent of the population surveyed
  - Footnotes - Symbols to indicate specific notes about the data collected
  - Number of Exemptions - The number of exmeptions for the geography type
  - Survey Type - The type of surveyed used to collect the data
  - Not_required - If the vaccine in required in that geography type or not

**WHO Vaccination Coverage Data**
  - region - Type of region (only countries)
  - region_abbr - Abbreviation of the country (only USA)
  - location - The country (Only United States of America)
  - year - Year in which the data pertains
  - antigen - Abbreviation of for the vaccine including the dose
  - dose_type - The vaccine and the specifc dose
  - estimation_category - Abbreviation of the data source
  - estimation_type - Data source
  - coverage - Percent of vaccine coverage

**WHO Reported Cases Data**
  - country / region - The country data was collected from (Only United States of America)
  - disease - The disease being reported
  - year - The year in which the data pertains
  - cases - The number of reported cases
