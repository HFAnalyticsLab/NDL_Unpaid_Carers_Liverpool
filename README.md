![plot](https://github.com/tom-prendergast-thf/NDL_Unpaid_Carers_CM/blob/main/ndlbanner.png)

# Networked Data Lab: NDL Liverpool and Wirral analysis on Unpaid Carers in Liverpool and Wirral

#### Project Status: In-progress

## Project Description

This Networked Data Lab analysis by the NDL lab in Liverpool and Wirral focusses unpaid carers registered with GP, or in contact with or in receipt of support from adult social services

Please note that these research outputs have not yet been peer-reviewed and should be treated as preliminary.

## Data sources

This analysis used the following data: 

For Liverpool and Wirral:
-	Pseudonymised individual GP records with (experimental) linkage to household identifiers
-	Pseudonymised Adult Social Care (ASC) records for individuals who have requested or are in receipt of social care services

## How does it work?
Raw data are compiled and counts are taken for those registed with GP or in contact with ASC, or in receipt of ASC support
Relevnt data columns are selected and relabled for data integration
Dates for inclusion are limited. For ASC carers the index date is that for which a request for contact was made.
Counts of carers are aggregated to household level. Note that the household data linkage is experimental and may be adjusted in line with data management updates. 


## Requirements

•	These scripts were written in R but running these requires raw data which are not included in the folder <br/> 
•	Ouput tables include supressed values and are saved to .csv <br/> 


## Authors

- Jamie O'Brien jamie75@liverpool.ac.uk
- Ben Barr bbarr@liverpool.ac.uk
- Roberta Piroddi Roberta.Piroddi@liverpoolccg.nhs.org

## License

This project is licensed under the [MIT License](https://opensource.org/licenses/MIT).
