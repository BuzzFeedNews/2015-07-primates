# Primates in biodefense research: Data and analysis

This repository contains data and script to repeat the reported analysis. It contains the following:

- `facility.txt` `report.txt` Raw data files downloaded from the USDA's [Animal Care Information System](https://acissearch.aphis.usda.gov/LPASearch/faces/CustomerSearch.jspx) database on May 13, 2015.
- `facility.json` `report.json` Files to process/clean the data above using [Open Refine](http://openrefine.org/). Load each `.txt` file as a new project, select the `Undo/Redo` tab, click the `Apply` button and then paste the content of the corresponding `.json` file into the `Apply Operation History` box. Click `Perform Operations` and the data will be processed to edit field headings, and ensure that entries for primates are labelled consistently as `NONHUMAN PRIMATES` in the field `col_a`. Then export the data from each project as a tab-separated text file.
- `facility.tsv` `report.tsv` Processed/cleaned versions of the data, exported from Open Refine.
- `primates.r` [R](http://www.r-project.org/) script to repeat the analysis reported in the article, using the processed/cleaned data. The script will generate a static version of the published interactive chart.




