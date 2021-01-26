# EPA-Expert-Elicitation

The code used to generate the data rows is in the file GenerateDataRows.R. This code was organized and annotated into Data_Development.Rmd, which produces the .docx file Data_Development.docx which includes all relevant code and annotations.

The original raw data set Raw_Data.csv includes the variables fecal coliform, specific conductance, total nitrogen, total phosphorus, and turbidity. This complete data set was too large to upload to the online Git repository. It can be found in the EPA project dropbox folder. A filtered version, Water_Quality_Data.csv is the starting point for the Rmd file analysis.
Biotic index data originate from the MacroData.csv and are initially manipulated in the MacroInvertibrate_Analysis.R file before being joined to the dataset in the GenerateDataRows file.