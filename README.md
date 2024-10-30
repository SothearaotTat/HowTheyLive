1. atussum-0323.zip is the original file downloaded from https://www.bls.gov/tus/data/datafiles-0323.htm,
specifically, https://www.bls.gov/tus/datafiles/atussum-0323.zip.

2. AUT data.csv.gz retains most of the original data and variables with variables begining with "P" or "G"
being removed as they are of no interest from the beginning. Only the variables beginning with "T" and "t"
are kept.

3. Note also that the original data will not contain description of the variables in the second row. I have added
this to easily identify the variables. They are true variables descriptions according to the Multi-Year ATUS Data Dictionaries (PDF Files)
and Multi-Year ATUS Activity Lexicon (PDF File), which I have copied from the PDF and using Excel, was able to 
construct a list of all of them. This second row in AUT data.csv.gz is a result of this.

4. acitivities_var.csv.gz & recreation_code.csv.gz contains the data for the grouping of the variables in the datafile.
They are devised by me and cannot be found in the original data. Although the grouping follow a clear logic, biased
is not 100% eliminated.acitivities_var.csv.gz is based on understanding of the 2003–2023 ATUS Coding Lexicon (PDF) and
the Multi-Year ATUS Data Dictionaries (PDF Files). recreation_code.csv.gz, however, is purely based on my own reasoning.

5. script.rmd contain all the codes used to create the visualisation and app. I recommend not running the chunk for Shinny app
as it need some dependencies. There are some codes which are masked using # such as the code to save the visualisations, for
example. Since the data is too big, it will take too long for the app to run. I have save the preprocessed data separately to
be used in the apps. The codes to save these data are masked in the RMD file.

6. app.R should be used in conjunction with "www", Avg_rec_byYear.Rdata, AUT_dist_hr.Rdata, as they contain the data and image
used in the app. 

NOTE: I have realised too late that the data is called ATU for American Time Use Survey, not AUT. Don't be alarmed by the typo.
