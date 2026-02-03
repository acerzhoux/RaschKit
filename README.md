===== Steps to install RaschKit and set up ====

1. Run code below to install packages.  
   lapply(  
      c('rlang','bookdown', 'ggthemes', 'ggrepel', 'patchwork','rmarkdown',\
         'gdata', 'janitor', 'data.table','knitr', 'RColorBrewer', 'fs',\
         'lazyeval', 'writexl', 'ggpubr', 'qpdf', 'tidyverse', 'openxlsx',\
         'kableExtra', 'magrittr', 'Rcpp', 'tidyselect', 'grid', 'flextable',\
         'magick'
      ),  
      function(x){  
         if (!require(x, character.only=TRUE)){  
            install.packages(x)  
            library(x, character.only=TRUE)
         }
      }  
   )
2. Software compatibility.\
   ConQuest v5.47.5\
   conquestr v1.5.5\
   Raschkit v1.1.3

   ConQuest v5.33.5\
   conquestr v1.1.0\
   Raschkit v1.0.7

   ConQuest v5.33.5\
   conquestr v1.0.8\
   Raschkit v1.0.6

   ConQuest v5.27.0\
   conquestr v1.0.1\
   Raschkit v1.0.1

   ConQuest v5.23.4\
   conquestr v0.9.995\
   Raschkit v1.0.0

4. Run code below and explore .Rmd files in ‘rCode’ folder for data exploration, 
   calibration, equating, and DIF.
   * library(RaschKit)
   * install\_packages\_ls()
   * create\_folders()

===== v1.1.3 Updates =====

1. Changed folder 'output' to 'calibration'. Added a lower-level folder with
   name of 'run', the parameter of calibrateLst(), to folders such as 'input',
   'calibration', and 'data'. This makes it possible to use same 'test' name when
   more than one run is done for it.
2. Put item analysis summary files 'itn\_test.xlsx' inside folder 'calibration/run'.
   Inside it is also put the combined file 'itn\_run.xlsx' with format and sheets for tests'
   stats, flag note, flagged items, and each test's itn summary. Now, the hyperlink
   always works since files are not moved to new folders.
3. Updated functions to make them compatible with ACER ConQuest v5.47.5.
4. Replaced 'plyr' functions with base R to remove 'plyr' from prerequisite to
   solve 'rename' conflict with dplyr.
5. Replaced !!sym(DIFVar) with .data\[\[DIFVar]] and {{ DIFVar }} inside plot\_DIF\_group().
   Package installation order may cause errors with !!sym(DIFVar).

===== v1.1.2 Updates =====

1. CCC\_ipMap(): ensure iLab of thr insdie is character; ensure first words of
   labels that include apostrophes are correctly extracted; add step numbers
   to iNum and labels so the former is drawn (when condition satisfied) in ipmap
   and the latter are correctly mnerged.
2. equate2Type(): dealt with horizontal test forms with no grades in
   file names.
3. equateLst(): debugged a file reading issue so only one file is selected.
4. delta\_DIF\_dich(): updated to read its and shw outputs of ConQuest 5.45.2.
5. item\_stats(): updated to read \_shw.xls output of ConQuest 5.45.2. Add 'anchor'
   to put 'anchored' to column name of delta estimates.
6. calibrate(): do item summary and plotting for anchoring as well.
7. CCC\_ipMap(): cancel delta shift adjustment for anchoring. Temporarily set
   error to NA for anchoring.
8. CCC\_plot(): as\_roster defunt and caused error. Debugged.

===== v1.1.1 Updates =====

1. section\_keys(): ensure 'Max\_score' of argument 'keyDf' is numeric inside
   this function's environment to make subtraction work.

===== v1.1.0 Updates =====

1. Revised functions to make them compatible with CQ 3.40.0. Examples are
   getTest(), item\_stats(), deltaStepTransform(), and delta\_DIF\_dich\_step().
   With those revisions, earlier versions of CQ should still work.
2. Debugged read2one() to remove domains without flagged items in flag summary
   list and solved column type merge issue.
3. Added sigma method to several equating functions such as equateLst().
4. Read deltas from itn.xls file for deltaStepTransform().
5. Updated fre\_resps\_cat() by keeping the frequency when proportion is TRUE.

===== v1.0.9 Updates =====

1. Debugged add\_format() so both list and vector of flags work.
2. Debugged read2one() so equating without scaled scores also work.
3. Debugged DIFDimOne() so that variable checks occur after data are read.
4. Corrected misspelt Argument 'design\_effect' in '2 DIF.Rmd'.
5. Updated column names to 'Delta adj ...' in polytomous DIF results.
6. Updated average error calculation within plot\_DIF\_poly().
7. Updated df\_shw\_Term3() to read the 4th column for category number of DIFVar.

===== v1.0.8 Updates =====

1. Debugged a line in DIF\_poly\_shw().
2. Allowed for saving of .CQS files for DIF analyses.
3. Tested to work with ACER ConQuest 5.34.3.
4. Updated item summary note for Priority 4 flagging criteria so polytomous items
   were included.
5. Debugged read2one() so 'Key' variables of different types can merge.
6. Debugged DIFVarTests() so that checking of DIF variable categories takes into
   account empty string ''.
7. Debugged 'flagVec' in add\_format() to make flag color appear for some columns
   of DIF analysis results.
8. Debugged CCC\_ipMap() so that row number of fit index dataframe works whether  
   quick is used or not.

===== v1.0.7 Updates =====

1. Beautified formats (rounding, font, etc.) of ouput files such as int analysis.
2. Put item removal and multiple keys at one place (keyDf).
3. equateLst() uses list of cutoff points to accepts different thresholds.
4. Updated procedures for item step DIF analysis.
5. Kept at most one level up for path hyperlink so it will work on other machines.
6. Added argument 'useR' to calibrateLst() to determine whether 'R' is included.
7. On TestStats sheet of itn summary file, added average facility,
   averagecorrelation, and cronbach. Multiple keys were sorted and added.
8. Updated procedures to flag distractors for CCC comments. Each key of multiple
   keys was checked.
9. In CCC appendix, PVAvg1 was adjusted by delta mean to be consistent with CCC.
10. For anchoring methods, type and dim of arguments such as ancShift were checked.
11. Argument 'DIF\_adj\_cut' was changed to 'DIF\_std\_cut' in DIF-related functions
    such as DIFVarTests().
12. equate2Type() has new argument 'linkTypeLst' to replace 'type', 'grdIntVec',
    and 'forms' and added argument checks.

===== v1.0.6 Updates =====

1. Debugged Equate() where iDIF should have been used to iteratively select
   DIF item each step.
2. Debugged CCC\_ipMap() where any item has all responses correct.
3. Added function of reading .sav datasets to DIFVarTests() so processed .sav
   files with test names can be put in 'data' folder.

===== v1.0.5 Updates =====

1. Debugged poly\_recode() where a 'test' argument is needed.
2. Debugged CCC\_ipMap() where '.' is not recognizable after '|>'.

===== v1.0.4 Updates =====

1. calibrateCon() merges grade response dataframes and codebooks before performing
   free calibrarion by regressing on grade.
2. calibrateLst() calibrates a list of response dataframes and puts summary file
   with standard ACER template format into 'results' folder.
3. calibrateScale() gives three ways of automating anchor file preparation.
   Refer to arguments of 'ancShift', 'ancTest2Read', and 'ancDf' for reference.
   Now, inputting a shift number, test name, or unordered anchor dataframe is
   sufficient to guarantee correct delta (and step) order.
4. equateLst() performs DIF analysis on a list of delta dataframes. If indDfLst is
   available, its stats and plots are added to summary file.
5. DIFVarTests() perfomrs DIF analysis on a vector of DIF variables on a list
   of response dataframes. Summary files named with DIF variables are put in
   'DIF' folder. If test3term is given, DIF analysis on both item and steps
   are done and summaries are put side by side on 'summary' sheet in summary file.
6. For all summary files, statistics are read from .xls files to guarantee
   accurate extraction of columns.
7. ACER template and format are used on all summary files including item analysis,
   equating, and DIF analysis. Hyperlinks are inserted if necessary. Plots are
   inserted into Excel sheets for equating and DIF. Flags are colored. Reliabilities
   are extracted and put in summary file.
8. Renamed arguments to make functions easier for use such as respDfLst (list
   of response dataframes) and grdIntVec (vector of integers representing grades).
9. More checked are added inside functions to help users debug problems. For
   example, calibrate() stops if item labels contain any space.
10. Solved plotting error of plot\_DIF() when there are only a couple of
    items/anchors. Regression line and formula are added onto plot.
11. Allows double-key items to have CCC and keys to be added to 'Key' column of
    summary files.
12. QA2Df() checks equivalance of char variables and difference of numeric variables.
    Difference is shown if only one variable is NA on any row.
13. Both calibrateScale() and calibrate() allow for diagnostics on trial items
    after anchoring on core items.
14. Added statistics on option/score and test to CCC plots. Added color flags.

===== v1.0.3 Updates =====

1. DIF\_dim\_one() corrected order of reading data and checking input.
2. poly\_recode() outputs in 'data' folder a file showing which and how
   polytomous items not continuously scored are recoded.
3. freq\_resps\_cat() solved a bug of disappearing letter response types and
   supports NA frequencies now.
4. Added output types of ConQuest results in .xls format. This allows for
   extracting model results from .xls files to solve column reading issues.
5. Used openxlsx to add format, hyperlink, and flag color in item summary file.
6. Add fit indexes such as facility, discrimination, and Infit, and their plots
   to DIF and equating analysis wherever possible.
7. Added floating table of contents to equating html reports.
8. Added dotted lines of 1.96 times average SE to delta scatterplots in DIF and
   equating.
9. Removed anchor.xlsx and keys.xlsx files from 'data' folder. Now, users need to
   prepare a dataframe with needed information.

===== v1.0.2 Updates =====

1. Suppressed ConQuest, Rmd, and other unnecessary printing in R console.
2. install\_packages\_ls() included packages needed to knit Rmd files.
3. freq\_resps\_cat() ordered in numerical/alphabetic order score categories in column names
   of wide form. It also added response total 'N' in output and argument 'prop'
   for proportion.
4. calibrate() supports case weights with argument 'pweight'.
5. calibrate() accepts dataframe with covariates after response columns. Need
   specify 'n\_dims'.
6. Used .txt extension for input and output text files.
7. Inside functions, changed some %>% to |>, removed some dependencies, and
   added some package names to functions (ongoing).
8. 'facet' method of DIF analysis for polytomous variable shows DIF variable
   name in both table column name and plot legend. Also, a note is shown beside
   table to interpret results.

===== v1.0.1 Updates =====

1. section\_model() sets up correct model terms for group model used in DIF
   analysis on polytomous variable.
2. DIF\_poly\_shw() extracts from .shw files delta columns of all numeric.
3. plot\_DIF\_group() plots polytomous items.
4. plot\_DIF\_poly() allows for 30 overlapping labels.
5. DIF\_dich() reads facilities from collapsed columns in .its file (CQ 5.27.0).
