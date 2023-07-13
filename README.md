**Replication of "Run-off Elections in the Laboratory" by Bouton et al. (2022)**

This repository holds the replication and analysis of the study titled "Run-off Elections in the Laboratory" originally conducted by Bouton, Laurent, et al., and published in The Economic Journal (132.641, 2022, pp. 106-146). Our replication reproduces the main findings of the original study and echoes its conclusions. Specifically, our analysis confirms the existence of minor and mostly non-significant disparities in electoral outcomes and voters' welfare between the two voting systems examined in the original study.

The original study's authors have provided only a cleaned dataset without the script required to transform the original zTree data output. As a solution, we have included our own script, 'raw_to_clean.R', that accomplishes this task. Please be aware that the 'raw data xls' folder is currently empty. Users are requested to populate this folder with the raw files extracted from the replication files provided by Bouton et al. (2022).

The output of our script, 'compiled_dataset.dta', is ready to be used with the Stata code (.do files) provided by Bouton et al. (2022).

Original replication files: https://academic.oup.com/ej/article-abstract/132/641/106/6310580.

**File Structure**

- raw_to_clean.R: Script to convert raw zTree output data to cleaned dataset.
- raw data xls: Folder where the raw data files from Bouton et al. (2022) need to be stored. (Note: This folder is currently empty)
- compiled_dataset.dta: Output from raw_to_clean.R, ready to be used with the .do files provided by Bouton et al. (2022).
