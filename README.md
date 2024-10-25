# prompts-evaluation
Code underlying the impact evaluation of Jacaranda Health's PROMPTS intervention. Under review at PLOS Medicine.

This repository contains author-generated code underlying our impact evaluation findings. While we are unable to share the data required for these code files to run (even in de-identified or anonymized form, per the safety agreement under which our study data were collected), we have commented each script to ensure transparency of our analytical pipeline. The following files are included:

1. randomization.do: Stata script that performs the facility randomization procedure described in our manuscript
2. facilities.dta: Data file of facility characteristics upon which the facility randomization script is run 
3. results.R: R script containing our full analytical pipeline, with sections describing how we assessed for:
   a) balance of baseline sample characteristics
   b) attrition at antenatal and postnatal follow-up
   c) impact estimates for our summary indices and domain component measures
      - We also include code demonstrating how we generated our various summary indices
4. figures.R: R script that generates Figures 4 and 5 of our manuscript
