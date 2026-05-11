# Analyzing Lengthened Partials vs Full Range-of-Motion with a Bayesian Approach

## An Experimental Study Analysis – Callie Nguyen, Nathan Samarasena, Baixue (Doris) Zhang

## Submission Information

- **Date of submission:** May 11, 2026

## Project Structure

- `Datasets/`: Raw and intermediate data files used for analysis, plus data-preparation scripts.
- `Papers/`: Main and supporting literature PDFs used for the study.
- `R code/`: R scripts for reproducing analyses, simulations, Bayesian/frequentist comparisons, and generated figures.
- `Report/`: Project write-up and proposal documents.
- `README.md`: Project overview, research question, analysis plan, and references.
- `Analyzing-Lengthened-Partials-vs-Full-Range-of-Motion-with-a-Bayesian-Approach.Rproj`: RStudio project file for this repository.

## Dataset License

The dataset was obtained from the authors’ OSF repository. The OSF page lists the license as “No License,” so no explicit reuse license is provided. The dataset is used in this project for educational reproduction purposes with attribution to the original authors.

## Acknowledgments

Repository tasks in this project used [Cursor Agent](https://cursor.com), an AI-assisted tool in the Cursor editor.

## I. Introduction

Resistance training, or strength training, is a popular type of fitness for maintaining physical health. Many adults want to include resistance training in their routines for various reasons, such as general health, supplementary exercise for better physical performance, or achieving an aesthetic physique. During workout sessions, it is essential to always employ proper form and extend range of motion (ROM), which is the distance a muscle can move or be stimulated, to give your muscles the best opportunity to grow.

Experimental fitness studies have been researching ways to optimize muscle growth for many years, and recent focus has been on how ROM affects muscle growth in resistance training. Interestingly, these studies often employ Bayesian approaches to implement prior research within the experiments. The combination of interesting and relevant research with different statistical approaches draws us in to analyze such studies in more detail. Does stimulating the muscle with or without achieving full ROM yield different results? How does the Bayesian approach within these experimental studies change how frequentists interpret results?

## II. Research Question

This project investigates the following research question: Do different resistance training techniques, specifically full range of motion (ROM) versus partial range of motion (i.e. lengthened partials), produce different effects on muscle hypertrophy? As a secondary methodological objective, we will also compare how Bayesian and frequentist approaches affect the interpretation of the results.

## III. Related Literature

The primary article for this project is “Lengthened partial repetitions elicit similar muscular adaptations as full range of motion repetitions during resistance training in trained individuals” [1], and its data is published [here](https://osf.io/a6cpz/files/q9djw). In this study, the effect of exercise techniques on muscle hypertrophy was examined. This study does not have a traditional frequentist power analysis, but instead uses a simulation-based sample size determination prior to data collection, serving as a Bayesian alternative.

The first additional article is from Gschneidner et al., 2025 [2]. The data is available [here](https://github.com/jamessteeleii/lenthened_partial_trial/tree/main/data). This is a large randomized controlled trial comparing full ROM and partial ROM training using frequentist statistical methods (p-values and confidence intervals).

The second additional article is from Augustin et al., 2025 [3]. We’ve requested and received the data from the authors. Employing a Bayesian framework, this article compared strict resistance training to training using external momentum using a within-participant design. This study did not have a traditional power analysis, but instead used a Bayesian simulation-based approach to evaluate sample size adequacy and estimate the precision of models.

The last additional reading is from Moreno et al., 2024 [4], which is a review paper, so no data was collected. This article reviews existing studies on whether performing resistance exercise with a partial range of motion at long muscle lengths leads to greater muscle hypertrophy than full range of motion training.

## IV. Study Design in the Main Article

Wolf et al. employed a within-participant experimental design, where each participant serves as their own control [1]. A total of approximately 25 resistance-trained individuals completed an 8-week supervised training program. Experimental units are individual participants with each limb treated as a separate condition. The treatment is the full range of motion (fROM) and lengthened partial range of motion (pROM). The response variables are muscle thickness measured via ultrasound and strength endurance with the 10-repetition maximum test. The participants are trained twice per week, and each limb was randomly assigned to one of the two conditions using block randomization. This design controls for between-subject variability and increases statistical efficiency. Blinding was applied for outcome assessment and statistical analysis to reduce bias.

## V. Analysis Plan

In the main article, linear mixed-effects models are used to fit the data, including random effects for participants and fixed effects for condition and time. In the Bayesian framework, the author examined posterior distributions, credible intervals, and Bayes factors to evaluate evidence for differences between conditions. We will reanalyze the data and compare these results with frequentist methods using p-values, confidence intervals, and hypothesis tests. To be specific, there are three steps for our analysis plan:

### 1. Evaluation of the study design and reanalysis:

We will first examine the experimental design of the main article, including the within-participant structure, randomization procedure, choice of response variables, models, and measurement methods. This step will assess how the design supports the result and possible improvement. From here, we will reanalyze muscle hypertrophy outcomes from the public dataset using mixed-effect models and compare the conclusions with the article’s analysis. \### 2. Simulation-based power analysis: We will write a simulation in R to extend the data in the main article. The main article used a Bayesian simulation-based approach rather than a traditional frequentist power analysis. We will simulate data under the same experimental design and analyze it with a traditional frequentist approach, with p-values, confidence intervals, and hypothesis tests. \### 3. Comparison of statistical approaches: We will compare how Bayesian and frequentist statistical approaches are applied across the selected studies and how they influence the interpretation of results. Specifically, we will compare how the main article and assisting papers use different frameworks to analyze similar research questions about muscle hypertrophy. We will compare the power analysis, prior information, and interpretation of results.

## Citations

[1] M. Wolf, P. Androulakis Korakakis, A. Piñero, A. E. Mohan, T. Hermann, F. Augustin, M. Sapuppo, B. Lin, M. Coleman, R. Burke, J. Nippard, P. A. Swinton, and B. J. Schoenfeld, “Lengthened partial repetitions elicit similar muscular adaptations as full range of motion repetitions during resistance training in trained individuals,” PeerJ, vol. 13, p. e18904, Feb. 2025, doi: 10.7717/peerj.18904.

[2] D. Gschneidner, L. Carlson, J. Steele, and J. P. Fisher, “The effects of lengthened-partial range of motion resistance training of the limbs on arm and thigh muscle area: A multi-site randomised trial,” Journal of Sports Sciences, vol. 43, no. 23, pp. 2963–2976, Dec. 2025, doi: 10.1080/02640414.2025.2567805.

[3] F. Augustin, A. Piñero, A. Enes, A. E. Mohan, M. Sapuppo, M. Coleman, M. Wolf, P. Androulakis Korakakis, P. A. Swinton, J. Nippard, and B. J. Schoenfeld, “Do cheaters prosper? Effect of externally supplied momentum during resistance training on measures of upper body muscle hypertrophy,” International Journal of Exercise Science, vol. 18, no. 3, pp. 329–342, 2025, doi: 10.70252/GDBL2230.

[4] E. N. Moreno, W. A. Ayers-Creech, S. L. Gonzalez, et al., “Does performing resistance exercise with a partial range of motion at long muscle lengths maximize muscle hypertrophic adaptations to training?,” Journal of Science in Sport and Exercise, 2024, doi: 10.1007/s42978-024-00301-z.

[5]R Core Team, R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing, 2024. [Online]. Available: <https://www.R-project.org/>

[6] P.-C. Bürkner, “brms: An R package for Bayesian multilevel models using Stan,” Journal of Statistical Software, vol. 80, no. 1, pp. 1–28, 2017, doi: 10.18637/jss.v080.i01.

[7] Steele, James & Fisher, James & Androulakis Korakakis, Patroklos & Wolf, Milo & Kroeske, Bram & Reuters, Rob. (2021). Long-term time-course of strength adaptation to minimal dose resistance training: Retrospective longitudinal growth modelling of a large cohort through training records. 10.31236/osf.io/eq485.
