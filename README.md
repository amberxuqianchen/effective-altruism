# Utilitarianism and Moral Judgments

## Project Description
This project investigates the relationship between utilitarian moral judgments and emotional responses. Utilitarianism is a moral theory that emphasizes maximizing overall happiness or utility by considering the greatest good for the greatest number. The research aims to understand how individuals with different moral orientations make judgments and how emotions influence their decision-making process.

### Research Rationale
Previous studies have suggested that utilitarian judgments may be associated with a greater reliance on reasoning rather than emotional responses. Damage to the prefrontal cortex, an area related to social emotions, has been shown to increase utilitarian moral judgments. However, emotions can also play a role in moral judgments by biasing social cognition. This research aims to explore the interplay between utilitarian judgment, emotions, and moral decision-making.

### Participants and Methodology
The study involves undergraduate participants divided into two groups: Effective Altruism (EA) group and non-EA group. The EA group consists of 81 participants, while the non-EA group consists of 92 participants. The research design utilizes Ecological Momentary Assessment (EMA) to capture participants' moral experiences and emotional responses in real-time. Participants are asked to provide responses over a 7-day period, with multiple assessments per day.

### Key Variables
- Oxford Utilitarianism Scale (OUS): Participants' scores on the scale are used to measure their levels of impartial beneficence (IB) and instrumental harm (IH), which reflect prioritization of the greater good over personal or group loyalty and willingness to cause harm for a greater good, respectively.
- Agency: Participants' perception of agency, distinguishing between self and other agency.
- Valence: The emotional valence associated with moral judgments, including self-blaming emotions (guilt, shame), other-blaming emotions (anger, disgust, contempt), self-praising emotions (pride), and other-praising emotions (gratitude, elevation).

### Analysis and Findings
The project involves several analyses and model replications. The initial steps include testing the agency * valence interaction as a sanity check and examining the rightness * agency model on Guilt and Shame Proneness (GASP) similarity. The GASP similarity serves as a measure of self-blaming emotions. Correlations between guilt/shame and OUS_IH/IB in the self-immoral cell are explored, considering the different relationships with the other three cells.

Additionally, the project aims to replicate models for specific emotional categories: self-blaming emotions (guilt, shame, embarrassment), other-blaming emotions (anger, disgust, contempt), self-praising emotion (pride), and other-praising emotions (gratitude, elevation). The agency * valence interaction is checked again as a sanity check.

Simplification of the model for OUS modulation is considered, focusing on specific emotions in self-moral and self-immoral cases. Correlations are initially explored, followed by hierarchical linear modeling (HLM) for a more comprehensive analysis.

The analyses are first conducted on the EA/non-EA dataset, and if time permits, the Oxford dataset is also considered.

### Conclusion
This project aims to contribute to our understanding of the relationship between utilitarian moral judgments, emotional responses, and decision-making processes. By examining various factors, such as agency, valence, and specific emotional categories, the research provides insights into the interplay between utilitarianism and emotions. The findings have implications for understanding moral decision-making and may contribute to future studies in the field.

## Repository Structure
- `1_data/`: All raw data stored here and should not be modified.
- `1_code/`: This directory contains all the R scripts for data processing, statistical modeling, result reporting, and visualization. They are numbered and named based on the general workflow. For specific detail about what each script does, please refer to "Code Descriptions" section below.
- `2_pipeline/`: This directory contains the preprocessing/preprocessed data (.csv file) for analysis.
- `3_output/`: The folder names are corresponded to the analysis scripts in code folder.
- `4_docs/`: Documentations.

## Code Descriptions
| File                                        | Description   |
| ------------------------------------------- | --------------|
| main.R                                      | Sets and manages file paths used in the program. |
| descriptive.R                               | Processes and analyzes data from Excel and CSV files. |
| GASP_RightAgency_descriptive.R              | Performs descriptive statistics and graphical analysis on the data. |
| correlation_4cell.R                         | Analyzes and displays the correlation between guilt and OUS_IH/IB in the data. |
| analysis.R                                  | Creates a linear mixed model for analysis and result reporting. |
| OUS_MoralAgency_HLM.R                       | Performs mixed linear model analysis on the data, where the main IV under study are moral valence/rightness, agency, OUS, and the DV is self-reported emotions. |
| OUS_RightAgency_4emotion.R                  | Creates and visualizes a series of models on moral agency and four emotions (self/other-praising/blaming) based on the data. |
| HarmAversion_MoralAgency_HLM.R              | Creates a mixed linear model to study data on harm aversion, moral rightness and moral agency. |
| HarmAversion_MoralAgency_bayesian.R         | Performs Bayesian linear mixed model analysis, with potential factors including k_self, moral, and agency. |
| OUS_MoralAgency_bayesian.R                  | Creates a Bayesian linear mixed-model, where the main variables under study are moral rightness, agency and OUS. |
| Tone_OUS_MoralAgency_HLM.R                  | Analyzes the impact of independent variables, such as moral rightness, agency, OUS_IB and OUS_IH, on tone (text-based positivity and negativity). |
| Tone_HarmAversion_MoralAgency_HLM.R         | Performs mixed linear model analysis on the data, examining the impact of factors such as moral, agency, and harm aversion on tone (text-based positivity and negativity). |
| GASP_RightAgency_HLM.R                      | Runs a mixed linear model with moral rightness and agency as IVs and text-based GASP as DV. |
| descriptive.ipynb                           | data processing and visualization. |


## Usage

To use these scripts, clone the repository and run the scripts locally. Please adjust the root directories and file paths according to your local environment. 

## Prerequisites

The R scripts use several libraries, please make sure you have the following R packages installed:

- lme4
- stargazer
- sjPlot
- ggplot2
- readxl
- nnet