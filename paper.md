---
title: 'StatAid: An R package with a graphical user interface for data analysis'
authors:
- affiliation: 1, 2
  name: Vincent Alcazer
  orcid: 0000-0003-1843-6286
date: "27 August 2020"
bibliography: paper.bib
tags:
- R
- Data analysis
- Medicine
- Science
- Survival analysis
affiliations:
- index: 1
  name: Cancer Research Center of Lyon, INSERM U1052, Lyon, FRANCE
- index: 2
  name: Hospices Civils de Lyon, Lyon, FRANCE

---

# Summary

Data analysis is a crucial step in every research project in life science. Every clinician or researcher is one day faced to the need of performing statistical analysis. However, few free accessible solutions exist to date and most of the reference softwares need a paid license. R is a free language allowing one to perform statistical analysis [@RCoreTeam:2017].
While the R environment is very powerful, its learning curve can be very steep at the beginning, especially for people with no previous coding skills or less time to learn. A graphical user interface has already been provided as an independent package, but its features are limited for medical and applied life science studies and its usage remains difficult and unintuitive for new users [@Fox:2005]. To my knowledge, no free open-source software directly designed for researchers with an intuitive interface and a collaborative/evolving environment has been proposed yet.

`StatAid` is a free open-source software provided as an R package allowing clinicians and researchers to perform statistical analysis through an intuitive graphical interface. It has been developed using the Shiny package [@Chang:2020]. Golem has been used for package compilation and deployment [@Guyader:2020].

The software guide the users through all the steps of a data analysis, including multiple features:

- Exploratory data analysis: distribution, count, missing-values and outliers check

- Descriptive analysis, simple comparative analysis and publication ready 'table 1' output 

- Publication-ready graph customization 

- Paired data analysis (case-control studies, repeated measures)

- Univariate analysis and models for continuous and categorical outcome: Correlation, linear and logistic regression 

- Univariate analysis and models for time-dependent outcome: Kaplan-Meier curves and cox regression 

- Multivariate analysis and models for continuous, categorical and time-dependent outcomes

Its user-friendly interface can guide any clinician or researcher even with no previous software experience for its statistical analysis. In addition to a local version, a ready-to-use online version [(https://vincentalcazer.shinyapps.io/StatAid/)](https://vincentalcazer.shinyapps.io/StatAid/) is also available, providing access to `StatAid` everywhere, even on hospital/research center computers where no external software can be installed.
 

# Statement of need 

`StatAid` is an R package designed to fit the needs for every-day statistical analysis in science. The package provides all the tools necessary to perform a complete data analysis in an intuitive ready-to-use graphical interface. Any user with no coding skill or no software with paid-license available can easily perform all the steps of a good statistical analysis, from data-exploration/quality check to multivariate modeling. The online version of `StatAid` renders it accessible everywhere, even on computers with restrictive software installation policies.

As an evolving open-source software, another goal of `StatAid` is also to fit specific needs by allowing users to request particular feature implementations for their study.

`StatAid` was designed to be used by clinicians, researchers, students and any person wanting to perform statistical analysis with no coding skills. It has already been used by multiple researchers and students for their PhD or medical doctors for their thesis. It was primarily designed for medical/life science data analysis but can easily be extended to other fields.


# References
