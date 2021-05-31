# Rural_Internet_Hackathon

Bridging the Connectivity Challenge Hackathon
Keenan Viney

# NAME OF MY PROJECT

Econometric Analysis of Rural Internet in Alberta


## THE PROBLEM HACKED ON

To understand how internet access impacts rural economic outcomes, specifically exploring whether areas with better internet access had fewer layoffs due to COVID-19.


# TEAM MEMBERS

This was a solo project submission by Keenan Viney to the Bridging the Connectivity Hackathon hosted by ISAIC and Cybera.


## SUBMISSION DEMO VIDEO

Here is a link to a video I made that explains some of the background of this project:

[![ALT TEXT](https://img.youtube.com/vi/JkIMODgbMSg/0.jpg)%5d(https:/www.youtube.com/watch?v=JkIMODgbMSg)

https://youtu.be/JkIMODgbMSg

## DEMO WEBSITE, APP, REPORT, DASHBOARD, NOTEBOOKS, or OTHER

Please refer to www.deepforest.io for a forthcoming blog post that contains a write up on this project.


### PROJECT INSPIRATION
 
As of 2021, the CRTC has committed to spending up to $750 million dollars to hit their targets on rural internet speed and connectivity. This subsidy implies that there is either some market failure in rural internet provision or that there are social benefits to rural internet which would not sufficiently incentivize an ISP to build new infrastructure.


### WHAT IS THIS PROJECT

This project uses a panel data regression to investigate the relationship between layoffs and internet access during the COVID-19 pandemic. It is a quasi-natural experiment because other recessions would not have some layoffs contingent on the ability to work from home.


### HOW THIS PROJECT WAS BUILT

To complete the regression, it was necessary to spatially merge several datasets. This project makes use of Statistics Canada tables on Employment Insurance recipients, Age and Gender Demographics, as well as Open Alberta tables on COVID-19 cases, and a proprietary dataset on internet speeds. Once spatially joined, it was necessary to aggregate and clean the data, utilizing visualizations to better understand what the problem looked like. The final model selection is driven by statistical tests of three different model types to find the most appropriate technique.


### HOW IT WORKS

The reason for choosing a panel data method was twofold; the goal of the hackathon was to address rural internet access directly, so it made sense to split the province and explore the heterogeneity of internet access between census divisions across time. Another important reason to geographically segment the data is that there is not much variation in internet metrics across time. Exploiting the differences in speeds between regions helps disentangle the effects across space. 


### IMPACT

Indicate who should use your data product or analysis, and how it can make an impact on solving the challenges of rural connectivity. This is largely a first-pass analysis to understand how internet access impacted the amount of unemployment during the pandemic. Although not all recessions will be mitigated with internet access, it is possible to argue that this is key infrastructure in making rural communities more economically resilient.


### PROJECT CHALLENGES 

The main outstanding issue with this analysis is the relatively poor data quality for the CERB benefit. Unlike EI benefits, the current information on CERB recipients is not broken down beyond a provincial level. I use a discontinuity analysis that also uses age ranges to allocate CERB recipients by Census division but until more granular information is available this is only a best estimate and total Employment Insurance recipients is imprecisely measured.


### PROJECT ACCOMPLISHMENTS 

While the analysis used in this project is not overly complex, the code first approach allows for this framework to be used to study other aspects of COVID-19 and assess other policy questions. This project is a good example of data fusion, where a diverse set of open-source datasets can be combined to assess a novel question.


### LESSONS LEARNED

This project has had a few lessons; chiefly converting between spatial projections was an issue for some of the open-source shapefiles. Additionally, this is the first time I have had an opportunity to employ spatial unions. In this project, I aggregated local health regions into Census divisions making available many more Statistics Canada datasets that may not have a matching spatial resolution between them.
