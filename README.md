survival_models
===============


This project currently implements Three traditional survival models in R language:

- Kaplan Meier Estimator
- Cox Proportional Hazard Model
- Accelerated Failure Model
    - Lognormal Distribution
    - Weibull Distribution

The codes are basicaly based on existing survival R packages, while two addtional opertations are supported:

- Query the survival distribution $$\hat{S}(t)$$ at any time point $$t$$
- Query the probability distribution $$\hat{f}(t)$$ at any time point $$t$$

For KM and Cox, we discretize time into several time intervals and

- $$S(t)$$ indicates the probability of surviving the time intervals prior to the interval where $$t$$ belongs
- $$f(t)$$ indicates the probability of dying at the time intervals where $$t$$ belongs

We also offered a continous version of KM and Cox with smoothing option (by loess function). Thus you can compute likelihood from both probability mass distribution (PMF) and probability density distribution (PDF).

However, for AFT model, only computing likelihood from PDF is supported in this project, as it is a full parametric model. (You can also discretize the intervals and compute the discrete PMF if you like)

Therefore, if you would like to use log-likelihood as a measurement of models for all them three, you should use the continuos version. That is because the likelihoods from a PDF and a PMF are not comparable at all.

**All models support left, right, interval censoring and event data.**

# KM Model

The input ``survData`` should be a data.frame with two variables

- time: event or censoring time
- delta: event indicator
	- 1: event
	- 0: right censoring
	- -1: left censoring
	- -2: interval censoring

This model is mainly based on the famouse [**survival** R package](http://cran.r-project.org/web/packages/survival/index.html) [1]. 

# Cox Model

Besides ``time`` and ``delta``, the input to Cox model also requires other features as variables in ``survData``. It also supports four kinds of data samples indicated by ``delta``.

In cox.R, you will find two functions, ``cox`` and ``cox2``. ``cox`` is based on the ****survival**** package [1], while ``cox2`` is based on  ****intcox**** package. The difference is that ``cox`` only supports event and right censoring data, while ``cox2`` supports all four kinds of data. The reason to keep ``cox`` here is that it is from the most popular package and may thus be preferred if you only have event and righ-censoring data.



# Accelerated Failure Model

This project supports AFT with Weibull and lognormal distribution for all four kinds of data. However, only continuos PDF can be computed and discrete PMF is not available, as it is a parametric model.

# Tips

1. Normalize features before using cox model


# Reference

[1] http://cran.r-project.org/web/packages/survival/index.html

[2] http://cran.r-project.org/web/packages/intcox/index.html




