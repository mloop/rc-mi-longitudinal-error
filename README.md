<<<<<<< HEAD
# bayesian-calibration-of-different-pwv-machines
=======
# Bayesian calibration of different PWV machines

This project attempts to solve a problem in studies using pulse wave velocity (PWV). In longitudinal studies, the device used to measure PWV can change due to companies going out of business, or simply differences in preference of the principal investigator funding the measurement of PWV. These changes naturally lead to difficulties when trying to compare values of PWV across different devices.

One way to approach this problem might be a latent variable model fit in the Bayesian framework. Briefly, you could treat each device's measurements as imperfect measurements of the underlying construct "PWV." Then, you could calculate the differences between the two latent variables and treat that as the difference you want to model over time. Covariates could be used to understand variation in this difference.

A helpful bit of information for such a study would certainly be information on the relationship between the two machines, measured on the same person at the same point in time. This information would improve prediction of the true value of the latent variable.
>>>>>>> master
