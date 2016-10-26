### Python mean estimation
import numpy as np
import pandas as pd
from ggplot import *
import pystan

incomes = pd.read_csv('data/DP_LIVE_17102016202817844.csv', delimiter = ',')

### Remove Euro area and european union, and pick USD measure
disposable_incomes_oecd_2012 = incomes[(incomes.MEASURE == "USD_CAP") & (incomes.TIME == 2012) & (incomes.LOCATION != "EU") & (incomes.LOCATION != "EA")]

ggplot(aes(x = 'Value'), data = disposable_incomes_oecd_2012) +\
    geom_histogram() +\
    xlab("Household disposable income") +\
    ylab("Count")

### Data with "Taiwan"
model_data = {
    'N': len(disposable_incomes_oecd_2012.Value),
    'income': disposable_incomes_oecd_2012.Value
}

fit = pystan.stan(file = 'src/stan/mean.stan', data = model_data, chains = 4, iter = 1000)

posterior = fit.extract()

samples = pd.Series()
for i in xrange(len(posterior['mmean'])):
    samples = samples.append(pd.Series(np.random.gamma(posterior['mmean'][i], posterior['msd'][i], 100)))

simulated_and_real = pd.DataFrame({'income': samples, 'simulated' : "Yes"}).append(pd.DataFrame({'income': pd.Series(model_data['income']), 'simulated': "No"}))

ggplot(aes(x = 'income', colour = 'simulated'), data = simulated_and_real) +\
    geom_density(stat = 'density') +\
    xlab("Household disposable income") +\
    ylab("Count")
