#!/usr/bin/python3

import pandas as pd

from scipy.stats import kstest
from scipy.stats import ks_2samp
# from scipy.stats import epps_singleton_2samp
import numpy as np
import scipy.stats as st
import warnings

warnings.simplefilter('ignore')


# Create models from data
def best_fit_distribution(data, bins=200, ax=None):
    """Model data by finding best fit distribution to data"""
    # Get histogram of original data
    print('Building...')

    y, x = np.histogram(data, bins=bins, density=True)
    x = (x + np.roll(x, -1))[:-1] / 2.0

    print('Start checking...')

    dist_names = ['alpha', 'anglit', 'arcsine', 'beta', 'betaprime', 'bradford', 'burr', 'cauchy', 'chi', 'chi2', 'cosine', 'dgamma', 'dweibull', 'erlang', 'expon', 'exponweib', 'exponpow', 'f', 'fatiguelife', 'fisk',
                  'foldcauchy', 'foldnorm', 'genlogistic', 'genpareto', 'genexpon', 'genextreme', 'gamma', 'gengamma', 'genhalflogistic', 'gilbrat', 'gompertz', 'gumbel_r',
                  'gumbel_l', 'halfcauchy', 'halflogistic', 'halfnorm', 'hypsecant', 'invgamma', 'invgauss', 'invweibull', 'johnsonsb', 'johnsonsu', 'ksone', 'kstwobign', 'laplace', 'logistic', 'loggamma', 'loglaplace',
                  'lognorm', 'lomax', 'maxwell', 'mielke', 'nakagami', 'ncx2', 'ncf', 'nct', 'norm', 'pareto', 'pearson3', 'powerlaw', 'powerlognorm',  'rdist', 'reciprocal', 'rayleigh', 'rice',
                  'recipinvgauss', 'semicircular', 't', 'triang', 'truncexpon', 'truncnorm', 'tukeylambda', 'uniform', 'vonmises', 'wald', 'weibull_min', 'weibull_max']  # , 'wrapcauchy']

    # Best holders
    dist_results = []
    best_distribution = st.norm
    best_params = (0.0, 1.0)
    best_sse = np.inf

    # Estimate distribution parameters from data
    for dist_name in dist_names:
        distribution = getattr(st, dist_name)

        # Ignore warnings from data that can't be fit
        with warnings.catch_warnings():
            warnings.filterwarnings('ignore')

            # fit dist to data
            params = distribution.fit(data)
            # print(str(params))

            # Separate parts of parameters
            arg = params[:-2]
            loc = params[-2]
            scale = params[-1]

            # print("Arg: " + str(arg))
            # print("Loc: " + str(loc))
            # print("Scale: " + str(scale))

            # Calculate fitted PDF and error with fit in distribution
            pdf = distribution.pdf(x, loc=loc, scale=scale, *arg)
            sse = np.sum(np.power(y - pdf, 2.0))

            size_data = len(data)
            if len(arg) == 0:
                dist_data = distribution.rvs(loc=loc, size=size_data, scale=scale)
            elif len(arg) == 1:
                dist_data = distribution.rvs(arg[0], loc=loc, size=size_data, scale=scale)
            elif len(arg) == 2:
                dist_data = distribution.rvs(arg[0], arg[1], loc=loc, size=size_data, scale=scale)
            elif len(arg) == 3:
                dist_data = distribution.rvs(arg[0], arg[1], arg[2], loc=loc, size=size_data, scale=scale)
            elif len(arg) == 4:
                dist_data = distribution.rvs(arg[0], arg[1], arg[2], arg[3], loc=loc, size=size_data, scale=scale)
            else:
                dist_data = distribution.rvs(loc=loc, size=size_data, scale=scale)
            print(distribution.name + ": Data generated")
            # print(str(dist_data))
            dist_data2 = []
            for i in dist_data:
                dist_data2.append(int(round(i)))
            test_stat = ks_2samp(data, dist_data)
            print(str(test_stat))
            test_stat = ks_2samp(data, dist_data2)
            print(str(test_stat))

            print(distribution.name + '\t sse: ' + str(sse), flush=True)
            print("\n")
            if sse > 0:
                dist_results.append((distribution.name, sse, test_stat.pvalue))

            # identify if this distribution is better
            if best_sse > sse > 0:
                best_distribution = distribution
                best_params = params
                best_sse = sse

    # sort results by sse
    sorted_dist_results = sorted(dist_results, key=lambda x: x[1])

    # print top 5 by sse
    print("Top 5 (sse):")
    print(sorted_dist_results[:5])
    print(sorted_dist_results)
    print(" ")

    # sort results by pvalue
    sorted_dist_results = sorted(dist_results, key=lambda x: x[2], reverse=True)

    # print top 5 by pvalue
    print("Top 5 (pvalue):")
    print(sorted_dist_results[:5])
    print(sorted_dist_results)
    print(" ")

    return best_distribution.name, best_params


def get_best_distribution(data):
    dist_names = ['alpha', 'anglit', 'arcsine', 'beta', 'betaprime', 'bradford', 'burr', 'cauchy', 'chi', 'chi2', 'cosine', 'dgamma', 'dweibull', 'erlang', 'expon', 'exponweib', 'exponpow', 'f', 'fatiguelife', 'fisk',
                  'foldcauchy', 'foldnorm', 'frechet_r', 'frechet_l', 'genlogistic', 'genpareto', 'genexpon', 'genextreme', 'gausshyper', 'gamma', 'gengamma', 'genhalflogistic', 'gilbrat', 'gompertz', 'gumbel_r',
                  'gumbel_l', 'halfcauchy', 'halflogistic', 'halfnorm', 'hypsecant', 'invgamma', 'invgauss', 'invweibull', 'johnsonsb', 'johnsonsu', 'ksone', 'kstwobign', 'laplace', 'logistic', 'loggamma', 'loglaplace',
                  'lognorm', 'lomax', 'maxwell', 'mielke', 'nakagami', 'ncx2', 'ncf', 'nct', 'norm', 'pareto', 'pearson3', 'powerlaw', 'powerlognorm', 'rdist', 'reciprocal', 'rayleigh', 'rice',
                  'recipinvgauss', 'semicircular', 't', 'triang', 'truncexpon', 'truncnorm', 'tukeylambda', 'uniform', 'vonmises', 'wald', 'weibull_min', 'weibull_max', 'wrapcauchy']
    dist_results = []
    params = {}
    for dist_name in dist_names:
        dist = getattr(st, dist_name)
        param = dist.fit(data)

        params[dist_name] = param
        # Applying the Kolmogorov-Smirnov test
        D, p = st.kstest(data, dist_name, args=param)
        print("p value for " + dist_name + " = " + str(p))
        dist_results.append((dist_name, p))

    # sort results
    sorted_dist_results = sorted(dist_results, key=lambda x: -x[1])

    # print top 5
    print("Top 5:")
    print(sorted_dist_results[:5])
    print(sorted_dist_results)
    print(" ")

    best_dist, best_p = (max(dist_results, key=lambda item: item[1]))
    # store the name of the best fit and its p value

    print("Best fitting distribution: " + str(best_dist))
    print("Best p value: " + str(best_p))
    print("Parameters for the best fit: " + str(params[best_dist]))

    return best_dist, best_p, params[best_dist]


df = pd.read_csv('instances.csv')

lst = df.input_cells

m = lst[0]
M = m
for i in lst:
    if i < m:
        m = i
    if i > M:
        M = i
print('Buckets: ' + str(M - m + 1))
# get_best_distribution(lst)
best = best_fit_distribution(lst, M - m + 1)

print(best)
