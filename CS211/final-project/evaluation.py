"""
Module for interpretation of data; in particular for use
in the processing module
"""

import random
import numpy as np
from data import companies, stock_cost, dates, stock_change


def gen_investment(string):
    investment = []
    for company, num_stocks in zip(companies, string):
        investment.append((int(num_stocks), stock_cost[company]))
    return investment


def smart_random_investment(budget):
    investment = [(0, 0)] * len(companies)
    while True:
        r = random.randint(0, len(companies) - 1)
        rand_comp = companies[r]
        if budget >= stock_cost[rand_comp]:
            investment[r] = (investment[r][0] + 1, stock_cost[rand_comp])
            budget -= stock_cost[rand_comp]
        else:
            break
    return investment


def random_investment():
    investment = []
    for company in companies:
        investment.append((random.randint(0, 1), stock_cost[company]))
    return investment


def percent_change(investment, date):
    """ Measure of how much invested stocks changed at 'date' """
    total_change, total_investment = 0, 0
    for company, value in zip(companies, investment):
        total_investment += value[0] * value[1]
        total_change += stock_change[company, date] * value[0] * value[1]
    if total_investment == 0:
        return float('inf')
    else:
        return total_change / total_investment


def total_change(investment):
    """ Returns total percentage change associated with investment """
    total_changes = []
    for date in dates:
        total_changes.append(percent_change(investment, date))
    return total_changes


def volatility(investment):
    """ The primary method by which an investment is evaluated """
    return np.std(total_change(investment), ddof=1)
