"""
Module to deal with processing and interpreting the data
"""

import data as d
import evaluation as e
from collections import OrderedDict
from itertools import combinations
import numpy as np  # For stats
from math import isnan
from tqdm import tqdm


def stock_diff_sd(s1, s2):
    diff = [a + b for a, b in zip(s1, s2)]
    return np.std(diff, ddof=1)


def gen_magic_pairs():
    pairs = \
        [(stock_diff_sd(
            [d.stock_change[company1, date] for date in d.dates],
            [d.stock_change[company2, date] for date in d.dates]),
            company1, company2)
            for company1, company2 in combinations(d.companies, 2)]
    pairs.sort()
    return pairs


def gen_magic_triples():
    """ Be prepared to wait a while """
    triples = \
        [(stock_diff_sd(
            [d.stock_change[company1, date] for date in d.dates],
            [d.stock_change[company2, date] for date in d.dates],
            [d.stock_change[company3, date] for date in d.dates]),
            company1, company2, company3)
            for company1, company2, company3 in combinations(d.companies, 3)]
    triples.sort()
    return triples


def gen_investment(budget):
    _investment = dict(zip(d.companies, [0] * len(d.companies)))
    investment = OrderedDict(sorted(_investment.items(), key=lambda x: x[0]))
    for _, co1, co2 in tqdm(d.magic_pairs):
        if budget >= d.stock_cost[co1] + d.stock_cost[co2]:
            curr_vol = \
                e.volatility(list(zip(
                    [d.stock_cost[c] for c in d.companies],
                    investment.values())))
            if isnan(curr_vol):
                curr_vol = float('inf')
            investment[co1] += 1
            investment[co2] += 1
            budget -= d.stock_cost[co1] + d.stock_cost[co2]
            new_vol = \
                e.volatility(list(zip(
                    [d.stock_cost[c] for c in d.companies],
                    investment.values())))
            if new_vol >= curr_vol:
                investment[co1] -= 1
                investment[co2] -= 1
                budget += d.stock_cost[co1] + d.stock_cost[co2]
        else:
            if budget >= d.stock_cost[co1]:
                curr_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c] for c in d.companies],
                        investment.values())))
                investment[co1] += 1
                budget -= d.stock_cost[co1]
                new_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c] for c in d.companies],
                        investment.values())))
                if new_vol >= curr_vol:
                    investment[co1] -= 1
                    budget += d.stock_cost[co1]
            elif budget >= d.stock_cost[co2]:
                curr_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c] for c in d.companies],
                        investment.values())))
                investment[co2] += 1
                budget -= d.stock_cost[co2]
                new_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c] for c in d.companies],
                        investment.values())))
                if new_vol >= curr_vol:
                    investment[co2] -= 1
                    budget += d.stock_cost[co2]
    # Return the current investment and money left up to now
    return investment, budget


def gen_global_investment(budget):
    investment, budget = gen_investment(budget)
    print("Accessing all companies")
    while True:
        i = 0
        for c in d.companies:
            if budget >= d.stock_cost[c]:
                curr_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c_] for c_ in d.companies],
                        investment.values())))
                print("Budget | Volatility:\t {} | {}"
                      .format(budget, curr_vol))
                if isnan(curr_vol):
                    curr_vol = float('inf')
                investment[c] += 1
                budget -= d.stock_cost[c]
                new_vol = \
                    e.volatility(list(zip(
                        [d.stock_cost[c_] for c_ in d.companies],
                        investment.values())))
                if new_vol >= curr_vol:
                    investment[c] -= 1
                    budget += d.stock_cost[c]
            else:
                i += 1
                if i > 5:
                    break
        if i > 5:
            break
    return (e.volatility(
        list(zip(
            [d.stock_cost[c] for c in d.companies], investment.values()))),
            ' '.join([str(investment[company]) for company in d.companies]))


def main():
    """ Try running the original doubles function overnight, see
        what results you get. """
    budget = int(input("Enter budget: "))
    print(gen_global_investment(budget))


if __name__ == "__main__":
    main()
