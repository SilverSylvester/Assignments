# pylint: disable-all

import time

def drawdown(ns):
    """
    Drawdown calculation on the assumption that the inputs are
    percentages. 
    Returns: max drawdown, start date index, end date index,
    the indices for use in a dates list later on.
    """
    peak = ns[0]
    # Current drawdown per iteration and maximum drawdown so far.
    dd, max_dd = 0, 0
    # peak_index
    start_date_index, end_date_index, peak_index = 0, 0, 0
    for i, n in enumerate(ns):
        if n > peak:
            peak, peak_index = n, i
        dd = n - peak
        if dd < max_dd:
            max_dd, start_date_index, end_date_index = dd, peak_index, i
    return max_dd, start_date_index, end_date_index

# Raw data
data = [line.rstrip('\n').split('\t') for line in open('/home/conor/pyworkspace/StockData.txt')]

# List of companies
companies = data[0][1:]

# List of dates (in the correct order)
dates = list(reversed([data[i][0] for i in range(1, len(data))]))

# Stock value dict per company
stocks = {}
for i, company in enumerate(companies):
    stocks[company] = \
            list(reversed([float(data[j][i + 1]) for j in range(1, len(data))]))

worst_company = None
best_company = None

# Initial values
max_drawdown, max_sd, max_ed = drawdown(stocks["A"])
min_drawdown, min_sd, min_ed = drawdown(stocks["A"])

st = time.clock()

for i, company in enumerate(companies):
    dd, sd, ed = drawdown(stocks[company])
    if dd < max_drawdown:
        max_drawdown = dd
        max_sd, max_ed = sd, ed
        worst_company = company
    if dd > min_drawdown:
        min_drawdown = dd
        min_sd, min_ed = sd, ed
        best_company = company

et = time.clock()

print("Most stable company: {}\nMaximum drawdown is {:.2f}%"
        .format(best_company, min_drawdown), end=" ")
print("between {} and {}".format(dates[min_sd], dates[min_ed]))
print("Least stable company: {}\nMaximum drawdown is {:.2f}%"
        .format(worst_company, max_drawdown), end=" ")
print("between {} and {}".format(dates[max_sd], dates[max_ed]))
print("Time taken to process data: {:.3f}".format(et - st))

