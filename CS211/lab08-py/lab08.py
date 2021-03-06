
def drawdown(ns):
    """ Drawdown calculation on the assumption that the inputs are
        percentages. """
    peak = ns[0]
    dd, max_dd = 0, 0
    start_date_index, end_date_index, peak_index = 0, 0, 0
    init = 100
    for i, n in enumerate(ns):
        init += init * n / 100
        if init > peak:
            peak = init
            peak_index = i
        dd = 1 - (init / peak)
        if dd > max_dd:
            max_dd = dd
            start_date_index = peak_index
            end_date_index = i
    return max_dd * 100, start_date_index, end_date_index

# Raw data
data = [line.rstrip('\n').split('\t') for line in open('StockData.txt')]

# List of companies (not including the total market)
companies = data[0][1:-1]

# List of dates
dates = list(reversed([data[i][0] for i in range(1, len(data))]))

stocks = {}
for i, company in enumerate(companies):
    stocks[company] = \
        list(reversed([float(data[j][i + 1]) for j in range(1, len(data))]))

worst_company = None
best_company = None

# Just for some kind of start value
max_drawdown, max_sd, max_ed = drawdown(stocks["A"])
min_drawdown, min_sd, min_ed = drawdown(stocks["A"])

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

print("Most stable company: {}\nDrawdown is {:.2f}%"
      .format(worst_company, max_drawdown), end=" ")
print("between {} and {}".format(dates[max_sd], dates[max_ed]))
print("Least stable company: {}\nDrawdown is {:.2f}%"
      .format(best_company, min_drawdown), end=" ")
print("between {} and {}".format(dates[min_sd], dates[min_ed]))

