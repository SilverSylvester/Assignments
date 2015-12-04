import statistics

n = int(input())
nums = map(int, input().split())

print(statistics.median(nums))

