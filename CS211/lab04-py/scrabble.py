from collections import Counter

chars = raw_input()
dictionary = [line.rstrip('\n') for line in open('/usr/share/dict/words')]
valid = [word for word in dictionary if not Counter(word) - Counter(chars)]
valid.sort(lambda x, y: cmp(len(y), len(x)))

print "Top ten suggestions:"
for i in range(10):
    print valid[i]

