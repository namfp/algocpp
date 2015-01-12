__author__ = 'hibou'

R = 1
L = 5



def create_new_line(last_line):
    if not last_line:
        return []
    count = 1
    i = 1
    last_val = last_line[0]
    results = []
    while True:
        if i >= len(last_line):
            return results + [count, last_val]
        elif last_line[i] != last_val:
            results += [count, last_val]
            last_val = last_line[i]
            count = 1
        else:
            count += 1
        i += 1




def compute(n, r):
    if n == 1:
        return [r]
    else:
        last_line = compute(n - 1, r)
        return create_new_line(last_line)




def output(l, r):
    if l == 1:
        return r

if __name__ == '__main__':
    print " ".join([str(i) for i in compute(6, 1)])

