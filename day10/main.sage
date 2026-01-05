import itertools

Z2 = IntegerModRing(2)

def parse_light(light_str):
    return vector(Z2, [1 if x == '#' else 0 for x in light_str[1:-1]])

def parse_switch(dim, switch_str):
    result = vector(Z2, [0] * dim)
    for num in switch_str[1:-1].split(','):
        result[int(num)] = Z2(1)
    return result

def parse_jolt():
    return

def run(name):
    with open(name) as fh:
        total = 0
        for line in fh:
            strs = line.split(' ')
            b = parse_light(strs[0])
            switches = [parse_switch(len(b),s) for s in strs[1:-1]]
            M = Matrix(Z2, switches).transpose()
            K = M.right_kernel_matrix()
            combs = itertools.product([0,1],repeat=K.nrows())
            sol = M.solve_right(b)
            sols = [sol + vector(Z2,comb) * K for comb in combs]
            total += min([sum(int(x) for x in sol) for sol in sols])
        return total
