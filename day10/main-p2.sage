def parse_switch(var, switch_str, constraints):
    for num in switch_str[1:-1].split(','):
        constraints[int(num)] += var

def parse_jolt(jolt_str):
    return [int(num) for num in jolt_str[1:-1].split(',')]

def run(name):
    with open(name) as fh:
        total = 0
        for line in fh:
            p = MixedIntegerLinearProgram(maximization=False, solver='GLPK')
            w = p.new_variable(integer=True, nonnegative=True, name='w')
            strs = line.strip().split(' ')
            b = parse_jolt(strs[-1])
            dim = len(b)
            constraints = [0] * dim
            obj = 0
            for i,s in enumerate(strs[1:-1]):
                obj += w[i]
                parse_switch(w[i],s,constraints)
            p.set_objective(obj)
            for lhs,rhs in zip(constraints,b):
                p.add_constraint(lhs == rhs)
            total += p.solve()
        return round(total)
