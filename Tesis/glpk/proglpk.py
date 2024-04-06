import pulp
import numpy as np

S={0:3,1:10,2:5}
solutions=[]
sol=-1

problem=pulp.LpProblem("Model-X",pulp.LpMinimize)
X = pulp.LpVariable.dicts("X", ((i)  for i in range(3)),cat='Binary')

problem+=3

eliminar=pulp.lpSum(X[i]*S[i] for i in range(3))

problem += eliminar <= 8
problem += pulp.lpSum(X[i] for i in range(3))>=1
problem += pulp.lpSum(X[i] for i in range(3))<=1


print(problem)

while len(solutions)!=sol:

    sol=len(solutions)
    for var in problem.variables():
        if var.name in solutions:
            var.upBound=0
            var.lowBound=0

    problem.solve()

    # Mostrar el valor de las variables
    for var in problem.variables():
        if var.varValue != 0 and var.varValue != None:
            print(var.name, "=", var.varValue)
            solutions.append(var.name)

print(solutions)