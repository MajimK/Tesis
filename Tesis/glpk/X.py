import pulp
import numpy as np
from modelo import grraph

problem=pulp.LpProblem("Model-X",pulp.LpMinimize)

n_r=len(grraph.routes)
n_j1=len(grraph.bridges)
n_j2=len(grraph.bridges)
n_i=len(grraph.bridges)
rout=grraph.routes

P=grraph.route_cost    # Valores de P para cada r
S = grraph.S  # Valores de S
c = grraph.c  # Valores de c
K = grraph.K  # Valores de K
L = grraph.L  # Valores de L


sol=-1 
solutions={}        

X = pulp.LpVariable.dicts("X", ((r1, j1, j2, r2, i) for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(len(rout[r1])) for r2 in range(n_r) for i in range(len(rout[r2]))), cat='Binary')
Eliminar = pulp.lpSum((X[(r1, j1, j2, r2, i)] *(P[r1] - S.get((r1, j1, j2), 0) - c.get((r1, j1), 0) - c.get((r1, j2), 0) + L.get((r1, j1, r1, j2), 0)) for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(len(rout[r1])) for r2 in range(n_r) for i in range(len(rout[r2]))))
Sumar = pulp.lpSum((X[(r1, j1, j2, r2, i)] *(P[r2] - c.get((r2, i), 0) + S.get((r1, j1, j2), 0) + K.get((r1, j1, r2, i), 0) + L.get((r1, j2, r2, i), 0)) for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(len(rout[r1])) for r2 in range(n_r) for i in range(len(rout[r2]))))
problem += 3
peso=2* sum(P.values())



# Agregar restricciones al problema
            
problem += pulp.lpSum(X[(r1, j2, j1, r2, i)] for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(j1) for r2 in range(n_r) for i in range(len(rout[r2])))<=1
problem += pulp.lpSum(X[(r1, j2, j1, r2, i)] for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(j1) for r2 in range(n_r) for i in range(len(rout[r2])))>=1
problem += pulp.lpSum(X[(r1, j2, j1, r1, i)] for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(j1) for i in range(j2,j1+1))<=0
problem += pulp.lpSum(X[(r1, j2, j1, r1, i)] for r1 in range(n_r) for j1 in range(len(rout[r1])) for j2 in range(j1) for i in range(j2,j1+1))>=0
problem += Eliminar + Sumar <= peso

print("Eliminar:", Eliminar)
print("Sumar:", Sumar)
print("peso:", peso)
print("Problema después de agregar la restricción:")
print(problem)



cuts = pulp.GLPK_CMD(options=["--cuts"])
#cuts = pulp.GLPK_CMD(options=["--branch-and-bound"])
branch=pulp.GLPK_CMD(options=["--simplex"])

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
            solutions[var.name]=var.varValue

print(solutions)

with open('sol.txt','w') as f:
    f.write(str(solutions))