import pulp

# Crear el problema
prob = pulp.LpProblem("MiProblema", pulp.LpMinimize)

# Definir las variables
x = pulp.LpVariable("x", lowBound=0, cat='Integer')
y = pulp.LpVariable("y", lowBound=0, cat='Integer')

# Definir la función objetivo
prob += 3*x + 5*y

# Definir las restricciones
prob += x <= 10
prob += x >= 3
prob += y <= 5
prob += y >= 2
prob += 3*y + 4*x <= 40
prob += 10*y + 4*x >= 20


# Resolver el problema utilizando ramificación y acotación
prob.solve(pulp.PULP_CBC_CMD(threads=1, msg=False))

# Imprimir el resultado
print("Estado:", pulp.LpStatus[prob.status])
print("Valor óptimo de la función objetivo:", pulp.value(prob.objective))
for v in prob.variables():
    print(v.name, "=", v.varValue)


# Crear el problema
prob = pulp.LpProblem("MiProblema", pulp.LpMinimize)

# Definir las variables
x = pulp.LpVariable("x", lowBound=0)
y = pulp.LpVariable("y", lowBound=0)

# Definir la función objetivo
prob += 3*x + 5*y

# Definir las restricciones
prob += x <= 10
prob += x >= 3
prob += y <= 5
prob += y >= 2
prob += 3*y + 4*x <= 40
prob += 10*y + 4*x >= 20

# Resolver el problema utilizando planos cortantes
cuts = 0
while True:
    prob.solve()
    cuts += 1
    
    # Obtener la solución actual
    current_solution = pulp.value(prob.objective)
    
    # Verificar si se necesitan agregar planos cortantes
    violated_cuts = []
    for r in prob.constraints:
        constraint = prob.constraints[r]
        if constraint.sense == pulp.LpConstraintLE and pulp.value(constraint) > constraint.constant + 1e-6:
            violated_cuts.append(constraint)
        elif constraint.sense == pulp.LpConstraintGE and pulp.value(constraint) < constraint.constant - 1e-6:
            violated_cuts.append(constraint)
    
    if len(violated_cuts) == 0:
        break
    
    # Agregar los planos cortantes violados al problema
    for cut in violated_cuts:
        for const in prob.constraints:
            if const == cut: continue
        prob += cut
    if len(prob.constraints)==0: break
    print(f"Iteración {cuts}: Valor óptimo = {current_solution}")
    
# Imprimir el resultado final
print("Estado:", pulp.LpStatus[prob.status])
print("Valor óptimo de la función objetivo:", pulp.value(prob.objective))
for v in prob.variables():
    print(v.name, "=", v.varValue)