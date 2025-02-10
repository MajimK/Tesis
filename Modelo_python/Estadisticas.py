import numpy as np
import pandas as pd
import scipy.stats as stats
import matplotlib.pyplot as plt
import seaborn as sns
import os
from scipy.stats import wilcoxon

# Función para calcular medidas descriptivas
def descriptive_statistics(data):
    print("Medidas de Tendencia Central y Dispersión:")
    print(f"Media: {np.mean(data):.2f}")
    print(f"Mediana: {np.median(data):.2f}")
    print(f"Varianza: {np.var(data, ddof=1):.2f}")
    print(f"Desviación Estándar: {np.std(data, ddof=1):.2f}")
    print(f"Rango: {np.ptp(data):.2f}")

# Función para realizar pruebas de hipótesis
def hypothesis_tests(data1, data2):
    # Prueba de normalidad
    print("\nPruebas de Normalidad (Shapiro-Wilk):")
    stat1, p1 = stats.shapiro(data1)
    stat2, p2 = stats.shapiro(data2)
    print(f"Modelos  - Estadístico: {stat1:.3f}, p-valor: {p1:.3f}")
    print(f"Exploracion exhaustiva - Estadístico: {stat2:.3f}, p-valor: {p2:.3f}")
    
    # Comparación de medias (t-test o Mann-Whitney)
    if p1 > 0.05 and p2 > 0.05:
        print("\nPrueba t de Student:")
        t_stat, p_t = stats.ttest_ind(data1, data2)
    else:
        print("\nPrueba Wilcoxon:")
        t_stat, p_t = stats.wilcoxon(data1, data2)
    
    print(f"Estadístico: {t_stat:.3f}, p-valor: {p_t:.3f}")
    if p_t < 0.05:
        print("Las diferencias entre los algoritmos son significativas.")
    else:
        print("No hay diferencias significativas entre los algoritmos.")

# Función para realizar ANOVA
def perform_anova(*datasets):
    print("\nAnálisis de Varianza (ANOVA):")
    f_stat, p_value = stats.f_oneway(*datasets)
    print(f"F-Estadístico: {f_stat:.3f}, p-valor: {p_value:.3f}")
    if p_value < 0.05:
        print("Hay diferencias significativas entre los grupos.")
    else:
        print("No hay diferencias significativas entre los grupos.")

def charge_data(file_name):
    try:
        with open(file_name, 'r') as file:
            numeros = [float(linea.strip()) for linea in file if linea.strip()]
            return numeros
    except FileNotFoundError:
        print(f"El archivo '{file_name}' no se encontró.")
    except ValueError:
        print("El archivo contiene datos que no son números.")
    return None

path_to_data = os.getcwd()
path_to_data = os.path.join(path_to_data,"Resultados")
path_to_data = os.path.join(path_to_data,"Datos")

path_to_data_model = os.path.join(path_to_data,"resultado-2clients.txt")
path_to_data_lisp = os.path.join(path_to_data,"Resultados_lisp.txt")

data_model = charge_data(path_to_data_model)
data_lisp = charge_data(path_to_data_lisp)
# data_no_simplex = charge_data('../resultado_no_simplex.txt')
data_no_simplex = charge_data(os.path.join(path_to_data,"resultado_no_simplex.txt"))
stat1, p1 = stats.shapiro(data_no_simplex)
print(f"Datos  - Estadístico: {stat1:.3f}, p-valor: {p1:.3f}")

# Análisis descriptivo
descriptive_statistics(data_model)
descriptive_statistics(data_lisp)
if data_no_simplex is not None and len(data_no_simplex) > 0: descriptive_statistics(data_no_simplex)

# Pruebas de hipótesis
hypothesis_tests(data_model, data_lisp)
hypothesis_tests(data_model, data_no_simplex)

# ANOVA
if data_no_simplex is not None and len(data_no_simplex) > 0:
    perform_anova(data_model, data_lisp, data_no_simplex)

# Visualización
if data_no_simplex is not None and len(data_no_simplex) > 0:
    sns.boxplot(data=[data_model, data_lisp, data_no_simplex], notch=True)
    plt.xticks([0, 1, 2], ['Algoritmo 1', 'Algoritmo 2', 'Algoritmo 3'])
    plt.ylabel("Tiempo de Ejecución")
    plt.title("Comparación de Tiempos de Ejecución por Algoritmo")
    plt.show()
else:
    sns.boxplot(data=[data_model, data_lisp], notch=True)
    plt.xticks([0, 1], ['Algoritmo 1', 'Algoritmo 2'])
    plt.ylabel("Tiempo de Ejecución")
    plt.title("Comparación de Tiempos de Ejecución por Algoritmo")
    plt.show()
