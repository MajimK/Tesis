import streamlit as st
import numpy as np
import streamlit as st
import numpy as np

import streamlit as st

def main():
    st.title("Llenar Matriz de Clientes por Ruta")

    num_rutas = st.number_input("Número de rutas:", min_value=1, value=1, step=1)
    num_filas = st.number_input("Número de filas en la matriz:", min_value=1, step=1)
    num_columnas = st.number_input("Número de columnas en la matriz:", min_value=1, step=1)

    matrices = []
    for ruta in range(num_rutas):
        st.header(f"Ruta {ruta + 1}")
        st.write(f"Ingrese los valores para la matriz de la Ruta {ruta + 1}:")
        matriz_ruta = []
        for i in range(num_filas):
            fila = st.text_input(f"Ingrese los valores para la fila {i + 1} separados por comas:")
            valores_fila = [float(valor.strip()) for valor in fila.split(',')]
            matriz_ruta.append(valores_fila)
        matrices.append(matriz_ruta)

    if st.button("Mostrar Matrices"):
        for idx, matriz in enumerate(matrices):
            st.subheader(f"Matriz de la Ruta {idx + 1}")
            st.write(matriz)

if __name__ == "__main__":
    main()
