#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <glpk.h>
#include "modelo.cpp"

using namespace std;

void optimization(const std::vector<vector<int>> &matrix_cost, const std::vector<vector<int>> &routes, int capacity, bool binary = true)
{

    Graph model(matrix_cost, routes);
    model.print_graph();
    cout << endl;

    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "Model-X");
    glp_set_obj_dir(lp, GLP_MIN);

    int n_r = model.routes.size();
    int var_count = 0;

    map<tuple<int, int, int, int, int>, double> X_indices;
    for (int r1 = 0; r1 < n_r; ++r1)
    {
        for (int j1 = 0; j1 < model.routes[r1].size(); ++j1)
        {
            for (int j2 = 0; j2 < model.routes[r1].size(); ++j2)
            {
                for (int r2 = 0; r2 < n_r; ++r2)
                {
                    for (int i = 0; i < model.routes[r2].size(); ++i)
                    {
                        if (j1 >= j2 || (r1 == r2 && i <= j2 && i >= j1))
                        {
                            continue;
                        }

                        var_count++;
                        X_indices[{r1, j1, j2, r2, i}] = var_count;
                    }
                }
            }
        }
    }

    glp_add_cols(lp, var_count);
    for (const auto &kv : X_indices)
    {
        const tuple<int, int, int, int, int> &key = kv.first;
        int index = kv.second;
        glp_set_col_name(lp, index, ("X_" + to_string(index)).c_str());
        // glp_set_col_kind(lp, index, GLP_BV); // GLP_BV asegura que X solo tome valores 0 o 1
        glp_set_col_bnds(lp, index, GLP_DB, 0.0001, 1.0); // 0 <= X <= 1
        glp_set_obj_coef(lp, index, 0.0);                 // Coeficiente inicial
        cout << "Index: " << index << " Key: (" << get<0>(key) << "," << get<1>(key) << "," << get<2>(key) << "," << get<3>(key) << "," << get<4>(key) << ")" << endl;
    }

    map<int, int> &P = model.route_cost;
    map<pair<int, int>, int> &c = model.c;
    map<tuple<int, int, int>, int> &S = model.S;
    map<tuple<int, int, int, int>, int> &K = model.K;
    map<tuple<int, int, int, int>, int> &L = model.L;

    // Recorre todas las rutas y nodos para calcular la función objetivo
    for (int r1 = 0; r1 < n_r; r1++)
    {
        for (int j1 = 0; j1 < model.routes[r1].size(); j1++)
        {
            for (int j2 = j1 + 1; j2 < model.routes[r1].size(); j2++)
            { // Aseguramos que j1 < j2
                for (int r2 = 0; r2 < n_r; r2++)
                {
                    for (int i = 0; i < model.routes[r2].size(); i++)
                    {
                        if (j1 >= j2 || (r1 == r2 && i <= j2 && i >= j1))
                        {
                            continue;
                        }
                        double eliminar = P[r1] - S[{r1, j1, j2}] - c[{r1, j1}] - c[{r1, j2}] + L[{r1, j1, r1, j2}];
                        double sumar = P[r2] - c[{r2, i}] + S[{r1, j1, j2}] + K[{r1, j1, r2, i}] + L[{r1, j2, r2, i}];
                        int indice = X_indices[{r1, j1, j2, r2, i}];
                        double coeficent_objetive = model.total_cost - P[r1] - P[r2];

                        // Agrega el término a la función objetivo
                        coeficent_objetive += (sumar + eliminar);
                        glp_set_obj_coef(lp, indice, coeficent_objetive);
                    }
                }
            }
        }
    }
    // Restricción de Unicidad: Suma de todas las X <= 1
    int global_row_idx = glp_add_rows(lp, 1);
    glp_set_row_bnds(lp, global_row_idx, GLP_LO, 1.0, 0.0); // Configura el límite superior como 1.0

    std::vector<int> indices;
    std::vector<double> coeficientes;

    for (const auto &kv : X_indices)
    {
        int var_index = kv.second;
        indices.push_back(var_index);
        coeficientes.push_back(1.0);
    }

    // Aplicar la restricción global con todos los índices y coeficientes de las X
    glp_set_mat_row(lp, global_row_idx, indices.size() - 1, indices.data(), coeficientes.data());

    // Esto es para garantizar la igualdad

    // int global_row_idx1 = glp_add_rows(lp, 1);
    // glp_set_row_bnds(lp, global_row_idx1, GLP_UP, 0.0, 1.0); // Configura el límite superior como 1.0

    // std::vector<int> indices2;
    // std::vector<double> coeficientes2;

    // for (const auto &kv : X_indices)
    // {
    //     int var_index = kv.second;
    //     indices2.push_back(var_index);
    //     coeficientes2.push_back(1.0);
    // }

    // Aplicar la restricción global con todos los índices y coeficientes de las X
    // glp_set_mat_row(lp, global_row_idx1, indices2.size() - 1, indices2.data(), coeficientes2.data());

    glp_simplex(lp, NULL); // Resolución usando el método simplex

    // Obtener resultados
    if (glp_get_status(lp) == GLP_OPT)
    {
        std::cout << "Solución óptima encontrada." << std::endl;

        // Obtener el valor de la función objetivo
        double obj_value = glp_get_obj_val(lp);
        std::cout << "Valor de la función objetivo: " << obj_value << std::endl;

        // Obtener valores de las variables
        for (int var = 1; var <= X_indices.size(); var++)
        {
            double value = glp_get_col_prim(lp, var);
            if (value > 0.0001)
            {
                std::cout << "Valor de X[" << var << "] = " << value << std::endl;
            }
        }
    }
    else
    {
        std::cout << "No se encontró una solución óptima." << std::endl;
    }

    // Limpieza
    glp_delete_prob(lp);
    glp_free_env();
}

int main()
{
    int capacity = 20;

    vector<vector<int>> matrix_cost = {
        {0, 5, 4, 5, 1, 4},
        {5, 0, 6, 5, 2, 1},
        {4, 6, 0, 1, 3, 5},
        {5, 5, 1, 0, 6, 7},
        {1, 2, 3, 6, 0, 8},
        {4, 1, 5, 7, 8, 0}};
    vector<vector<int>> list = {{2, 3, 1}, {4, 5}};
    optimization(matrix_cost, list, capacity);
}