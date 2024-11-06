#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <glpk.h>
#include "modelo.cpp"

using namespace std;

void optimization(Graph &model, bool binary = true)
{
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
        if (binary)
        {
            glp_set_col_kind(lp, index, GLP_BV); // GLP_BV para variables binarias
        }
        else
        {
            glp_set_col_bnds(lp, index, GLP_DB, 0.0001, 1.0); // 0 <= X <= 1
        }
        glp_set_obj_coef(lp, index, 0.0); // Coeficiente inicial
        cout << "Index: " << index << " Key: (" << get<0>(key) << "," << get<1>(key) << "," << get<2>(key) << "," << get<3>(key) << "," << get<4>(key) << ")" << endl;
    }

    int capacity = model.capacity;
    vector<int> &demands = model.demands;
    map<int, int> &P = model.route_cost;
    map<int, int> &route_demand = model.route_demand;
    map<pair<int, int>, int> &c = model.c;
    map<tuple<int, int, int>, int> &S = model.S;
    map<tuple<int, int, int>, int> &D = model.D;
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
                        cout << "index: " << indice << " coeficent: " << coeficent_objetive << endl;
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

    // Restriccion de capacidad

    for (int r = 0; r < n_r; ++r)
    {
        // Crear una nueva restricción para la capacidad de la ruta `r`
        int row_idx = glp_add_rows(lp, 1);
        glp_set_row_bnds(lp, row_idx, GLP_UP, 0.0, capacity); // La demanda total no debe exceder la capacidad

        vector<int> indicesc;
        vector<double> coeficientesc;

        for (const auto &kv : X_indices)
        {
            int r1 = get<0>(kv.first); // Ruta `r` desde la que se selecciona la subruta
            int j1 = get<1>(kv.first); // Primer cliente de la subruta
            int j2 = get<2>(kv.first); // Último cliente de la subruta

            if (r1 == r)
            {
                int var_index = kv.second;

                // Suma la demanda de los clientes de `j1` a `j2`
                double demanda_total = D[{r1, j1, j2}] + route_demand[r];
                indicesc.push_back(var_index);
                coeficientesc.push_back(demanda_total); // Coeficiente de demanda total de la subruta
            }
        }

        // Aplica la restricción para que la suma de demandas no exceda la capacidad del vehículo
        glp_set_mat_row(lp, row_idx, indicesc.size() - 1, indicesc.data(), coeficientesc.data());
    }

    glp_simplex(lp, NULL); // Resolución usando el método simplex

    if (glp_get_status(lp) == GLP_OPT)
    {
        std::cout << "Solución óptima encontrada." << std::endl;

        double obj_value = glp_get_obj_val(lp);
        std::cout << "Valor de la función objetivo: " << obj_value << std::endl;

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
    vector<int> demands = {0, 3, 1, 4, 5, 2};

    /*

(0,2) (2,3) (3,1) (1,0)   0->2->3->1->5->0 = 4+1+5+1+4= 15
(0,4) (4,5) (5,0)         0->4->5->0 = 1+1 = 2

*/

    Graph model(matrix_cost, list, demands, 10);
    optimization(model);
}