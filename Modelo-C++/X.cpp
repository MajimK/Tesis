#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <chrono>
#include <glpk.h>
#include "modelo.cpp"
#include "data.cpp"
#include "generate_problems.cpp"
#include "Modelo-clients.cpp"

using namespace std;

tuple<int, int, int, int, int> optimization(Graph &model, Data &savedata, bool binary = true, bool verbose = false, double number_of_solution = 1.0, long penalty_index = 10000)
{
    int solution = 0;
    cout << "entre en optimization" << endl;
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "Model-X");
    glp_set_obj_dir(lp, GLP_MIN);

    int n_r = model.routes.size();
    int var_count = 0;

    map<tuple<int, int, int, int, int>, int> X_index;
    map<int, tuple<int, int, int, int, int>> index_to_params;
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
                        if (j1 >= j2 || (r1 == r2 && i < j2 && i > j1))
                        {
                            continue;
                        }

                        var_count++;
                        X_index[{r1, j1, j2, r2, i}] = var_count;
                        index_to_params[var_count] = {r1, j1, j2, r2, i};
                    }
                }
            }
        }
    }

    glp_add_cols(lp, var_count);
    for (const auto &kv : X_index)
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
            glp_set_col_bnds(lp, index, GLP_DB, 0.0, 0.7); // 0 <= X <= 1
        }

        glp_set_obj_coef(lp, index, 1.0); // Coeficiente inicial
        if (verbose)
        {
            cout << "Index: " << index << " Key: (" << get<0>(key) << "," << get<1>(key) << "," << get<2>(key) << "," << get<3>(key) << "," << get<4>(key) << ")" << endl;
        }
    }

    int capacity = model.capacity;
    map<int, int> &P = model.route_cost;
    map<int, int> &route_demand = model.route_demand;
    map<pair<int, int>, int> &c = model.c;
    map<tuple<int, int, int>, int> &S = model.S;
    map<tuple<int, int, int>, int> &D = model.D;
    map<tuple<int, int, int, int>, int> &K = model.K;
    map<tuple<int, int, int, int>, int> &L = model.L;
    map<int, double> restric;

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
                        if (j1 >= j2 || (r1 == r2 && i < j2 && i > j1))
                        {
                            continue;
                        }
                        long penalty = 0;
                        double eliminar = P[r1] - S[{r1, j1, j2}] - c[{r1, j1}] - c[{r1, j2}] + L[{r1, j1, r1, j2}];
                        double sumar = P[r2] - c[{r2, i}] + S[{r1, j1, j2}] + K[{r1, j1, r2, i}] + L[{r1, j2, r2, i}];
                        int indice = X_index[{r1, j1, j2, r2, i}];
                        double coeficent_objetive = model.total_cost - P[r1] - P[r2];
                        double demand_insertion = D[{r1, j1, j2}] + route_demand[r2];
                        double demand_retrieve = route_demand[r1] - D[{r1, j1, j2}];

                        if (demand_insertion > capacity)
                        {
                            penalty += (demand_insertion - capacity) * penalty_index;
                        }
                        if (demand_retrieve > capacity)
                        {
                            penalty += (demand_retrieve - capacity) * penalty_index;
                        }
                        coeficent_objetive += (sumar + eliminar + penalty);

                        if ((i == j1 || i == j2) && r1 == r2)
                        {
                            coeficent_objetive = model.total_cost;
                        }
                        restric[indice] = coeficent_objetive;

                        if (verbose)
                        {
                            cout << "index: " << indice << " coeficent: " << coeficent_objetive << endl;
                            // cout << "demand_retrieve: " << demand_retrieve << " demand_insertion: " << demand_insertion << endl;
                        }
                        glp_set_obj_coef(lp, indice, coeficent_objetive);
                    }
                }
            }
        }
    }
    int reduction_row_idx = glp_add_rows(lp, 1); // Fila para la restricción de reducción
    glp_set_row_name(lp, reduction_row_idx, "Reduction_Constraint");
    glp_set_row_bnds(lp, reduction_row_idx, GLP_UP, 0.0, model.total_cost); // Restricción: <= p

    vector<int> reduction_indices(1, 0); // Primer índice reservado para GLPK
    vector<double> reduction_coef(1, 0); // Coeficientes de la restricción

    for (const auto &kv : X_index)
    {
        int var_index = kv.second;

        // Calcula el coeficiente suma + eliminar
        double coeficient = restric[var_index]; // Ya calculado en tu lógica
        reduction_indices.push_back(var_index);
        reduction_coef.push_back(coeficient);
    }

    // Asocia los índices y coeficientes a la fila
    glp_set_mat_row(lp, reduction_row_idx, reduction_indices.size() - 1, reduction_indices.data(), reduction_coef.data());

    // Restricción de Unicidad: Suma de todas las X <= 1
    int global_row_idx = glp_add_rows(lp, 1);
    glp_set_row_bnds(lp, global_row_idx, GLP_LO, 1.0, 0.0);

    vector<int> indices;
    vector<double> coeficientes;

    for (const auto &kv : X_index)
    {
        int var_index = kv.second;
        indices.push_back(var_index);
        coeficientes.push_back(1.0);
    }
    glp_set_mat_row(lp, global_row_idx, indices.size() - 1, indices.data(), coeficientes.data());

    glp_simplex(lp, NULL);

    if (glp_get_status(lp) == GLP_OPT)
    {
        cout << "Solución óptima encontrada." << endl;

        double obj_value = glp_get_obj_val(lp);
        cout << "Valor de la función objetivo: " << obj_value << endl;

        for (int var = 1; var <= X_index.size(); var++)
        {
            double value = glp_get_col_prim(lp, var);
            if (value > 0)
            {
                cout << "Valor de X[" << var << "] = " << value << endl;
                solution = var;
                cout << "Var represents: " << " Key: (" << get<0>(index_to_params[var]) << "," << get<1>(index_to_params[var]) << "," << get<2>(index_to_params[var]) << "," << get<3>(index_to_params[var]) << "," << get<4>(index_to_params[var]) << ")" << endl;
                // savedata.AddBestsNeigh({get<0>(index_to_params[var]), get<1>(index_to_params[var]), get<2>(index_to_params[var]), get<3>(index_to_params[var]), get<4>(index_to_params[var])});
            }
        }
    }
    else
    {
        cout << "No se encontró una solución óptima." << endl;
    }

    glp_delete_prob(lp);
    glp_free_env();
    return index_to_params[solution];
}

void removeEmptyRoutes(vector<vector<int>> &list)
{
    // Elimina rutas vacías del vector de rutas
    list.erase(
        remove_if(list.begin(), list.end(), [](const vector<int> &route)
                  {
                      return route.empty(); // Condición: la ruta está vacía
                  }),
        list.end());
}

vector<vector<int>> transformSolution(vector<vector<int>> list, int r1, int j1, int j2, int r2, int i)
{
    // Extraer la subruta de la ruta r1
    vector<int> subroute(list[r1].begin() + j1, list[r1].begin() + j2);

    // Eliminar la subruta de la ruta r1
    list[r1].erase(list[r1].begin() + j1, list[r1].begin() + j2);

    // Insertar la subruta en la ruta r2 en la posición de la arista i
    if (r1 == r2)
    {
        list[r2].insert(list[r2].begin() + i - 1, subroute.begin(), subroute.end());
    }
    else
    {
        list[r2].insert(list[r2].begin() + i, subroute.begin(), subroute.end());
    }

    removeEmptyRoutes(list);

    return list;
}

int main()
{
    for (size_t i = 0; i < 1; i++)
    {
        Generate_Problems problem;
        problem.Create_Problem(300, 20, 6);
        problem.Create_file_with_data();

        // vector<vector<int>> matrix_cost = {{0, 26, 45, 16, 39, 18, 7, 19, 39, 12, 45, 39, 12, 14, 4, 40, 16, 35, 23, 42, 29, 2, 48, 47, 48, 8, 48, 3, 9, 19, 35, 9, 3, 14, 47, 24},
        //                                    {8, 0, 42, 27, 32, 33, 17, 35, 13, 22, 47, 24, 6, 49, 35, 26, 30, 42, 19, 14, 41, 39, 32, 7, 6, 27, 3, 39, 41, 48, 22, 29, 27, 44, 17, 25},
        //                                    {19, 26, 0, 27, 22, 27, 7, 35, 3, 46, 46, 14, 16, 33, 48, 44, 42, 38, 19, 48, 18, 6, 5, 42, 23, 36, 49, 38, 22, 1, 42, 2, 6, 29, 21, 23},
        //                                    {46, 15, 44, 0, 41, 14, 12, 3, 30, 26, 20, 38, 10, 12, 26, 2, 34, 4, 16, 23, 36, 2, 32, 25, 43, 41, 11, 34, 9, 7, 26, 14, 25, 6, 49, 34},
        //                                    {45, 21, 43, 19, 0, 35, 12, 1, 34, 42, 33, 26, 5, 45, 9, 3, 40, 42, 42, 42, 9, 30, 10, 42, 8, 40, 7, 19, 39, 47, 19, 18, 32, 43, 22, 47},
        //                                    {30, 14, 45, 8, 39, 0, 6, 48, 16, 37, 42, 3, 6, 39, 9, 48, 23, 18, 17, 1, 28, 46, 36, 19, 29, 24, 10, 14, 47, 44, 40, 15, 6, 8, 29, 2},
        //                                    {23, 42, 20, 1, 20, 12, 0, 14, 35, 7, 6, 40, 12, 38, 26, 18, 30, 37, 48, 27, 38, 30, 42, 9, 22, 46, 29, 47, 31, 24, 38, 32, 1, 21, 20, 26},
        //                                    {48, 35, 14, 18, 6, 32, 30, 0, 2, 38, 29, 8, 33, 11, 15, 22, 32, 3, 2, 13, 2, 38, 29, 3, 48, 30, 16, 6, 26, 24, 21, 12, 9, 40, 23, 43},
        //                                    {42, 35, 31, 36, 30, 25, 35, 18, 0, 16, 4, 34, 39, 22, 27, 26, 39, 19, 16, 49, 13, 33, 16, 5, 41, 2, 46, 25, 35, 17, 48, 5, 16, 28, 33, 21},
        //                                    {22, 49, 3, 8, 38, 22, 38, 48, 27, 0, 47, 6, 10, 31, 48, 3, 28, 1, 19, 20, 42, 9, 35, 45, 49, 2, 9, 32, 48, 42, 15, 17, 7, 24, 4, 11},
        //                                    {27, 7, 20, 26, 35, 23, 22, 21, 30, 3, 0, 19, 8, 13, 35, 20, 28, 8, 45, 46, 6, 32, 24, 26, 26, 48, 31, 19, 28, 46, 6, 12, 6, 25, 48, 29},
        //                                    {33, 3, 12, 18, 14, 32, 41, 32, 13, 22, 30, 0, 45, 5, 48, 32, 34, 18, 23, 8, 37, 12, 13, 19, 23, 43, 34, 48, 32, 35, 39, 35, 29, 18, 6, 8},
        //                                    {12, 24, 26, 16, 19, 25, 3, 16, 11, 6, 22, 27, 0, 40, 26, 16, 2, 46, 4, 29, 22, 20, 30, 33, 3, 31, 30, 35, 12, 33, 35, 19, 32, 20, 42, 25},
        //                                    {11, 18, 19, 30, 45, 6, 18, 28, 6, 37, 1, 40, 10, 0, 17, 2, 6, 25, 38, 36, 45, 22, 22, 19, 40, 28, 43, 27, 9, 15, 36, 3, 5, 4, 35, 18},
        //                                    {22, 26, 11, 12, 48, 29, 9, 32, 26, 41, 42, 26, 14, 17, 0, 44, 2, 3, 24, 33, 42, 29, 23, 39, 42, 32, 29, 27, 42, 9, 42, 39, 18, 20, 11, 19},
        //                                    {24, 47, 39, 22, 3, 23, 13, 6, 32, 44, 23, 1, 29, 36, 20, 0, 9, 27, 3, 35, 35, 45, 7, 21, 27, 34, 32, 49, 32, 42, 12, 6, 37, 27, 42, 19},
        //                                    {5, 45, 39, 37, 26, 39, 41, 46, 18, 26, 22, 12, 30, 10, 36, 23, 0, 4, 6, 49, 1, 9, 12, 30, 15, 45, 32, 16, 1, 48, 49, 6, 1, 22, 20, 15},
        //                                    {45, 14, 5, 29, 32, 39, 34, 17, 34, 10, 12, 48, 3, 12, 26, 32, 7, 0, 35, 35, 40, 23, 29, 16, 9, 14, 11, 8, 29, 3, 6, 37, 16, 32, 26, 4},
        //                                    {46, 9, 9, 3, 7, 19, 1, 16, 33, 23, 20, 23, 27, 49, 41, 41, 23, 35, 0, 24, 21, 19, 39, 14, 4, 24, 40, 3, 22, 35, 4, 46, 45, 18, 45, 45},
        //                                    {47, 36, 3, 34, 46, 48, 21, 45, 35, 22, 48, 13, 26, 43, 29, 22, 45, 29, 29, 0, 43, 14, 29, 14, 13, 44, 29, 39, 35, 39, 1, 2, 15, 13, 21, 32},
        //                                    {10, 17, 33, 16, 12, 16, 12, 18, 7, 16, 16, 16, 10, 18, 23, 19, 30, 42, 35, 38, 0, 40, 17, 39, 30, 43, 6, 20, 44, 36, 21, 39, 12, 25, 18, 12},
        //                                    {3, 46, 2, 15, 32, 35, 39, 47, 28, 6, 30, 1, 15, 13, 35, 8, 23, 5, 25, 43, 41, 0, 42, 32, 12, 6, 45, 12, 19, 3, 46, 17, 9, 21, 20, 1},
        //                                    {26, 5, 8, 23, 21, 26, 12, 42, 19, 35, 19, 9, 39, 14, 40, 8, 42, 22, 39, 41, 18, 43, 0, 7, 48, 26, 12, 23, 45, 8, 46, 28, 30, 37, 42, 19},
        //                                    {29, 45, 14, 9, 40, 11, 8, 24, 26, 39, 26, 33, 3, 9, 15, 35, 14, 35, 20, 9, 20, 32, 7, 0, 3, 33, 45, 40, 30, 39, 3, 26, 30, 19, 38, 36},
        //                                    {22, 24, 10, 46, 14, 6, 10, 3, 28, 22, 30, 24, 9, 21, 29, 43, 16, 29, 27, 1, 12, 12, 39, 46, 0, 45, 3, 41, 29, 31, 25, 17, 5, 11, 44, 5},
        //                                    {21, 45, 42, 27, 27, 2, 28, 26, 9, 28, 13, 34, 30, 23, 7, 27, 23, 39, 11, 36, 23, 43, 32, 32, 32, 0, 14, 44, 41, 40, 6, 34, 44, 18, 41, 48},
        //                                    {44, 28, 43, 24, 9, 3, 23, 16, 22, 19, 6, 4, 13, 18, 16, 42, 9, 34, 23, 18, 39, 28, 32, 46, 14, 40, 0, 13, 35, 33, 48, 49, 2, 4, 48, 48},
        //                                    {40, 44, 32, 45, 49, 3, 22, 15, 3, 33, 35, 5, 11, 19, 38, 9, 5, 42, 49, 35, 32, 42, 40, 25, 23, 19, 41, 0, 11, 22, 14, 39, 28, 26, 6, 14},
        //                                    {35, 26, 22, 46, 26, 10, 41, 16, 47, 34, 27, 3, 32, 27, 15, 28, 33, 2, 44, 29, 37, 49, 1, 42, 14, 12, 16, 29, 0, 43, 8, 4, 30, 26, 42, 22},
        //                                    {21, 3, 24, 48, 42, 18, 45, 31, 47, 11, 18, 38, 32, 26, 45, 32, 1, 35, 45, 4, 16, 48, 14, 8, 27, 15, 16, 39, 15, 0, 24, 38, 8, 22, 27, 32},
        //                                    {5, 7, 16, 37, 45, 48, 1, 12, 37, 19, 35, 29, 32, 1, 43, 16, 21, 25, 29, 38, 19, 43, 17, 3, 29, 36, 36, 45, 32, 42, 0, 4, 9, 42, 22, 26},
        //                                    {6, 2, 6, 42, 10, 45, 18, 3, 43, 4, 32, 24, 23, 17, 48, 17, 38, 3, 14, 18, 13, 8, 22, 49, 18, 43, 24, 15, 12, 45, 20, 0, 19, 35, 45, 42},
        //                                    {17, 25, 38, 4, 30, 39, 39, 1, 8, 1, 19, 10, 35, 18, 2, 25, 37, 41, 16, 12, 47, 4, 16, 29, 36, 45, 3, 42, 26, 24, 41, 6, 0, 47, 39, 39},
        //                                    {15, 42, 19, 49, 4, 45, 32, 20, 48, 48, 48, 42, 41, 34, 26, 23, 48, 39, 23, 6, 39, 25, 3, 2, 17, 33, 6, 35, 12, 44, 19, 48, 15, 0, 18, 48},
        //                                    {9, 20, 18, 47, 3, 35, 7, 6, 16, 23, 11, 42, 37, 31, 3, 22, 28, 22, 4, 16, 26, 6, 28, 6, 32, 25, 47, 22, 48, 28, 47, 45, 48, 35, 0, 9},
        //                                    {32, 17, 35, 19, 35, 40, 16, 11, 45, 7, 10, 7, 40, 38, 6, 16, 24, 35, 16, 49, 5, 42, 45, 3, 13, 1, 46, 31, 45, 15, 45, 32, 16, 1, 40, 0}};

        // vector<vector<int>> list = {{1, 2},
        //                             {3, 4, 5},
        //                             {6, 31},
        //                             {7, 8},
        //                             {9},
        //                             {10, 11},
        //                             {12},
        //                             {13, 14},
        //                             {15},
        //                             {16, 17},
        //                             {18},
        //                             {19, 20},
        //                             {21, 22},
        //                             {23},
        //                             {24},
        //                             {25, 26, 27, 28, 29},
        //                             {30, 32, 33, 34},
        //                             {35}};

        // vector<int> demands = {0, 12, 16, 5, 8, 3, 13, 18, 8, 1, 13, 18, 18, 13, 16, 5, 18,
        //                        18, 17, 17, 13, 1, 11, 18, 14, 11, 8, 18, 2, 3, 12, 10, 7,
        //                        16, 12, 3};

        // vector<vector<int>> matrix_cost = {{0, 5, 4, 5, 1, 4},
        //                                    {5, 0, 6, 5, 2, 1},
        //                                    {4, 6, 0, 1, 3, 5},
        //                                    {5, 5, 1, 0, 6, 7},
        //                                    {1, 2, 3, 6, 0, 8},
        //                                    {4, 1, 5, 7, 8, 0}};

        // vector<vector<int>> list = {{2, 3, 1},
        //                             {4, 5}};

        // vector<int> demands = {0, 2, 3, 4, 5, 2};

        // Graph model(matrix_cost, list, demands, 100);
        // graph_clients modelo2(model.routes, matrix_cost, demands, model.route_demand, 20);
        cout << "Calculate time model" << endl;
        Graph model(problem.weight, problem.solution, problem.demand, problem.capacity);

        model.print_graph();

        Data save;

        // save.AddSolution(problem.solution);
        // save.AddCaracteristics(model.route_demand);
        // save.AddCaracteristics(model.route_cost);
        // cout << "Modelo X binario" << endl;
        // cout << endl;
        int p = 0;
        tuple<int, int, int, int, int> solution;
        int r1_s = 0;
        int j1_s = 0;
        int j2_s = 0;
        int r2_s = 0;
        int i_s = 0;

        auto inicio = chrono::high_resolution_clock::now();
        while (true)
        {
            solution = optimization(model, save);

            int r1 = get<0>(solution);
            int j1 = get<1>(solution);
            int j2 = get<2>(solution);
            int r2 = get<3>(solution);
            int i = get<4>(solution);

            if (r1 == r1_s && j1 == j1_s && j2 == j2_s && r2 == r2_s && i == i_s)
            {
                break;
            }
            r1_s = r1;
            j1_s = j1;
            j2_s = j2;
            r2_s = r2;
            i_s = i;

            problem.solution = transformSolution(problem.solution, r1, j1, j2, r2, i);
            model = Graph(problem.weight, problem.solution, problem.demand, problem.capacity);
            // cout << endl;
            // save.createJson("DataSol.json", i);
        }
        auto fin = chrono::high_resolution_clock::now();
        chrono::duration<double> duracion = fin - inicio;
        cout << "Todo tomo " << duracion.count() << " seg en ejecutarse." << std::endl;
    }
}