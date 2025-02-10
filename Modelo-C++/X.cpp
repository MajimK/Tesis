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

void optimization_Swap(Graph &model, Data &savedata, bool binary = true, bool verbose = false, double number_of_solution = 1.0, long penalty_index = 10000)
{
    int solution = 0;
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "Model-X");
    glp_set_obj_dir(lp, GLP_MIN);

    int n_r = model.routes.size();
    int var_count = 0;

    map<tuple<int, int, int, int, int, int>, int> X_index;
    map<int, tuple<int, int, int, int, int, int>> index_to_params;
    for (int r1 = 0; r1 < n_r; ++r1)
    {
        for (int j1 = 0; j1 < model.routes[r1].size(); ++j1)
        {
            for (int j2 = 0; j2 < model.routes[r1].size(); ++j2)
            {
                for (int r2 = 0; r2 < n_r; ++r2)
                {
                    for (int i1 = 0; i1 < model.routes[r2].size(); ++i1)
                    {
                        for (int i2 = 0; i2 < model.routes[r2].size(); ++i2)
                        {
                            if (j1 >= j2 || i1 >= i2 || (i1 == j2 && r1 == r2) || (i2 == j1 && r1 == r2) || (i1 == j1 && i2 != j2 && r1 == r2) || (i2 == j2 && i1 != j1 && r1 == r2) || (r1 == r2 && i1 <= j2 && i2 <= j2 && i2 >= j1 && i1 >= j1) || (r1 == r2 && i1 >= j1 && i1 <= j2) || (r1 == r2 && i2 >= j1 && i2 <= j2) || (r1 == r2 && j1 >= i1 && j1 <= i2) || (r1 == r2 && j2 >= i1 && j2 <= i2))
                            {
                                continue;
                            }

                            var_count++;
                            X_index[{r1, j1, j2, r2, i1, i2}] = var_count;
                            index_to_params[var_count] = {r1, j1, j2, r2, i1, i2};
                        }
                    }
                }
            }
        }
    }

    glp_add_cols(lp, var_count);
    for (const auto &kv : X_index)
    {
        const tuple<int, int, int, int, int, int> &key = kv.first;
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
                    for (int i1 = 0; i1 < model.routes[r2].size(); i1++)
                    {
                        for (int i2 = 0; i2 < model.routes[r2].size(); ++i2)
                        {
                            if (j1 >= j2 || i1 >= i2 || (i1 == j2 && r1 == r2) || (i2 == j1 && r1 == r2) || (i1 == j1 && i2 != j2 && r1 == r2) || (i2 == j2 && i1 != j1 && r1 == r2) || (r1 == r2 && i1 <= j2 && i2 <= j2 && i2 >= j1 && i1 >= j1) || (r1 == r2 && i1 >= j1 && i1 <= j2) || (r1 == r2 && i2 >= j1 && i2 <= j2) || (r1 == r2 && j1 >= i1 && j1 <= i2) || (r1 == r2 && j2 >= i1 && j2 <= i2))
                            {
                                continue;
                            }
                            long penalty = 0;
                            double eliminar_j = P[r1] - S[{r1, j1, j2}] - c[{r1, j1}] - c[{r1, j2}] + L[{r1, j1, r1, j2}];
                            double eliminar_i = P[r2] - S[{r2, i1, i2}] - c[{r2, i1}] - c[{r2, i2}] + L[{r2, i1, r2, i2}];
                            double sumar_j = eliminar_i - L[{r2, i1, r2, i2}] + S[{r1, j1, j2}] + K[{r2, i1, r1, j1}] + L[{r1, j2, r2, i2}];
                            double sumar_i = eliminar_j - L[{r1, j1, r1, j2}] + S[{r2, i1, i2}] + K[{r1, j1, r2, i1}] + L[{r2, i2, r1, j2}];
                            int indice = X_index[{r1, j1, j2, r2, i1, i2}];
                            double coeficent_objetive = model.total_cost - P[r1] - P[r2];
                            double demand_retrieve_r1 = route_demand[r1] - D[{r1, j1, j2}];
                            double demand_retrieve_r2 = route_demand[r2] - D[{r2, i1, i2}];

                            double demand_insertion_r2 = D[{r1, j1, j2}] + demand_retrieve_r2;
                            double demand_insertion_r1 = D[{r2, i1, i2}] + demand_retrieve_r1;

                            if (demand_insertion_r1 > capacity)
                            {
                                penalty += (demand_insertion_r1 - capacity) * penalty_index;
                            }
                            if (demand_insertion_r2 > capacity)
                            {
                                penalty += (demand_insertion_r2 - capacity) * penalty_index;
                            }
                            if (demand_retrieve_r1 > capacity)
                            {
                                penalty += (demand_retrieve_r1 - capacity) * penalty_index;
                            }
                            if (demand_retrieve_r2 > capacity)
                            {
                                penalty += (demand_retrieve_r2 - capacity) * penalty_index;
                            }
                            coeficent_objetive += (sumar_i + sumar_j + penalty);

                            if ((i1 == j1 && i2 == j2) && r1 == r2)
                            {
                                coeficent_objetive = model.total_cost;
                            }
                            restric[indice] = coeficent_objetive;

                            glp_set_obj_coef(lp, indice, coeficent_objetive);
                        }
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
                cout << "Var represents: " << " Key: (" << get<0>(index_to_params[var]) << "," << get<1>(index_to_params[var]) << "," << get<2>(index_to_params[var]) << "," << get<3>(index_to_params[var]) << "," << get<4>(index_to_params[var]) << "," << get<5>(index_to_params[var]) << ")" << endl;
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
}
int main()
{
    for (size_t i = 0; i < 30; i++)
    {
        Generate_Problems problem;
        problem.Create_Problem(35, 20, 4);
        problem.Create_file_with_data();

        Graph model(problem.weight, problem.solution, problem.demand, problem.capacity);

        model.print_graph();

        Data save;
        auto inicio = chrono::high_resolution_clock::now();

        optimization_Swap(model, save);
        auto fin = chrono::high_resolution_clock::now();
        chrono::duration<double> duracion = fin - inicio;
        cout << "Todo tomo " << duracion.count() << " seg en ejecutarse." << std::endl;

        // int p = 0;
        // tuple<int, int, int, int, int> solution;
        // int r1_s = 0;
        // int j1_s = 0;
        // int j2_s = 0;
        // int r2_s = 0;
        // int i_s = 0;

        // auto inicio = chrono::high_resolution_clock::now();
        // while (p < 1)
        // {
        //     solution = optimization(model, save, true);

        //     // int r1 = get<0>(solution);
        //     // int j1 = get<1>(solution);
        //     // int j2 = get<2>(solution);
        //     // int r2 = get<3>(solution);
        //     // int s = get<4>(solution);

        //     // if (r1 == r1_s && j1 == j1_s && j2 == j2_s && r2 == r2_s && s == i_s)
        //     // {
        //     //     cout << "se acabo" << endl;
        //     //     break;
        //     // }
        //     // r1_s = r1;
        //     // j1_s = j1;
        //     // j2_s = j2;
        //     // r2_s = r2;
        //     // i_s = s;

        //     // problem.solution = transformSolution(problem.solution, r1, j1, j2, r2, s);
        //     // model = Graph(problem.weight, problem.solution, problem.demand, problem.capacity);
        //     // cout << endl;
        //     // save.createJson("DataSol.json", i);
        //     p++;
        // }
        // auto fin = chrono::high_resolution_clock::now();
        // chrono::duration<double> duracion = fin - inicio;
        // cout << "Todo tomo " << duracion.count() << " seg en ejecutarse." << std::endl;

        std::ofstream archivo("../resultado.txt", ios ::app);

        if (archivo.is_open())
        {
            // Escribir la información en el archivo
            archivo << duracion.count() << std::endl;
            archivo.close(); // Cerrar el archivo
        }
        else
        {
            std::cerr << "No se pudo abrir el archivo." << std::endl;
        }
        cout << i << endl;
    }
}