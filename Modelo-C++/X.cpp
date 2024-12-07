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

void optimization(Graph &model, Data &savedata, bool binary = true, bool verbose = false, double number_of_solution = 1.0, long penalty_index = 10000)
{

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
                        if (j1 >= j2 || (r1 == r2 && i <= j2 && i >= j1))
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
            glp_set_col_bnds(lp, index, GLP_DB, 0.0001, 1.0); // 0 <= X <= 1
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
    // Restricción de Unicidad: Suma de todas las X <= 1
    int global_row_idx = glp_add_rows(lp, 1);
    glp_set_row_bnds(lp, global_row_idx, GLP_LO, number_of_solution, 0.0);

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
}

void client_optimization(Graph &model, graph_clients &model_client, bool binary = true, bool verbose = false)
{
    cout << "entre en optimization" << endl;
    glp_prob *lp = glp_create_prob();
    glp_set_prob_name(lp, "Model-two-clients");
    glp_set_obj_dir(lp, GLP_MIN);

    int number_Clients = model_client.client_weight.size();
    int var_count = 0;

    map<tuple<int, int, int, int>, int> Y_index;
    map<int, tuple<int, int, int, int>> index_to_params;

    for (size_t c1 = 1; c1 < number_Clients + 1; c1++)
    {
        for (size_t c2 = c1 + 1; c2 < number_Clients + 1; c2++)
        {
            for (size_t p1 = 0; p1 < 3; p1++)
            {
                for (size_t p2 = 0; p2 < 3; p2++)
                {
                    var_count++;
                    Y_index[{c1, c2, p1, p2}] = var_count;
                    index_to_params[var_count] = {c1, c2, p1, p2};
                }
            }
        }
    }

    glp_add_cols(lp, var_count);
    for (const auto &kv : Y_index)
    {
        const tuple<int, int, int, int> &key = kv.first;
        int index = kv.second;
        glp_set_col_name(lp, index, ("Y_" + to_string(index)).c_str());

        if (binary)
        {
            glp_set_col_kind(lp, index, GLP_BV); // GLP_BV para variables binarias
        }
        else
        {
            glp_set_col_bnds(lp, index, GLP_DB, 0.0001, 1.0); // 0 <= X <= 1
        }

        glp_set_obj_coef(lp, index, 0.0); // Coeficiente inicial
        if (verbose)
        {
            cout << "Index: " << index << " Key: (" << get<0>(key) << "," << get<1>(key) << "," << get<2>(key) << "," << get<3>(key) << ")" << endl;
        }
    }

    for (size_t c1 = 1; c1 < number_Clients + 1; c1++)
    {
        for (size_t c2 = c1 + 1; c2 < number_Clients + 1; c2++)
        {
            int route_retrieve1 = model_client.client_original_pos[c1];
            int route_retrieve2 = model_client.client_original_pos[c2];
            tuple<int, int, int> best_pos_c1 = model_client.best_pos[c1];
            tuple<int, int, int> best_pos_c2 = model_client.best_pos[c2];

            for (size_t p1 = 0; p1 < 3; p1++)
            {
                for (size_t p2 = 0; p2 < 3; p2++)
                {

                    int route_insert1 = model_client.pos_route[p1];
                    int route_insert2 = model_client.pos_route[p2];

                    long penalty = 0;
                    int pos_c1 = Get_tuple_element(p1, best_pos_c1);
                    int pos_c2 = Get_tuple_element(p2, best_pos_c2);
                    int indice = Y_index[{c1, c2, p1, p2}];

                    int total_cost = model_client.value_best_pos[{c1, pos_c1}] + model_client.value_best_pos[{c2, pos_c2}];
                    double coeficent_objetive = model.total_cost + total_cost;
                    if (pos_c1 == pos_c2)
                    {
                        coeficent_objetive += 100000;
                    }
                    glp_set_obj_coef(lp, indice, coeficent_objetive);
                }
            }
        }
    }

    int global_row_idx = glp_add_rows(lp, 1);
    glp_set_row_bnds(lp, global_row_idx, GLP_FX, 1.0, 1.0); // Igualdad: suma(Y[i]) = 1

    vector<int> indices(1, 0);         // Primer elemento reservado para GLPK (debe ser 0)
    vector<double> coeficientes(1, 0); // Misma razón que índices

    for (const auto &kv : Y_index)
    {
        indices.push_back(kv.second); // Índices de las variables
        coeficientes.push_back(1.0);  // Coeficientes de la restricción (1 * Y[i])
    }

    // Nota: GLPK requiere tamaño-1 para indices y coeficientes
    glp_set_mat_row(lp, global_row_idx, indices.size() - 1, indices.data(), coeficientes.data());

    glp_simplex(lp, NULL);

    if (glp_get_status(lp) == GLP_OPT)
    {
        cout << "Solución óptima encontrada." << endl;

        double obj_value = glp_get_obj_val(lp);
        cout << "Valor de la función objetivo: " << obj_value << endl;

        for (int var = 1; var <= Y_index.size(); var++)
        {
            double value = glp_get_col_prim(lp, var);
            if (value > 0)
            {
                cout << "Valor de Y[" << var << "] = " << value << endl;
                cout << "Var represents: " << " Key: (" << get<0>(index_to_params[var]) << "," << get<1>(index_to_params[var]) << "," << get<2>(index_to_params[var]) << "," << get<3>(index_to_params[var]) << ")" << endl;
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
    auto inicio = chrono::high_resolution_clock::now();
    for (size_t i = 0; i < 1; i++)
    {
        Generate_Problems problem;
        problem.Create_Problem(100, 20, 6);

        Graph model(problem.weight, problem.solution, problem.demand, problem.capacity);
        graph_clients modelo2(model.routes, problem.weight, problem.demand, model.route_demand, model.capacity);
        // model.print_graph();
        // // modelo2.print_clients();
        // client_optimization(model, modelo2);

        Data save;

        // save.AddSolution(problem.solution);
        // save.AddCaracteristics(model.route_demand);
        // save.AddCaracteristics(model.route_cost);
        optimization(model, save);
        // save.createJson("DataSol.json", i);
    }
    auto fin = chrono::high_resolution_clock::now();
    chrono::duration<double> duracion = fin - inicio;
    cout << "Todo tomo " << duracion.count() << " seg en ejecutarse." << std::endl;
}