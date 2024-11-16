#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <chrono>
#include <glpk.h>
#include "modelo.cpp"
#include "data.cpp"
#include "generate_problems.cpp"

using namespace std;

void optimization(Graph &model, Data &savedata, bool binary = true, bool verbose = false, long penalty_index = 10000)
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

        glp_set_obj_coef(lp, index, 0.0); // Coeficiente inicial
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
    glp_set_row_bnds(lp, global_row_idx, GLP_LO, 10.0, 0.0);

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
                savedata.AddBestsNeigh({get<0>(index_to_params[var]), get<1>(index_to_params[var]), get<2>(index_to_params[var]), get<3>(index_to_params[var]), get<4>(index_to_params[var])});
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
    int capacity = 20;

    /*

    (0,2) (2,3) (3,1) (1,0)   0->2->3->1->5->0 = 4+1+5+1+4= 15
    (0,4) (4,5) (5,0)         0->4->5->0 = 1+1 = 2

    */
    for (size_t i = 0; i < 3; i++)
    {
        Generate_Problems problem;
        problem.Create_Problem(20, 13);

        Graph model(problem.weight, problem.solution, problem.demand, 20);
        Data save;

        save.AddSolution(problem.solution);
        save.AddCaracteristics(model.route_demand);
        save.AddCaracteristics(model.route_cost);
        save.AddNeighborhoodCount();

        auto inicio = chrono::high_resolution_clock::now();
        optimization(model, save);
        auto fin = chrono::high_resolution_clock::now();
        chrono::duration<double> duracion = fin - inicio;
        cout << "La funcion tomo " << duracion.count() << " seg en ejecutarse." << std::endl;

        save.createJson("DataSol.json", i);
    }
}