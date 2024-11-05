#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <algorithm>

using namespace std;

class Graph
{
public:
    int total_cost = 0;
    vector<int> clients;
    vector<pair<int, int>> bridges;
    vector<vector<pair<int, int>>> routes;
    vector<vector<int>> weight;
    map<int, int> route_cost;
    map<pair<int, int>, int> c;
    map<tuple<int, int, int>, int> S;
    map<tuple<int, int, int, int>, int> K, L;
    Graph(const vector<vector<int>> &matrix, const vector<vector<int>> &routes) : weight(matrix)
    {
        add_route(routes);
        calc_constant();
    }

    void print_graph()
    {
        for (auto &bridge : bridges)
            cout << bridge.first << " -> " << bridge.second << endl;
        int total_cost = 0;
        cout << "Weight: " << total_cost << endl;
    }

private:
    void add_route(const vector<vector<int>> &routes)
    {
        for (const auto &route : routes)
        {
            int cost = 0, client = 0;
            vector<pair<int, int>> path;
            for (int item : route)
            {
                if (find(clients.begin(), clients.end(), item) == clients.end())
                    clients.push_back(item);
                path.push_back({client, item});
                cost += weight[client][item];
                bridges.push_back({client, item});
                client = item;
            }
            path.push_back({client, 0});
            cost += weight[client][0];
            bridges.push_back({client, 0});
            route_cost[this->routes.size()] = cost;
            this->routes.push_back(path);
        }
    }
    void calc_constant()
    {
        for (auto &cost : route_cost)
        {
            total_cost += cost.second;
        }
        // Calculate C
        for (size_t route = 0; route < routes.size(); ++route)
        {
            for (size_t i = 0; i < routes[route].size(); ++i)
            {
                pair<int, int> bridge = routes[route][i];
                c[{route, i}] = weight[bridge.first][bridge.second];
            }
        }

        // Calculate S
        for (size_t route = 0; route < routes.size(); ++route)
        {
            for (size_t j = 0; j < routes[route].size(); ++j)
            {
                for (size_t i = 0; i < routes[route].size(); ++i)
                {
                    if (j - i <= 1)
                        break;
                    int sum = 0;
                    for (size_t k = 0; k < routes[route].size(); ++k)
                    {
                        if (k >= j)
                            break;
                        else if (k <= i)
                            continue;
                        else
                        {
                            pair<int, int> bridge = routes[route][k];
                            sum += weight[bridge.first][bridge.second];
                        }
                    }
                    S[{route, i, j}] = sum;
                }
            }
        }

        // Calculate K
        for (size_t route1 = 0; route1 < routes.size(); ++route1)
        {
            for (size_t i = 0; i < routes[route1].size(); ++i)
            {
                for (size_t route2 = 0; route2 < routes.size(); ++route2)
                {
                    for (size_t j = 0; j < routes[route2].size(); ++j)
                    {
                        pair<int, int> bridge1 = routes[route1][i];
                        pair<int, int> bridge2 = routes[route2][j];
                        K[{route1, i, route2, j}] = weight[bridge2.first][bridge1.second];
                    }
                }
            }
        }

        // Calculate L
        for (size_t route1 = 0; route1 < routes.size(); ++route1)
        {
            for (size_t i = 0; i < routes[route1].size(); ++i)
            {
                for (size_t route2 = 0; route2 < routes.size(); ++route2)
                {
                    for (size_t j = 0; j < routes[route2].size(); ++j)
                    {
                        pair<int, int> bridge1 = routes[route1][i];
                        pair<int, int> bridge2 = routes[route2][j];
                        L[{route1, i, route2, j}] = weight[bridge1.first][bridge2.second];
                    }
                }
            }
        }
    }
};

void printear(const map<pair<int, int>, int> &matrix)
{
    for (const auto &item : matrix)
        cout << "{" << item.first.first << ", " << item.first.second << "} -> " << item.second << endl;
}

// int main()
// {
//     // Matriz de costos inicial
//     vector<vector<int>> matrix_cost = {
//         {0, 5, 4, 5, 1, 4},
//         {5, 0, 6, 5, 2, 1},
//         {4, 6, 0, 1, 3, 5},
//         {5, 5, 1, 0, 6, 7},
//         {1, 2, 3, 6, 0, 8},
//         {4, 1, 5, 7, 8, 0}};

//     // Lista de rutas inicial
//     vector<vector<int>> list = {
//         {2, 3, 1},
//         {4, 5}};

//     // Crear modelo de grafo
//     Graph model(matrix_cost, list);

//     model.print_graph();
//     for (auto &item : model.route_cost)
//     {
//         cout << "Route " << item.first << " cost: " << item.second << endl;
//     }

//     // cout << "Matrix C" << endl;
//     // printear(model.c);

//     // cout << "Matrix S" << endl;
//     // for (const auto &item : model.S)
//     //     cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << "} -> " << item.second << endl;

//     // cout << "Matrix K" << endl;
//     // for (const auto &item : model.K)
//     //     cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << ", " << get<3>(item.first) << "} -> " << item.second << endl;

//     // cout << "Matrix L" << endl;
//     // for (const auto &item : model.L)
//     //     cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << ", " << get<3>(item.first) << "} -> " << item.second << endl;

//     return 0;
// }