#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <algorithm>
#include <string>
#include <chrono>

using namespace std;

class Graph
{
public:
    int total_cost = 0;
    int capacity = 0;
    vector<int> demands;
    vector<pair<int, int>> bridges;
    vector<vector<pair<int, int>>> routes;
    vector<vector<int>> weight;
    map<int, int> route_cost;
    map<int, int> route_demand;
    map<pair<int, int>, int> c;
    map<tuple<int, int, int>, int> S, D;
    map<tuple<int, int, int, int>, int> K, L;
    Graph(const vector<vector<int>> &matrix, const vector<vector<int>> &routes, const vector<int> clients_demands, int capac) : weight(matrix), demands(clients_demands), capacity(capac)
    {
        add_route(routes);
        calc_demands(routes);
        calc_constant();
    }

    void print_graph()
    {
        // for (auto &item : route_cost)
        // {
        //     cout << get<0>(item) << " : " << get<1>(item) << endl;
        // }
        int total_cost = 0;
        for (auto &route : route_cost)
        {
            total_cost += route.second;
        }

        cout << "Weight: " << total_cost << endl;
    }
    void apply_criteria(const tuple<int, int, int, int, int> &key)
    {
        int r1 = get<0>(key);
        int j1 = get<1>(key);
        int j2 = get<2>(key);
        int r2 = get<3>(key);
        int i = get<4>(key);

        vector<vector<pair<int, int>>> routes_criteria;
        vector<pair<int, int>> saves;

        for (size_t k = 0; k < routes.size(); k++)
        {
            vector<pair<int, int>> path;
            if (k == r1)
            {
                int j1first = 0;
                int j2second = 0;
                int j1second = 0;
                int j2first = 0;
                for (size_t z = 0; z < routes[r1].size(); z++)
                {
                    if (z == j1)
                    {
                        j1first = routes[r1][z].first;
                        j1second = routes[r1][z].second;
                    }
                    else if (z == j2)
                    {
                        j2first = routes[r1][z].first;
                        j2second = routes[r1][z].second;
                        path.push_back({j1first, j2second});
                        if (j1 == j2 - 1)
                        {
                            saves.push_back({j1second, j2first});
                        }
                    }
                    else if (z > j1 && z < j2)
                    {
                        saves.push_back(routes[r1][z]);
                    }
                    else
                    {
                        path.push_back(routes[r1][z]);
                    }
                }
            }

            else if (k == r2)
            {
                int ifirst = 0;
                int isecond = 0;
                for (size_t z = 0; z < routes[r2].size(); z++)
                {
                    if (z == i)
                    {
                        ifirst = routes[r2][z].first;
                        isecond = routes[r2][z].second;
                        for (size_t h = 0; h < saves.size(); h++)
                        {
                            if (h == 0)
                            {
                                path.push_back({ifirst, saves[h].first});
                            }
                            if (h >= 0 && h <= saves.size() - 1)
                            {
                                path.push_back(saves[h]);
                            }
                            if (h == saves.size() - 1)
                            {
                                path.push_back({saves[h].second, isecond});
                            }
                        }
                    }
                    else
                    {
                        path.push_back(routes[r2][z]);
                    }
                }
            }
            else
            {
                for (size_t z = 0; z < routes[k].size(); z++)
                {
                    path.push_back(routes[k][z]);
                }
            }
            routes_criteria.push_back(path);
        }

        routes = routes_criteria;

        for (const auto &route : routes)
        {
            for (const auto &bridge : route)
            {
                cout << bridge.first << "->" << bridge.second << endl;
            }
        }
        reset_constant();
        calc_constant();
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
    void calc_demands(const vector<vector<int>> &routes)
    {
        for (size_t i = 0; i < routes.size(); i++)
        {
            int client_demands = 0;
            for (size_t j = 0; j < routes[i].size(); j++)
            {
                client_demands += demands[routes[i][j]];
            }
            route_demand[i] = client_demands;
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
        calc_acumulate_s();
        for (size_t route = 0; route < routes.size(); ++route)
        {
            for (size_t i = 0; i < routes[route].size(); ++i)
            {
                for (size_t j = i + 2; j < routes[route].size(); ++j)
                {
                    S[{route, i, j}] = acumulate_S[route][j] - acumulate_S[route][i + 1];
                }
            }
        }

        // Calculate Demanda por subruta
        calc_acumulate_d();
        for (size_t route = 0; route < routes.size(); route++)
        {
            for (size_t j1 = 0; j1 < routes[route].size(); j1++)
            {
                for (size_t j2 = j1 + 1; j2 < routes[route].size(); j2++)
                {
                    D[{route, j1, j2}] = acumulate_D[route][j2] - acumulate_D[route][j1];
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
    void calc_acumulate_s()
    {
        acumulate_S.resize(routes.size());
        for (size_t route = 0; route < routes.size(); ++route)
        {
            acumulate_S[route].resize(routes[route].size()); // +1 para incluir 0 al inicio
            for (size_t i = 2; i < routes[route].size(); ++i)
            {
                pair<int, int> bridge = routes[route][i - 1];

                acumulate_S[route][i] = acumulate_S[route][i - 1] + weight[bridge.first][bridge.second];
            }
        }
    }
    void calc_acumulate_d()
    {
        acumulate_D.resize(routes.size());
        for (size_t route = 0; route < routes.size(); ++route)
        {
            acumulate_D[route].resize(routes[route].size()); // +1 para incluir 0 al inicio
            for (size_t i = 1; i < routes[route].size(); ++i)
            {
                pair<int, int> bridge = routes[route][i - 1];

                acumulate_D[route][i] = acumulate_D[route][i - 1] + demands[routes[route][i].first];
            }
        }
    }
    void reset_constant()
    {
        S.clear();
        D.clear();
        K.clear();
        L.clear();
        c.clear();
    }

    vector<vector<int>> acumulate_D;
    vector<vector<int>> acumulate_S;
};

void printearS(const map<tuple<int, int, int>, int> &matrix)
{
    for (const auto &item : matrix)
        cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << "} -> " << item.second << endl;
}
void printearL(const map<tuple<int, int, int>, int> &matrix)
{
    for (const auto &item : matrix)
        cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << "} -> " << item.second << endl;
}
void printearK(const map<tuple<int, int, int>, int> &matrix)
{
    for (const auto &item : matrix)
        cout << "{" << get<0>(item.first) << ", " << get<1>(item.first) << ", " << get<2>(item.first) << "} -> " << item.second << endl;
}
