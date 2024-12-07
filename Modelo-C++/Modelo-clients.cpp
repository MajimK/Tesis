#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <algorithm>
#include <string>
#include <climits>

using namespace std;

class graph_clients
{
private:
    vector<vector<pair<int, int>>> routes;
    vector<vector<int>> weight;
    vector<int> demands;
    map<int, int> demand_route;
    int capacity = 0;

    void Calculate_clients()
    {
        int route = 0;
        for (const auto &bridges : routes)
        {
            int nodo1 = 0;
            for (const auto &bridge : bridges)
            {
                if (bridge.first == 0)
                {
                    client_weight[bridge.second] = weight[bridge.first][bridge.second];
                    client_original_pos[bridge.second] = route;
                    nodo1 = 0;
                }
                if (bridge.second == 0)
                {
                    client_weight[bridge.first] += weight[bridge.first][bridge.second] - weight[nodo1][bridge.second];
                }

                if (bridge.first != 0 && bridge.second != 0)
                {
                    if (client_weight.count(bridge.first) > 0)
                    {
                        client_weight[bridge.first] += weight[bridge.first][bridge.second] - weight[nodo1][bridge.second];
                    }
                    if (client_weight.count(bridge.second) == 0)
                    {
                        client_weight[bridge.second] = weight[bridge.first][bridge.second];
                        client_original_pos[bridge.second] = route;
                        nodo1 = bridge.first;
                    }
                }
            }
            route++;
        }
    }

    void Calculate_position()
    {
        int pos = 0;
        int route = 0;
        for (const auto &bridges : routes)
        {
            for (const auto &bridge : bridges)
            {
                for (const auto &item : client_weight)
                {
                    int client = get<0>(item);
                    if (client == bridge.first || client == bridge.second)
                    {
                        continue;
                    }
                    int insert = weight[bridge.first][client] + weight[client][bridge.second];
                    position[{client, pos}] = insert;
                }
                pos_route[pos] = route;
                pos++;
            }
            route++;
        }
    }

    void Calculate_best_pos()
    {
        int value_pos0 = INT_MAX;
        int value_pos1 = INT_MAX;
        int value_pos2 = INT_MAX;
        int best_pos0 = 0;
        int best_pos1 = 0;
        int best_pos2 = 0;
        int current_client = 0;
        for (const auto &item : position)
        {
            long penalty = 0;
            if (current_client == 0)
            {
                current_client = get<0>(item).first;
            }
            int client = get<0>(item).first;
            if (client != current_client)
            {
                best_pos[current_client] = {best_pos0, best_pos1, best_pos2};
                value_best_pos[{current_client, best_pos0}] = value_pos0;
                value_best_pos[{current_client, best_pos1}] = value_pos1;
                value_best_pos[{current_client, best_pos2}] = value_pos2;
                current_client = client;
                value_pos0 = INT_MAX;
                value_pos1 = INT_MAX;
                value_pos2 = INT_MAX;
                best_pos0 = 0;
                best_pos1 = 0;
                best_pos2 = 0;
            }
            int pos = get<0>(item).second;
            int insert_cost = get<1>(item);
            int retrieve_cost = client_weight[client];
            int total_cost = insert_cost - retrieve_cost;
            int demand_without_client = demand_route[client_original_pos[client]] - demands[client];
            if (capacity < demand_route[pos_route[pos]] + demands[client])
            {
                penalty += 10000 * (demand_route[pos_route[pos]] + demands[client] - capacity);
            }
            if (pos_route[pos] == client_original_pos[client])
            {
                penalty = 0;
            }
            if (capacity < demand_without_client)
            {
                penalty += 10000 * (demand_route[pos_route[pos]] - demands[client] - capacity);
            }
            total_cost += penalty;
            if (total_cost < value_pos0)
            {
                best_pos2 = best_pos1;
                best_pos1 = best_pos0;
                best_pos0 = pos;
                value_pos2 = value_pos1;
                value_pos1 = value_pos0;
                value_pos0 = total_cost;
            }
            else if (total_cost < value_pos1)
            {
                best_pos2 = best_pos1;
                best_pos1 = pos;
                value_pos2 = value_pos1;
                value_pos1 = total_cost;
            }
            else if (total_cost < value_pos2)
            {
                best_pos2 = pos;
                value_pos2 = total_cost;
            }
        }
        best_pos[current_client] = {best_pos0, best_pos1, best_pos2};
        value_best_pos[{current_client, best_pos0}] = value_pos0;
        value_best_pos[{current_client, best_pos1}] = value_pos1;
        value_best_pos[{current_client, best_pos2}] = value_pos2;
    }

public:
    map<pair<int, int>, int> value_best_pos;
    map<int, int> pos_route;
    map<int, int> client_original_pos;
    map<int, int> client_weight;       // primero es el cliente 2do es la posicion
    map<pair<int, int>, int> position; // La clave es el cliente y el valor un par posicion, el costo de insertarlo ahi
    map<int, tuple<int, int, int>> best_pos;
    graph_clients(const vector<vector<pair<int, int>>> &Bridges, const vector<vector<int>> &matrix_weight, const vector<int> &Demands, const map<int, int> &Demand_route, int Capacity) : weight(matrix_weight), routes(Bridges), demands(Demands), demand_route(Demand_route), capacity(Capacity)
    {
        Calculate_clients();
        Calculate_position();
        Calculate_best_pos();
    }
    void print_clients()
    {
        for (const auto &item : client_weight)
        {
            cout << "Client: " << get<0>(item) << " cost: " << get<1>(item) << endl;
        }
        for (const auto &item : client_original_pos)
        {
            cout << "Client: " << get<0>(item) << " route: " << get<1>(item) << endl;
        }
        for (const auto &item : pos_route)
        {
            cout << "pos: " << get<0>(item) << " route: " << get<1>(item) << endl;
        }
        for (const auto &item : position)
        {
            cout << "Client: " << get<0>(item).first << " position: " << get<0>(item).second << " cost: " << get<1>(item) << endl;
        }
        for (const auto &item : best_pos)
        {
            cout << "Client: " << get<0>(item) << " position1: " << get<0>(get<1>(item)) << " position2: " << get<1>(get<1>(item)) << " position3: " << get<2>(get<1>(item)) << endl;
        }
        for (const auto &item : value_best_pos)
        {
            cout << "Client: " << get<0>(item).first << " position: " << get<0>(item).second << " cost: " << get<1>(item) << endl;
        }
    }
};

int Get_tuple_element(int p, tuple<int, int, int> tuple)
{
    switch (p)
    {
    case 0:
        return std::get<0>(tuple);
    case 1:
        return std::get<1>(tuple);
    case 2:
        return std::get<2>(tuple);
    }
    return -1;
}
