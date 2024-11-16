#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <tuple>
#include <fstream>
#include <random>
#include <algorithm>

using namespace std;

class Generate_Problems
{
private:
public:
    vector<vector<int>> weight;
    vector<vector<int>> solution;
    vector<int> demand;

    Generate_Problems()
    {
    }

    void Create_Problem(int capacity, int max_demand)
    {
        random_device rd;  // Semilla aleatoria
        mt19937 gen(rd()); // Generador de números aleatorios
        uniform_int_distribution<> dis(1, 50);
        uniform_int_distribution<> disdemand(1, max_demand);
        for (size_t i = 0; i < 36; i++)
        {
            vector<int> client_demand;
            for (size_t j = 0; j < 36; j++)
            {
                if (i == j)
                {
                    client_demand.push_back(0);
                }
                else
                {
                    client_demand.push_back(dis(gen));
                }
            }
            weight.push_back(client_demand);
            if (i == 0)
            {
                demand.push_back(0);
            }
            else
            {
                demand.push_back(disdemand(gen));
            }
        }
        vector<int> clients;
        for (size_t i = 1; i < 36; i++)
        {
            clients.push_back(i);
        }

        shuffle(clients.begin(), clients.end(), gen);
        vector<int> present_route;
        int currentdemand = 0;

        for (int client : clients)
        {
            if (currentdemand + demand[client] <= capacity)
            {
                present_route.push_back(client);
                currentdemand += demand[client];
            }
            else
            {
                // Guardar la ruta actual y empezar una nueva
                solution.push_back(present_route);
                present_route.clear();
                present_route.push_back(client);
                currentdemand = demand[client];
            }
        }
    }
};

// Función para imprimir un vector unidimensional
void printVector(const vector<int> &vec)
{
    cout << "[";
    for (size_t i = 0; i < vec.size(); ++i)
    {
        cout << vec[i];
        if (i != vec.size() - 1)
            cout << ", ";
    }
    cout << "]" << endl;
}

// Función para imprimir un vector bidimensional
void printMatrix(const vector<vector<int>> &mat)
{
    cout << "[" << endl;
    for (const auto &row : mat)
    {
        cout << "  ";
        printVector(row);
    }
    cout << "]" << endl;
}
