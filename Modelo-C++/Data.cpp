#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <tuple>
#include <fstream>

using namespace std;

class Data
{
public:
    // solution vector<vector<int>> -> vector<int>
    // demand vector<int>
    // capacity vector<int>
    // neighborhood
    // best_neigbor
    // Region

    vector<string> data_to_save;
    vector<string> sol_save;
    vector<string> region_save;

    int count = 0;
    Data() {}
    void AddSolution(const vector<vector<int>> &routes)
    {
        string save = "[ ";
        for (size_t i = 0; i < routes.size(); i++)
        {
            save += "r" + to_string(i) + "= " + to_string(routes[i].size()) + ",";
        }
        save += "]";
        data_to_save.push_back(save);
    }
    void AddCaracteristics(const map<int, int> &caracteristics)
    {
        int i = 0;
        string save = "[ ";
        for (const auto &item : caracteristics)
        {
            save += "r" + to_string(i) + "= " + to_string(item.second) + ",";
            i++;
        }
        save += "]";
        data_to_save.push_back(save);
    }
    void AddBestsNeigh(const tuple<int, int, int, int, int> &neigh)
    {

        string save_region = "";
        string save = "";
        save_region += "\"[ ";
        save += "\"[ ";
        save += "r1 = " + to_string(get<0>(neigh)) + ",";
        save_region += "r1 = " + to_string(get<0>(neigh)) + ",";
        save += "j1 = " + to_string(get<1>(neigh)) + ",";
        save += "j2 = " + to_string(get<2>(neigh)) + ",";
        save_region += "s = " + to_string(get<2>(neigh) - get<1>(neigh)) + ",";
        save += "r2 = " + to_string(get<3>(neigh)) + ",";
        save_region += "r2 = " + to_string(get<3>(neigh)) + ",";
        save += "i = " + to_string(get<4>(neigh)) + ",";
        save += "]\",\n";
        save_region += "]\",\n";
        sol_save.push_back(save);
        region_save.push_back(save_region);
    }
    void AddNeighborhoodCount()
    {
        string save = " ";
        data_to_save.push_back(save);
    }
    void createJson(const string &FileName)
    {
        charge_sol();
        ofstream file(FileName, ios::app);
        string savejson = "";
        savejson = exportJson();
        file << savejson;
        file.close();
    }

private:
    string exportJson()
    {
        string save = "";

        save += "\"data" + to_string(count) + "\":\n";
        save += "{\n";
        save += "\"solution\": \"" + data_to_save[0] + "\",\n";
        save += "\"demand\": \"" + data_to_save[1] + "\",\n";
        save += "\"capacity\": \"" + data_to_save[2] + "\",\n";
        save += "\"neighborhood\": \"" + data_to_save[3] + "\",\n";
        save += "\"best_solutions\": [\n";
        save += data_to_save[4] + "],\n";
        save += "\"regions\": [\n";
        save += data_to_save[5] + "]\n";
        save += "},\n";
        return save;
    }
    void charge_sol()
    {
        string save = "";
        for (const auto &item : sol_save)
        {
            save += item;
        }
        data_to_save.push_back(save);
        save = "";
        for (const auto &item : region_save)
        {
            save += item;
        }
        data_to_save.push_back(save);
    }
};