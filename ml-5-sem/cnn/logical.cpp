#include <iostream>
#include <algorithm>
#include <vector>
#include <map>
#include <cstdlib>
#include <ctime>
   
using namespace std;

using calc = double;

void activate(vector<calc>& values) {
    for (auto& value : values) {
        value = value > 0 ? 1 : 0;
    }
}

calc get_init() {
    calc shift = 100 - rand() % 200;
    return shift / 100;
}

struct layer {
    layer(int _layer_size, int prev_sz) : layer_size(_layer_size) {
        ws = vector<vector<calc>>(layer_size, vector<calc>(prev_sz, 0));
        for (int i = 0; i < layer_size; i++) {
            for (int j = 0; j < prev_sz; j++) {
                ws[i][j] = get_init();
            }
        }
        bs = vector<calc>(layer_size, get_init());
    }

    vector<calc> forward(const vector<calc>& pred_litups) {
        vector<calc> new_litups;
        for (int i = 0; i < layer_size; i++) {
            calc cur_litup = 0;
            for (int j = 0; j < static_cast<int>(pred_litups.size()); j++) {
                cur_litup += pred_litups[j] * ws[i][j];
            }
            cur_litup += bs[i];
            new_litups.push_back(cur_litup);
        }
        return new_litups;
    }

    vector<calc> backpropagate(const vector<calc>& ders, 
            const vector<vector<calc>>& forwarded, int layer_num) {
        int prev_layer_size = ws[0].size();
        vector<calc> prev_ders(prev_layer_size, 0);
        for (int i = 0; i < layer_size; i++) {
            // partially calculate prev derivatives:
            for (int j = 0; j < prev_layer_size; j++) {
                prev_ders[j] += ws[i][j] * ders[i];
            }

            // modify weights:
            for (int j = 0; j < ws[i].size(); j++) {
                // why 1? by definition should be 0
                // access to previous layer 
                ws[i][j] -= 1 * forwarded[layer_num][j] * ders[i]; 
            }

            // modify bias:
            bs[i] -= ders[i];
        }
        return prev_ders;
    }

    int layer_size;
    vector<vector<calc>> ws;
    vector<calc> bs;
};

struct t_nn {
    t_nn(int start_size, int end_size) : nn_sz(start_size) {
        layers = vector<layer>();

        int middle_layer_size = 512; // no more than 512
        layers.push_back(layer(middle_layer_size, nn_sz));
        layers.push_back(layer(end_size, middle_layer_size));
    }

    void train(int litups_n, calc req) {
        vector<calc> litups = vector<calc>();
        for (int i = 0; i < nn_sz; i++) {
            litups.push_back(litups_n % 2);
            litups_n /= 2;
        }
        
        vector<vector<calc>> forwarded;
        for (int i = 0; i < static_cast<int>(layers.size()); i++) {
            forwarded.push_back(litups);
            litups = layers[i].forward(litups);
            activate(litups);
        }

        // cout << "der " << 2 * (litups[0] - req) << endl;
        vector<calc> ders(1, 2 * (litups[0] - req));
        for (int i = nn_sz - 1; i >= 0; i--) {
            ders = layers[i].backpropagate(ders, forwarded, i);
        }
    }

    calc predict(int litups_n) {
        vector<calc> litups = vector<calc>();
        for (int i = 0; i < nn_sz; i++) {
            litups.push_back(litups_n % 2);
            litups_n /= 2;
        }
        
        vector<vector<calc>> forwarded;
        for (int i = 0; i < static_cast<int>(layers.size()); i++) {
            forwarded.push_back(litups);
            litups = layers[i].forward(litups);
            activate(litups);
        }
        return litups[0];
    }

    vector<layer> layers;
    int nn_sz;
};

int main() {
    srand(time(NULL));
    ios::sync_with_stdio(false);
    // freopen("input.txt", "r", stdin);

    int m;
    cin >> m;
    vector<pair<int, int>> data;
    for (int i = 0; i < (1 << m); i++) {
        int cur;
        cin >> cur;
        int num = i;
        data.push_back(make_pair(i, cur));
    }

    t_nn nn(m, 1);

    int epochs = 10;
    for (int i = 0; i < epochs; i++) {
        for (auto& sample : data) {
            // cout << sample.first << " " << sample.second << endl;
            nn.train(sample.first, sample.second);
        }
    }
    
    cout << nn.layers.size() << endl;

    for (auto& layer : nn.layers) {
        cout << layer.layer_size << " ";
    }
    cout << endl;

    for (auto& layer : nn.layers) {
        for (int i = 0; i < layer.layer_size; i++) {
            for (auto& w : layer.ws[i]) {
                cout << w << " ";
            }
            cout << layer.bs[i] << endl;
        }
    }
}
