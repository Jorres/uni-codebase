#include <iostream>
#include <vector>
#include <cassert>

using namespace std;

const int SIZE = 10;

struct monom {
    bool x_present;
    bool y_present;
    long long fact;
    long long x_pow;
    long long y_pow; 

    monom() {
        this->x_present = false;
        this->y_present = false;
        this->fact = 1;
        this->x_pow = 1;
        this->y_pow = 1;
    }
};

int parse_power_x(const string& input, int pos, monom& m) {
    assert(input[pos] == 'x');
    m.x_present = true;
    pos++;
    if (pos < input.size() && input[pos] == '^') {
        pos++;
        m.x_pow = (long long)(input[pos] - '0');
        pos++;
    } else {
        m.x_pow = 1;
    }
    return pos;
}

int parse_power_y(const string& input, int pos, monom& m) {
    assert(input[pos] == 'y');
    m.y_present = true;
    pos++;
    if (pos < input.size() && input[pos] == '^') {
        pos++;
        m.y_pow = (long long)(input[pos] - '0');
        pos++;
    } else {
        m.y_pow = 1;
    }
    return pos;
}

long long pow(long long what, long long pow) {
    long long ans = 1;
    for (int i = 0; i < pow; i++) {
        ans *= what;
    }
    return ans;
}

struct poly {
    vector<monom> monoms;
    monom chop_monom(const string& input, int& pos) {
        monom m;

        if (input[pos] == '-') {
            m.fact = -1;
            pos++;
        } else if (input[pos] == '+') {
            pos++;
        }

        if (isdigit(input[pos])) {
            m.fact *= (long long)(input[pos] - '0');
            pos++;
        } 

        if (input[pos] == 'x') {
            m.x_present = true;
            pos = parse_power_x(input, pos, m);
        } 

        if (input[pos] == 'y') {
            pos = parse_power_y(input, pos, m);
        }

        return m;
    }

    poly(const string& input) {
        int pos = 0;
        int it = 0;
        while (pos != input.size()) {
            monoms.push_back(chop_monom(input, pos));
        }
    }

    long long evaluate(int x, int y) const {
        long long ans = 0;

        for (auto& m : monoms) {
            long long cur = m.fact;
            if (m.x_present) {
                cur *= pow(x, m.x_pow);
            }
            if (m.y_present) {
                cur *= pow(y, m.y_pow);
            }
            ans += cur;
        }
            
        return ans;
    }
};

bool out(int x, int y) {
    return (x < -SIZE || x > SIZE || y < -SIZE || y > SIZE);
}

int check_local_min(const poly& p, int x, int y) {
    int sat_cells = 0;
    for (int i = x - 1; i <= x + 1; i++) {
        for (int j = y - 1; j <= y + 1; j++) {
            if ((x != i) ^ (y != j)) {
                if (out(i, j) || p.evaluate(x, y) < p.evaluate(i, j))  {
                    sat_cells++;
                }
            }
        }
    }
    return sat_cells == 4 ? 1 : 0;
}

int check_local_max(const poly& p, int x, int y) {
    int sat_cells = 0;
    for (int i = x - 1; i <= x + 1; i++) {
        for (int j = y - 1; j <= y + 1; j++) {
            if ((x != i) ^ (y != j)) {
                if (out(i, j) || p.evaluate(x, y) > p.evaluate(i, j))  {
                    sat_cells++;
                }
            }
        }
    }
    return sat_cells == 4 ? 1 : 0;
}

bool check_plateaus(const poly& p, int x, int y) {
    for (int i = x - 1; i <= x + 1; i++) {
        for (int j = y - 1; j <= y + 1; j++) {
            if ((x != i) ^ (y != j)) {
                if (!out(i, j) && p.evaluate(x, y) == p.evaluate(i, j))  {
                    return true;
                }
            }
        }
    }
    return false;
}

int main() {
    // freopen("input.txt", "r", stdin);
    freopen("unimulti.in", "r", stdin);
    freopen("unimulti.out", "w", stdout);
    string input;
    getline(cin, input);
    poly p(input);

    int max_args = 0;
    int min_args = 0;
    bool plateaus = false;
    for (int i = -SIZE; i <= SIZE; i++) {
        for (int j = -SIZE; j <= SIZE; j++) {
            min_args += check_local_min(p, i, j);
            max_args += check_local_max(p, i, j);
            plateaus = plateaus || check_plateaus(p, i, j);
        }
    }

    cout << "Multiple local maxima: " << (max_args > 1 ? "Yes" : "No") << endl;
    cout << "Multiple local minima: " << (min_args > 1 ? "Yes" : "No") << endl;
    cout << "Plateaus: " << (plateaus ? "Yes" : "No") << endl;
}
