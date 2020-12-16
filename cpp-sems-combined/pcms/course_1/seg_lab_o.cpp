#include <iostream>
#include <algorithm>
#include <climits>

using namespace std;

const int MAXHEIGHT = 19;
const int MAXVERTEXES = 200001;

int br[MAXVERTEXES][MAXHEIGHT] = {0};
int min_on_rise[MAXVERTEXES][MAXHEIGHT];
int height[MAXVERTEXES] = {0};

void uprise(int & v, int to_h) {
    for (int j = MAXHEIGHT - 1; j >= 0 && height[v] > to_h; j--) {
        if (height[br[v][j]] >= to_h) {
            v = br[v][j];
        }
    }
}

int lca(int a, int b) {
	if (height[a] < height[b]) {
		swap(a, b);
	}

	uprise(a, height[b]);

	if (a == b) {
		return b;
	}

	for (int j = MAXHEIGHT - 1; j >= 0; j--) {
		if (br[a][j] != br[b][j]) {
			a = br[a][j];
			b = br[b][j];
    	}
    }

    return br[a][0];
}

int get_min_on_way(int x, int u) {
	int ans = INT_MAX;
	for (int j = MAXHEIGHT - 1; j >= 0 && height[x] > height[u]; j--) {
        if (height[br[x][j]] >= height[u]) {
        	ans = min(ans, min_on_rise[x][j]);
            x = br[x][j];
        }
    }
	return ans;
}

int main() {
	// freopen("input.in", "r", stdin);
	ios::sync_with_stdio(false);
	for (int i = 0; i < MAXVERTEXES; i++) {
		for (int j = 0; j < MAXHEIGHT; j++) {
			min_on_rise[i][j] = INT_MAX;
		}
	}

	int n;
	cin >> n;
	for (int i = 1; i < n; i++) {
		int pr, edge;
		cin >> pr >> edge;
		pr--;
		br[i][0] = pr;
		min_on_rise[i][0] = edge;

		height[i] = height[pr] + 1;

		for (int j = 0; j < MAXHEIGHT - 1; j++) {
			br[i][j + 1] = br[br[i][j]][j];
			min_on_rise[i][j + 1] = min(min_on_rise[i][j], min_on_rise[br[i][j]][j]);
		}
	}

	int m;
	cin >> m;
	for (int i = 0; i < m; i++) {
		int x, y;
		cin >> x >> y;
		x--; y--;
		int v = lca(x, y);
		int ans = get_min_on_way(x, v);
		ans = min(ans, get_min_on_way(y, v));
		cout << ans << "\n";
	}
}
