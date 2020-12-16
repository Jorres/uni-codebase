#pragma GCC optimize("Ofast")

#include <iostream>
#include <algorithm>

typedef std::pair<int, int> PII;

template <class T>
struct vector {
	int size, capacity;
	T* p;

	vector() {

	}

	vector(int _capacity) {
		size = 0;
		capacity = _capacity;
		p = new T[capacity];
	}

	void copy_previous() {
		T* new_mem = new T[capacity];
		for (int i = 0; i < size; i++)
			new_mem[i] = p[i];
		delete p;
		p = new_mem;
	}

	void reallocate_double_memory() {
		// instead of 2 let's expand each time by the factor of 8 
		// to avoid needless copying
		capacity *= 8;

		/* CUSTOM FIX TO FIT ML */
		capacity = std::min(capacity, 30000000);
		/* END OF CUSTOM FIX */

		this->copy_previous();
	}

	void shrink() {
		capacity /= 2;
		this->copy_previous();
	}

	T top() {
		return p[size - 1];
	}

	T pop() {
		T t = p[size - 1];
		size--;

		if ((2 * size < capacity) && (capacity > 1))
			this->shrink();

		return t;
	}

	void push(T x) {
		if (capacity == size)
			this->reallocate_double_memory();	
		p[size++] = x;		
	}
};

const long long MOD = (static_cast<long long>(1) << 32);

int f(long long val) {
	int ans;
	if (val >= 0 && val <= MOD / 2) { // [0..2^31]
		ans = static_cast<int>(val);
	} else { // if (val > MOD / 2) { // (2^31..2^32)
		ans = static_cast<int>(val - MOD);
	}
	return ans;
}

int get_next(int num, int k, int &prev_f, int &prev_s, long long a, long long b, long long c) {
	int cur;
	if (num < k) {
		std::cin >> cur;
	} else {
		cur = f(((a * prev_f) % MOD + b * prev_s + c) % MOD);
	}

	prev_f = prev_s;
	prev_s = cur;
	return cur;
}

long long get_cur_min(vector<PII> &left, vector<PII> &right) {
	long long left_min  = (left.size  > 0) ? left.top().second  : LLONG_MAX;
	long long right_min = (right.size > 0) ? right.top().second : LLONG_MAX;
	return std::min(left_min, right_min);
}

PII construct_pair(vector<PII> &v, int t) {
	int s;
	if (v.size > 0) {
		s = std::min(t, v.top().second);
	}
	else {
		s = t;
	}
	return PII(t, s);
}

void transfer(vector<PII> &left, vector<PII> &right) {
	while (right.size > 0) {
		PII t = right.pop();
		left.push(construct_pair(left, t.first));
	}
}

int main() {
	std::ios::sync_with_stdio(0);
	std::cin.tie(0);

	vector<PII> left  = vector<PII>(1);
	vector<PII> right = vector<PII>(1);

	int n, m, k;
	long long a, b, c;
	int prev_f = 0, prev_s = 0;
	std::cin >> n >> m >> k >> a >> b >> c;

	for (int i = 0; i < m; i++) {
		right.push(construct_pair(right, get_next(i, k, prev_f, prev_s, a, b, c)));
	}

	long long answer = 0;
	for (int i = m; i < n; i++) {
		answer += get_cur_min(left, right);
		if (left.size == 0)
			transfer(left, right);

		left.pop();
		right.push(construct_pair(right, get_next(i, k, prev_f, prev_s, a, b, c)));
	}

	answer += get_cur_min(left, right);

	delete left.p;
	delete right.p;

	std::cout << answer << std::endl;
}