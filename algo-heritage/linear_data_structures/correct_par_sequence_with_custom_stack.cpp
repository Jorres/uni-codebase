#include <iostream>
#include <string>

typedef char vector_elem;

struct my_vector {
	int size, capacity;
	vector_elem * p;
};

void make_null(my_vector &v) {
	v.size = 0;
	v.capacity = 1;
	v.p = new vector_elem[v.capacity];
}

void copy_previous(my_vector &v) {
	vector_elem* new_mem = new vector_elem[v.capacity];
	for (int i = 0; i < v.size; i++)
		new_mem[i] = v.p[i];
	delete v.p;
	v.p = new_mem;
}

void reallocate_double_memory(my_vector &v) {
	v.capacity *= 2;
	copy_previous(v);
}

void shrink(my_vector &v) {
	v.capacity /= 2;
	copy_previous(v);
}

void push(my_vector &v, vector_elem x) {
	if (v.capacity == v.size)
		reallocate_double_memory(v);
	v.p[v.size++] = x;
}

vector_elem pop(my_vector &v) {
	vector_elem t = v.p[v.size - 1];
	v.size--;

	if (2 * v.size < v.capacity && v.capacity > 1)
		shrink(v);

	return t;
}

vector_elem top(my_vector &v) {
	return v.p[v.size - 1];
}

bool correct(char f, char s) {
	return ((f == '(') && (s == ')')) || ((f == '[') && (s == ']'));
}

int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(0);

	my_vector a;
	make_null(a);

	std::string s;

	while (std::cin >> s) {
		bool is_correct = true;
		for (int i = 0; i < static_cast<int>(s.size()); i++) {
			if (s[i] == '(' || s[i] == '[') {
				push(a, s[i]);
			} else {
				if (a.size == 0) {
					is_correct = false;
					break;
				}

				if (correct(top(a), s[i])) {
					pop(a);
				} else {
					is_correct = false;
					break;
				}
			}
		}

		std::cout << ((is_correct && a.size == 0) ? "YES" : "NO") << std::endl;
		delete a.p;
		make_null(a);
	}
}