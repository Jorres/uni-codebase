#include <iostream>

typedef int vector_elem;

const int MIN_SIZE = 2;

struct my_vector {
	int size, capacity;
	int head, tail;
	vector_elem* p;
};

void make_null(my_vector &v) {
	v.capacity = MIN_SIZE;

	v.size = 0;
	v.head = 0;
	v.tail = 0;

	v.p = new vector_elem[v.capacity];
}

void copy_previous(my_vector &v, int prev_cap) {
	vector_elem* new_mem = new vector_elem[v.capacity];
	int pos = 0;
	for (int i = v.head; i < v.head + v.size; i++) {
		new_mem[pos++] = v.p[i % prev_cap];
	}

	v.head = 0;
	v.tail = pos;

	delete v.p;
	v.p = new_mem;
}

void reallocate_double_memory(my_vector &v) {
	v.capacity *= 2;
	copy_previous(v, v.capacity / 2);
}

void shrink(my_vector &v) {
	v.capacity /= 2;
	copy_previous(v, v.capacity * 2);
}

void push(my_vector &v, vector_elem x) {
	if (v.head == v.tail && v.size > 0) {
		reallocate_double_memory(v);
	}

	v.size++;
	v.p[v.tail] = x;
	v.tail = (v.tail + 1) % v.capacity;
}

vector_elem pop_front(my_vector &v) {
	v.size--;
	vector_elem t = v.p[v.head];
	v.head = (v.head + 1) % v.capacity;

	if (2 * v.size < v.capacity && v.capacity > MIN_SIZE) {
		shrink(v);
	}

	return t;
}

int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(0);

	my_vector a;
	make_null(a);

	int n;
	std::cin >> n;

	for (int i = 0; i < n; i++) {
		char type;

		std::cin >> type;

		if (type != '-' && type != '+') {
			i--;
			continue;
		}

		if (type == '-') {
			std::cout << pop_front(a) << std::endl;
		}
		else {
			int elem;
			std::cin >> elem;
			push(a, elem);
		}
	}

	delete a.p;
	return 0;
}