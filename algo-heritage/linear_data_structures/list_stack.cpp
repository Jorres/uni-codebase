#include <iostream>

struct node {
	node* next;
	node* prev;
	int elem;

	node() {

	}

	node(node* p, int x) {
		next = NULL;
		prev = p;
		elem = x;
	}
};

void push(node* &top, int x) {
	node* new_top = new node(top, x);
	top->next = new_top;
	top = new_top;
}

int pop(node* &top) {
	int t = top->elem;
	node* temp = top->prev;

	delete top;

	top = temp;
	return t;
}

void clear(node* &top) {
	while (top->prev != NULL) {
		node* prev = top->prev;
		delete top;
		top = prev;
	}
	delete top;
}

int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(0);

	node* top = new node(NULL, -1);

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
			std::cout << pop(top) << std::endl;
		}
		else {
			int elem;
			std::cin >> elem;
			push(top, elem);
		}
	}

	clear(top);
}