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

int pop(node* &front) {
	node* temp = front->next;
	delete front;
	front = temp;
	return front->elem;
}

void clear(node* &front) {
	while (front->next != NULL) {
		node* next = front->next;
		delete front;
		front = next;
	}
	delete front;
}

int main() {
	std::ios::sync_with_stdio(false);
	std::cin.tie(0);

	node* top = new node(NULL, -1);
	node* front = top;
	
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
			std::cout << pop(front) << std::endl;
		}
		else {
			int elem;
			std::cin >> elem;
			push(top, elem);
		}
	}

	clear(front);
}