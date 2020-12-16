#define _CRT_SECURE_NO_WARNINGS
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

int is_sign(char c) {
	return (c == '-' || c == '+' || c == '*');
}

int main() {
	node* top = new node(NULL, -1);

	char c;
	while (std::cin.get(c)) {
		if (c >= '0' && c <= '9') {
			push(top, static_cast<int>(c - '0'));
		} else {
			if (is_sign(c)) {
				int last = pop(top), pre_last = pop(top);
				if (c == '-') {
					push(top, pre_last - last);
				}
				else if (c == '+') {
					push(top, pre_last + last);
				}
				else if (c == '*') {
					push(top, pre_last * last);
				}
			}
		}
	}

	std::cout << pop(top) << std::endl;
}