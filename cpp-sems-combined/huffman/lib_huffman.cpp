#include <set>
#include <iostream>
#include "lib_huffman.h"
#include "reader.h"
#include "writer.h"
#include "exception.h"

void throw_exception(std::string data) {
	custom_exception s(data);
	throw s;
}

huffman_tree::huffman_tree() {
	char_data = new int32_t[ALPH_SIZE];
	for (int32_t i = 0; i < ALPH_SIZE; i++) {
		char_data[i] = 0;
	}
}

void huffman_tree::encrypt_char(char c) {
	char_data[static_cast<uint32_t>(static_cast<unsigned char>(c))]++;
}

void dfs(std::map<char, std::string> & conv, std::string & cur, std::string & tree_data, std::string & tree_chars, Node * v) {
	if (v->left == nullptr && v->right == nullptr) {
		tree_data.push_back('1');
		tree_chars.push_back(v->smb);
		conv.insert(std::make_pair(v->smb, cur));
		return;
	} else {
		tree_data.push_back('0');
	}

	cur.push_back('0');
	dfs(conv, cur, tree_data, tree_chars, v->left);
	cur.pop_back();
	 
	cur.push_back('1');
	dfs(conv, cur, tree_data, tree_chars, v->right);
	cur.pop_back();
}

void clear(Node* p) {
	if (p != nullptr) {
		clear(p->left);
		clear(p->right);
		delete p;
	}
}

void huffman_tree::build_tree() {
	std::set<Node *> nodes;
	for (int32_t i = 0; i < ALPH_SIZE; i++) {
		if (char_data[i] > 0) {
			nodes.insert(new Node(static_cast<char>(i), char_data[i]));
		}
	}

	while (static_cast<int32_t>(nodes.size()) > 1) {
		Node * min1 = nullptr, * min2 = nullptr;
		for (auto p : nodes) {
			if (min1 == nullptr) {
				min1 = p;
			} else if (min2 == nullptr) {
				min2 = p;
			} else {
				if (min1->sz > min2->sz) {
					std::swap(min1, min2);
				}

				if (min1->sz > p->sz) {
					min2 = min1;
					min1 = p;
				} else if (min2->sz > p->sz) {
					min2 = p;
				}
			}
		}

		Node * tmp = new Node(NEUTRAL, min1->sz + min2->sz);
		tmp->left = min1;
		tmp->right = min2;

		nodes.erase(min1);
		nodes.erase(min2);
		nodes.insert(tmp);
	}

	if (nodes.size() == 0) {
		return;
	}

	std::string cur = "";
	dfs(conversion, cur, tree_data, tree_chars, (*nodes.begin()));
	clear(*nodes.begin());
}

std::string const & huffman_tree::code_char(char a) {
	return conversion[a];
}

huffman_tree::~huffman_tree() {
	delete[] char_data;
}

Node* read_node(buffered_reader& fin) {
	int32_t b = fin.read_bit();
	if (b == 0) {
		Node* v = new Node(NEUTRAL, 0);
		v->left = read_node(fin);
		v->right = read_node(fin);
		return v;
	} else {
		char c;
		fin.read_char(c);
		Node* t = new Node(c, 0);
		return t;
	}
}



void do_unpack(char* in_buf, char* out_buf, int32_t & available, int32_t bytes_to_read,
	int32_t tree_length, int32_t file_length) {
	buffered_reader fin(in_buf, 16 * 1024);

	Node* root = nullptr;
	if (tree_length > 0) {
		root = read_node(fin);
	}

	int32_t smb_pos = 0;
	if (root != nullptr && root->left == nullptr && root->right == nullptr) {
		for (int32_t i = 0; i < file_length; i++) {
			out_buf[smb_pos++] = root->smb;
		}
	} else {	
		Node * curp = root;
		while (file_length > 0) {
			int32_t a = fin.read_bit();
			Node * to = (a == 0 ? curp->left : curp->right);
			if (to == nullptr) {
				throw_exception("File corrupted.\n");
			} else {
				curp = to;
			}
			if (curp->left == nullptr && curp->right == nullptr) {
				out_buf[smb_pos++] = curp->smb;
				curp = root;
				file_length--;
			}
		}
	}
	available = smb_pos;
	clear(root);	
}

void do_pack(char* in_buf, char* out_buf, int32_t & available, int32_t to_read) {
	huffman_tree tree;
	for (int32_t i = 0; i < to_read; i++) {
		tree.encrypt_char(in_buf[i]);
	}

	tree.build_tree();
	buffered_writer fout(out_buf, 16 * 1024);
	int32_t tree_size = static_cast<int32_t>(tree.tree_data.size() + 8 * tree.tree_chars.size());	

	fout.write(tree_size);	
	fout.write(to_read);

	int32_t bits_to_read = 0;
	for (int32_t i = 0; i < to_read; i++) {
		bits_to_read += static_cast<int32_t>(tree.code_char(in_buf[i]).size());
	}
	fout.write(bits_to_read);
	
	int32_t ch_p = 0;
	for (auto ch : tree.tree_data) {
		fout.write_bit(ch);
		if (ch == '1') {
			fout.write(tree.tree_chars[ch_p++]);
		}
	}

	for (int32_t i = 0; i < to_read; i++) {
		fout.write(tree.code_char(in_buf[i]));
	}

	available = fout.am_valid;
}
