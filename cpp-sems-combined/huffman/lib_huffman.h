#ifndef LIB_HUFFMAN
#define LIB_HUFFMAN

#include <string>
#include <map>

const int32_t ALPH_SIZE = 256;
const int32_t BUFSIZE = 1024;
const char NEUTRAL = '#';

class Node {
public:
	Node(char smb, int32_t sz) : smb(smb), sz(sz) {
		left = right = nullptr;
	};

	Node* left, * right;
	char smb;
	int32_t sz;
};

class huffman_tree {
public:
	huffman_tree();
	void encrypt_char(char);
	void build_tree();
	std::string const & code_char(char);
	~huffman_tree();

	int32_t* char_data;
	std::map<char, std::string> conversion;
	std::string tree_data, tree_chars;
};

#endif