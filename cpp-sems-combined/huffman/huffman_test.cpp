/*#include <iostream>
#include <string> 
#include <cassert>
#include <fstream>
#include <ctime>

void do_unpack(char*, char*, int32_t &, int32_t);
void do_pack(char*, char*, int32_t &, int32_t);

void write_to_file(std::string const & s, char const* filename) {
	std::ofstream f(filename);
	for (auto c : s) {
		f << c;
	}
}


bool equal(char const* f1_, char const* f2_) {
	std::ifstream f1(f1_), f2(f2_);
	const int bufsize = 4096;
	char b1[bufsize];
	char b2[bufsize];
	f1.read(b1, bufsize);
	f2.read(b2, bufsize);

	if (f1.gcount() != f2.gcount()) {
		return false;
	}
	for (int i = 0; i < f1.gcount(); i++) {
		if (b1[i] != b2[i]) {
			std::cout << b1[i] << " " << b2[i] << std::endl;
			return false;
		}
	}
	return true;
}


bool perform_test(std::string const & s) {
	std::cout << "Testing on \"" << s << "\"\n";

	write_to_file(s, "test.tmp\0");
	write_to_file(s, "test.in\0");
	do_pack("test.in\0", "test.out\0");
	do_unpack("test.out\0", "test.in\0");

	if (equal("test.in\0", "test.tmp\0")) {
		std::cout << "Complete!" << std::endl;
		return true;		
	} else {
		std::cout << "Failed!" << std::endl;
		return false;
	}
}

void get_rand_data(std::string & s, int am) {
	for (int i = 0; i < am; i++) {
		s.push_back(static_cast<char>(rand() % 256));
	}
}

bool perform_test(int am) {
	std::cout << "Testing on " << am << " random chars:\n";
	std::string rand_data;
	get_rand_data(rand_data, am);
	write_to_file(rand_data, "test.tmp\0");
	write_to_file(rand_data, "test.in\0");

	do_pack("test.in\0", "test.out\0");
	do_unpack("test.out\0", "test.in\0");

	if (equal("test.in\0", "test.tmp\0")) {
		std::cout << "Complete!" << std::endl;
		return true;		
	} else {
		std::cout << "Failed!" << std::endl;
		return false;
	}
}

int test() {
	srand(time(NULL));
	assert(perform_test(""));
	assert(perform_test("abracadabra"));
	assert(perform_test("1 2 3 \n \n \n text"));
	for (int i = 0; i < 10; i++) {
		std::cout << i << ": ";	
		assert(perform_test(4000));
	}
	return 0;
}*/