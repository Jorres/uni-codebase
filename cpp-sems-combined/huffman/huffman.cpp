#include <iostream>
#include <fstream>
#include <string>
#include "lib_huffman.h"
#include "reader.h"
#include "writer.h"
#include "exception.h"

int32_t test();
void do_unpack(char*, char*, int32_t &, int32_t, int32_t, int32_t);
void do_pack(char*, char*, int32_t &, int32_t);

int32_t main(int32_t argc, char ** argv) {
	std::ios::sync_with_stdio(false);

	// test();

	if (argc != 4) {
		return 0;
	}

	std::string option(argv[3]);
	reader fin(argv[1]);
	writer fout(argv[2]);

	if (option == "pack") {
		int32_t available;
		while (fin.actual_symbols > 0) {
			char tmp_buffer[16 * 1024] = {0};
			try {
				do_pack(fin.buffer, tmp_buffer, available, fin.actual_symbols);
			} catch (custom_exception s) {
				std::cout << s.message << std::endl;
				return 0;
			}
			fout.fl.write(tmp_buffer, available); 
			fin.refresh_buffer();
		}
	} else if (option == "unpack") {
		while (true) {
			char tmp_buffer[16 * 1024] = {0};

			int32_t bit_str;
			int32_t should_chars;
			int32_t bits_to_read;
			bool ended = false;
			try {
				fin.read_int(bit_str);
			} catch (custom_exception s) {
				ended = true;
			}
			if (ended) {
				break;
			}

			fin.read_int(should_chars);
			fin.read_int(bits_to_read);

			int32_t bytes_to_read = (bit_str + bits_to_read) / 8;
			if ((bit_str + bits_to_read) % 8 != 0) {
				bytes_to_read++;
			}

			int32_t available;
			fin.read_buffer(tmp_buffer, bytes_to_read);
			char out_buffer[16 * 1024];
			try {
				do_unpack(tmp_buffer, out_buffer, available, bytes_to_read, bit_str, should_chars);
			} catch (custom_exception s) {
				std::cout << s.message << std::endl;
				return 0;
			}
			fout.fl.write(out_buffer, available);
		}
	} else {
		std::cout << "Unknown option. Specify: pack/unpack." << std::endl;
	}
	return 0;
}