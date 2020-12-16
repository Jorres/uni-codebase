#ifndef WRITER_H
#define WRITER_H

#include "exception.h"
#include <fstream>

struct writer {
	writer(char const* file) {
		fl = std::ofstream(file);
		if (!fl.good()) {	
			throw_exception("Failed to open file fow writing.\n");
		}
		buffer = new char[BUFSIZE];
		am_valid = 0;
		pos = 0;
		last_byte_pos = 0;
		for (int32_t i = 0; i < BUFSIZE; i++) {
			buffer[i] = 0;
		}
	}

	void write(int32_t a) {
		for (int32_t i = 0; i < 32; i++) {
			write_bit((a & (1 << i)) > 0 ? '1' : '0');
		}
	}

	void write(char a) {
		for (int32_t i = 0; i < 8; i++) {
			write_bit((a & (1 << i)) > 0 ? '1' : '0');
		}
	}

	void write(std::string const & s) {
		for (char c : s) {
			write_bit(c);
		}
	}

	void write_bit(char a) {
		if (pos == BUFSIZE) {
			fl.write(buffer, BUFSIZE);
			for (int32_t i = 0; i < BUFSIZE; i++) {
				buffer[i] = 0;
			}
			am_valid = 0;
			last_byte_pos = 0;
			pos = 0;
		}

		if (a == '1') {
			buffer[pos] |= (1 << last_byte_pos);
		}
		
		if (last_byte_pos == 7) {
			pos++;			
		}

		if (last_byte_pos == 0) {
			am_valid++;
		}

		last_byte_pos = (last_byte_pos + 1) % 8;
	}

	~writer() {
		if (am_valid > 0) {
			fl.write(buffer, am_valid);
		}
		delete[] buffer;
	}

	std::ofstream fl;
	char* buffer;
	int32_t am_valid, pos, last_byte_pos;
};

struct buffered_writer {
	buffered_writer(char* buf, int32_t sz) {
		buffer = buf;
		pos = 0;
		last_byte_pos = 0;
		am_valid = 0;
		for (int32_t i = 0; i < sz; i++) {
			buffer[i] = 0;
		}
	}

	void write(int32_t a) {
		for (int32_t i = 0; i < 32; i++) {
			write_bit((a & (1 << i)) > 0 ? '1' : '0');
		}
	}

	void write(std::string const & s) {
		for (char c : s) {
			write_bit(c);
		}
	}

	void write(char a) {
		for (int32_t i = 0; i < 8; i++) {
			write_bit((a & (1 << i)) > 0 ? '1' : '0');
		}
	}

	void write_bit(char a) {
		if (a == '1') {
			buffer[pos] |= (1 << last_byte_pos);
		}
		
		if (last_byte_pos == 7) {
			pos++;			
		}

		if (last_byte_pos == 0) {
			am_valid++;
		}

		last_byte_pos = (last_byte_pos + 1) % 8;
	}

	char* buffer;
	int32_t pos, last_byte_pos, am_valid, sz;
};


#endif