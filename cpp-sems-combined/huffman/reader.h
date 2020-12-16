#ifndef READER_H
#define READER_H

#include "exception.h"
#include <fstream>

struct reader {
	reader(char const* file) {
		fl = std::ifstream(file);
		if (!fl.good()) {
			throw_exception("Failed to open input file.\n");
		}
		buffer = new char[BUFSIZE];
		refresh_buffer();
	}

	void reopen(char const* file) {
		fl.close();
		fl = std::ifstream(file);
		refresh_buffer();
	}

	bool remains() {
		if (pos == actual_symbols) {
			refresh_buffer();	
		}
		if (pos == actual_symbols) {
			return false;
		}
		return true;
	}

	void refresh_buffer() {
		fl.read(buffer, BUFSIZE);
		if (fl.bad()) {
			throw_exception("Failed to read, input stream corrupted. Check whether you read from a file that exists.\n");
		}
		actual_symbols = fl.gcount();
		pos = 0;
		last_byte_pos = 0;
	}

	void read_char(char & c) {
		c = 0;
		for (int32_t i = 0; i < 8; i++) {
			c |= (read_bit() << i);
		}
	}

	void read_int(int32_t & a) {
		a = 0;
		for (int32_t i = 0; i < 32; i++) {
			a |= (read_bit() << i);
		}
	}

	int32_t read_bit() {
		if (pos == actual_symbols) {
			refresh_buffer();
		}
		if (pos == actual_symbols) {
			throw_exception("Failed to read while expecting input. \
				File is probably corrupted.\n");
		}

		int32_t ppos = last_byte_pos;
		int32_t old_pos = pos;

		if (last_byte_pos == 7) {
			pos++;
		}

		last_byte_pos = (last_byte_pos + 1) % 8;
		return (buffer[old_pos] >> ppos) & 1;
	}

	void read_buffer(char* buf, int32_t smb) {
		for (int32_t i = 0; i < smb; i++) {
			char c;
			read_char(c);
			buf[i] = c;
		}
	}

	~reader() {
		delete[] buffer;
	}

	int32_t pos, actual_symbols, last_byte_pos;
	char* buffer;
	std::ifstream fl;
};

struct buffered_reader {
	buffered_reader(char* in_buf, int32_t sz) {
		buffer = in_buf;
		pos = 0;
		last_byte_pos = 0;
		actual_symbols = 0;
	}

	void read_char(char & c) {
		c = 0;
		for (int32_t i = 0; i < 8; i++) {
			c |= (read_bit() << i);
		}
	}

	void read_int(int32_t & a) {
		a = 0;
		for (int32_t i = 0; i < 32; i++) {
			a |= (read_bit() << i);
		}
	}

	int32_t read_bit() {
		int32_t ppos = last_byte_pos;
		int32_t old_pos = pos;

		if (last_byte_pos == 7) {
			pos++;
		}

		last_byte_pos = (last_byte_pos + 1) % 8;
		return (buffer[old_pos] >> ppos) & 1;
	}

	int32_t pos, last_byte_pos, actual_symbols, sz;
	char* buffer;
};

#endif