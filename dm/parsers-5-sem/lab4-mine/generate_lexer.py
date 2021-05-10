import sys

lexfile = ""

if sys.argv[1] == 'CALC':
    lexfile = "lex_calc"
elif sys.argv[1] == 'KOTLIN':
    lexfile = "lex_kotlin"

def escape(s):
    res = ""
    for c in s:
        if c in set([')', '(', '+', '*', '/', '?']):
            res += '\\'
        res += c
    return res

with open("lexer.cpp_blueprint", "r") as f:
    cpp_b = f.read()

with open("lexer.h_blueprint", "r") as f:
    h_b = f.read()

with open(lexfile, "r") as f:
    lines = f.readlines()

regexps = []
tokens = []
tokens_to_text = []
parse_code = []
print("Processing lexrules...")
for line in lines:
    parts = list(filter(lambda part: len(part.strip()) > 0, line.split(' ')))

    if line.startswith('#') or len(parts) <= 1:
        continue

    token = parts[0].strip()
    regexp = parts[1].strip()
    if (parts[1].startswith('\'')):
        regexp = escape(regexp.strip('\''))

    pos = line.find('{')
    if pos != -1:
        cur_code = line[pos:].strip()
        cur_code = cur_code.replace('$matched', 'matched')
        cur_code = cur_code.replace('$$->', 'term->node_data.')
        parse_code.append("if (token == " + parts[0] + ") " + cur_code)

    regexps.append('{regex(R"rgx(^' + 
                    regexp +
                    ')rgx"), ' + 
                    token +
                    '},')
    tokens.append(token + ',')
    tokens_to_text.append("{" + token + ", " + '"' + token + '"},')

with open("tokens_to_text.txt", "w") as f:
    f.write('\n'.join(tokens_to_text))

with open("code/lexer.cpp", "w") as f:
    cpp_full = cpp_b.replace('{place_regexps_here}', '\n'.join(regexps))
    f.write(cpp_full)

with open("code/lexer.h", "w") as f:
    h_full = h_b.replace('{place_tokens_here}', '\n'.join(tokens))
    f.write(h_full)
                                             
print("    lexer.h and lexer.cpp ready!")

with open("nodes.cpp_with_parse_rules", "w") as f1:
    with open("nodes.cpp_blueprint", "r") as f2:
        n_b = f2.read()
        n_full = n_b.replace('{place_parse_actions_here}', '\n'.join(parse_code))
        f1.write(n_full)
print("    nodes.cpp_with_parse_rules ready!")

