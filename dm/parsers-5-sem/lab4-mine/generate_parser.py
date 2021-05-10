import os
import re
import sys

grmfile = ""

if sys.argv[1] == 'CALC':
    grmfile = "grammar_calc"
elif sys.argv[1] == 'KOTLIN':
    grmfile = "grammar_kotlin"

def get_part(txt, delim):
    ans = []
    started = False
    for line in txt:
        if line.startswith(delim):
            if not started:
                started = True
            else:
                return ans
        elif started:
            ans.append(line)
    return ans

def get_nterms(gram):
    nterms = set()
    for line in gram:
        pos = line.find("->")
        if pos != -1:
            nterms.add(line[:pos].strip())

    nterm_to_text = []
    for nterm in nterms:
        nterm_to_text.append("{" + nterm + ", " + '"' + nterm + '"}')
    return (list(nterms), nterm_to_text)

with open(grmfile, "r") as f:
    txt = f.readlines()

defs = get_part(txt, "&&")
node = get_part(txt, "%%")
gram = get_part(txt, "##")
code = get_part(txt, "==")

def get_node_fields(node): 
    return node

def process_parser_h():
    print("Processing parser.h...")
    with open("parser.h_blueprint", "r") as f:
        raw = f.readlines()

    (nterms, nterm_to_text) = get_nterms(gram)
    raw = ''.join(raw).replace("{place_nterms_here}", ", ".join(nterms))
    tokensFile = "tokens_to_text.txt"
    with open(tokensFile, "r") as f:
        raw = raw.replace("{place_token_to_text_here}", f.read())
        os.system("rm " + tokensFile)

    raw = raw.replace("{place_nterm_to_text_here}", ", ".join(nterm_to_text))

    decls = []
    for nterm in nterms:
        decls.append("node* parse_node_" + nterm + "(const t_node_data& inh);")

    raw = raw.replace("{place_node_fields_here}", ''.join(get_node_fields(node)));
    raw = raw.replace("{place_parse_node_decls_here}", "\n".join(decls))

    with open("code/parser.h", "w") as f:
        f.write(raw)
    print("    parser.h ready!")

def extract_code_from(tmp):
    rule = tmp[:tmp.find("->") + 2].split()
    tmp = tmp[tmp.find("->") + 2:].split()
    tmp.append('fictive')

    code = []
    cur_code = []
    code_mode = 0
    for part in tmp:
        for c in part:
            if c == '{':
                code_mode += 1    

        if code_mode == 0:
            code.append(' '.join(cur_code))
            cur_code = []

        if code_mode > 0:
            cur_code.append(part)
        else:
            rule.append(part)

        for c in part:
            if c == '}':
                code_mode -= 1    

    rule.pop()
    return (rule, code)

def split_gram_to_rules():
    rule = []
    rules = []
    gram.append("->")
    for line in gram:
        if "parse_start" in line:
            continue
        if len(line.strip()) == 0:
            continue
        if len(rule) > 0 and line.find("->") != -1:
            rule = '\n'.join(rule)
            (rule, code) = extract_code_from(rule)
            rules.append((rule, code))
            rule = [line]
        else:
            rule.append(line)
    gram.pop()
    # print(rules)
    return rules

def make_rrule_side(rule):
    ans = "rruleside { ";
    for i in range(2, len(rule)):
        ans += rule[i] + ', '
    ans += '}'
    return ans

def get_start():
    for line in gram:
        parts = line.split()
        if parts[0] == "parse_start":
            print("Starting non-terminal: " + parts[1])
            return parts[1]

    print("Starting non-terminal not found!")
    assert False

def process_parser_cpp():
    print("Processing parser.cpp...")
    with open("parser.cpp_blueprint", "r") as f:
        cpp_b = f.readlines()
    rules_code = split_gram_to_rules()
    cpp_rules = []
    # print(rules_code)
    for (rule, usercode) in rules_code:
        cpp_rule = "rules.push_back(t_rule("
        cpp_rule += rule[0] + ", " + make_rrule_side(rule)
        cpp_rule += '));'
        cpp_rules.append(cpp_rule)

    cpp_b = ''.join(cpp_b).replace("{place_rules_here}", "\n".join(cpp_rules))
    cpp_b = cpp_b.replace("{place_first_parse_function_here}", "return parse_node_" + get_start() + "(st);")
    cpp_b = cpp_b.replace("{place_tests_here}", "tests_" + sys.argv[1] + "();")

    with open("code/parser.cpp", "w") as f:
        f.write(cpp_b);        
    print("    parser.cpp ready!")

def transform_from_bison(code):
    code = code.replace('@@->', 'inh.')
    code = code.replace('$$->', 'cur->node_data.')
    # maybe incorrect regexps somewhere
    code = re.sub(r"\$(\d)->", "sin_next[\g<1>].", code)
    code = re.sub(r"\@(\d)->", "inh_next[\g<1>].", code)
    return code

def generate_parsing_for(fs, rule, user_code):
    (nterms, _) = get_nterms(gram)
    for i in range(2, len(rule)):
        fs.append(transform_from_bison(user_code[i - 2]))
        s = "sin_next[" + str(i - 1) + "] = ";
        if rule[i] in set(nterms):
            s += "safe_append(cur, parse_node_" + rule[i] + "(inh_next[" + str(i - 1) + "]));"
            fs.append(s)
        elif rule[i] != "EPS":
            s += "process_token(cur, " + rule[i] + ");"
            fs.append(s)
    fs.append(transform_from_bison(user_code[len(user_code) - 1]))

def generate_nodes():
    with open("nodes.cpp_with_parse_rules", "r") as f:
        nodes_h = f.read()
    print("Generating recursive parsing functions to nodes.cpp...")
    rules_code = split_gram_to_rules()
    (nterms, _) = get_nterms(gram)
    t_fs = ""
    for nterm in nterms:
        fs = []
        print("Generating " + nterm + "...")
        fs.append("node* t_parser::parse_node_" + nterm + "(const t_node_data& inh) {")
        these_rules = list(filter(lambda rule: rule[0][0] == nterm, rules_code))
        fs.append("node* cur = new node(" + nterm + ");")
        # fs.append('cout << "I am at ' + nterm + '" << endl;')
        fs.append("auto ptoken = lexer.get_token();");
        fs.append("auto token = ptoken.first;");
        fs.append("auto matched = ptoken.second;");
        fs.append("vector<t_node_data> inh_next(100, inh);");
        fs.append("vector<t_node_data> sin_next(100, t_node_data{});");

        for (this_rule, user_code) in these_rules:
            fs.append("if (in(token, g_first(" + nterm + ", " + make_rrule_side(this_rule) + "))) {")
            fs.append("cur->node_data = t_node_data{};");
            assert len(this_rule) == len(user_code) + 1
            generate_parsing_for(fs, this_rule, user_code)
            fs.append("} else");
        fs.append ("{ ambiguous_fail (FIRST.at(" + nterm + "));}")

        fs.append("return cur;")
        fs.append("}\n")
        t_fs += "\n".join(fs)

    nodes_h = nodes_h.replace("{place_functions_here}", "".join(t_fs))
    nodes_h = nodes_h.replace("{place_user_code_here}", "".join(code));

    with open("code/nodes.cpp", "w") as f:
        f.write(nodes_h)

    print("    nodes.cpp ready!")

process_parser_h()
process_parser_cpp()
generate_nodes()
