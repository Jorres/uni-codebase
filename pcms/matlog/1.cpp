#include <bits/stdc++.h>
#include "help.h"
#include "help2.h"

int main() {
    std::cin >> std::ws;
    std::string s;
    std::getline(std::cin, s);
    std::map<int, int> code_to_hyp;
    std::map<int, int> hyp_to_code;
    size_t pos = 0;
    size_t next_pos;
    int cnt_hyp = 0;
    while ((next_pos = s.find(',', pos)) != std::string::npos) {
        std::string hyp = s.substr(pos, next_pos - pos);
        int hyp_code = parse(hyp) -> code;
        code_to_hyp[hyp_code] = cnt_hyp;
        hyp_to_code[cnt_hyp] = hyp_code;
        pos = next_pos + 1;
        ++cnt_hyp;
    }
    next_pos = s.find("|-");
    if (next_pos > 0) {
        std::string hyp = s.substr(pos, next_pos - pos);
        int hyp_code = parse(hyp) -> code;
        code_to_hyp[hyp_code] = cnt_hyp;
        hyp_to_code[cnt_hyp] = hyp_code;
        ++cnt_hyp;
    }
    int res_code = parse(s.substr(next_pos + 2)) -> code;

    std::map<int, int> first_pos;
    int last_code;
    int cur_line = 0;
    while (std::getline(std::cin, s)) {
        last_code = parse(s) -> code;
        if (!first_pos.count(last_code)) {
            first_pos[last_code] = cur_line;
        }
        ++cur_line;
    }
    if (last_code != res_code) {
        std::cout << "Proof is incorrect\n";
        return 0;
    }

    std::map<int, proof_info> map_proof;
    for (auto& i : first_pos) {
        map_proof[i.first] = is_ax(i.first);
    }

    for (auto& i : first_pos) {
        proof_info& p = map_proof[i.first];
        if (!p.is_proof) {
            if (code_to_hyp.count(i.first)) {
                p = proof_info(1, code_to_hyp[i.first], 0);
            }
        }
    }

    for (auto& i : first_pos) {
        node* n1 = code_to_node[i.first];
        if (n1 -> type == 0) {
            node_arr* n = dynamic_cast<node_arr*>(n1);
            int le_code = n -> le -> code;
            int r_code = n -> r -> code;
            if (first_pos.count(le_code) && first_pos.count(r_code)) {

                int r_pos = first_pos[r_code];
                int le_pos = first_pos[le_code];
                if (i.second < r_pos && le_pos < r_pos) {
                    proof_info& p = map_proof[r_code];
                    if (!p.is_proof) {
                        p = proof_info(2, i.first, le_code);
                    }
                }
            }
        }
    }

    int min_inc = 1000000;
    for (auto& i : first_pos) {
        if (!map_proof[i.first].is_proof) {
            if (min_inc > i.second + 2) {
                min_inc = i.second + 2;
            }
        }
    }
    if (min_inc != 1000000) {
        std::cout << min_inc << std::endl;
        std::cout << "Proof is incorrect\n";
        return 0;
    }
    
    std::stack<int> st;
    std::set<int> used;
    st.push(res_code);
    used.insert(res_code);
    while (st.size() > 0) {
        int cur_code = st.top();
        st.pop();
        proof_info p = map_proof[cur_code];
        if (p.type == 2) {
            int code1 = p.n1;
            int code2 = p.n2;
            if (used.insert(code1).second) {
                st.push(code1);
            }
            if (used.insert(code2).second) {
                st.push(code2);
            }
        }
    }
    
    std::vector<std::pair<int, int> > pos_code;
    for (int i : used) {
        pos_code.push_back(std::make_pair(first_pos[i], i));
    }
    std::sort(pos_code.begin(), pos_code.end());

    std::map<int, int> code_to_line;
    for (int i = 0; i < pos_code.size(); ++i) {
        code_to_line[pos_code[i].second] = i;
    }

    
    for (int i = 0; i < cnt_hyp; ++i) {
        if (i > 0) {
            std::cout << ", ";
        }
        code_to_node[hyp_to_code[i]] -> print();
    }   
    if (cnt_hyp > 0) {
        std::cout << " ";
    }
    std::cout << "|- ";
    code_to_node[res_code] -> print();
    std::cout << "\n";

    for (int i = 0; i < pos_code.size(); ++i) {
        std::cout << "[" << (i + 1) << ". ";
        int cur_code = pos_code[i].second;
        proof_info p = map_proof[cur_code];
        if (p.type == 0) {
            std::cout << "Ax. sch. " << p.n1;
        } else if (p.type == 1) {
            std::cout << "Hypothesis " << (p.n1 + 1);
        } else {
            std::cout << "M.P. " << (code_to_line[p.n1] + 1) << ", " << (code_to_line[p.n2] + 1);
        }
        std::cout << "] ";
        code_to_node[cur_code] -> print();
        std::cout << "\n";
    }
    return 0;
}
