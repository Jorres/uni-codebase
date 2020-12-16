def calculate_f_mera(matrix):
    m_len = len(matrix)
    elems_of = [0] * m_len
    col = [0] * m_len

    tp = [0] * m_len
    tn = [0] * m_len
    fp = [0] * m_len
    fn = [0] * m_len


    for i in range(m_len):
        line = matrix[i]
        elems_of[i] = sum(line)
        for j in range(len(line)):
            col[j] += line[j]
        tp[i] = line[i]

    all_elems = sum(elems_of)

    for i in range(m_len):
        fn[i] = elems_of[i] - tp[i]
        fp[i] = col[i] - tp[i]
        tn[i] = sum(elems_of) - tp[i] - fn[i] - fp[i]

    micro_fs = [0] * m_len

    prec_w = 0
    rec_w = 0
    for i in range(m_len):
        prec = 0
        if tp[i] + fp[i] != 0:
            prec = tp[i] / (tp[i] + fp[i])
        rec = 0
        if tp[i] + fn[i] != 0:
            rec = tp[i] / (tp[i] + fn[i])

        if prec + rec == 0:
            micro_fs[i] = 0
        else:
            part = elems_of[i] / all_elems
            micro_fs[i] = part * (2 * (prec * rec) / (prec + rec))

        prec_w += part * prec
        rec_w += part * rec

    return (2 * (prec_w * rec_w) / (prec_w + rec_w), sum(micro_fs))

N = int(input())
matrix = []
for _ in range(N):
    matrix.append(list(map(int, input().split())))
# print(matrix)
print(calculate_f_mera(matrix))
