import math

def fast_pow(base, power, n):
    result = 1
    while power > 0:
        if power % 2 != 0:
            power = power - 1
            result = result * base % n

        power = power // 2
        base = base * base % n
    return result

def check_for_initial(cur_ans, A, B, N):
    return fast_pow(A, cur_ans, N) == B

def solve(a, b, n):
    block = int(math.sqrt(n)) + 1
    print("Шаг великана = округлить_вниз(sqrt(", n, ")) + 1 = ", block)
    vals = dict()
    temp_pow = fast_pow(a, block, n) % n
    cur_val = fast_pow(a, 0, n) % n
    print("насчитаем ряд шагов великана")
    for i in range(block):
        print("step ", i + 1, ", value =  ", cur_val, " * (3 ^ шаг великана) % 197  = ", cur_val * cur_val % n)
        vals[cur_val] = i
        cur_val = cur_val * temp_pow % n
    cur = b % n
    print("теперь подсчитаем шаги малыша")
    print("начнем с 44 % 197 = ", b % n)
    for i in range(block):
        print('проверяем, было ли такое число в шагах великана')
        if cur in vals:
            print('да, было! проверим степень ', vals[cur], ' * ', block, ' - ', i)
            cur_ans = vals[cur] * block - i
            if cur_ans < n and check_for_initial(cur_ans, a, b, n):
                return cur_ans
            else:
                print('не сошлось, пробуем дальше')
        else:
            print('не было, домножаем на 3 и идем дальше')
        cur = cur * a % n
        print(cur)

    return -1

a = 3
b = 44
n = 197
print(a, "^ x = ", b, " mod ", n)
print("Решим с помощью методa giant-step\small-step")

print(solve(a, b, n))
