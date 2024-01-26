def calculate_sum(n: int) -> float:
    # sum formula: sum = ((-1)^(n+1)) / n , n = 1, 2, 3, ...
    sum = 0.0
    for i in range(1, n+1):
        sum += ((-1)**(i+1)) / i
    return sum

def calculate_sum_p_q(p: int, q: int) -> float:
    sum = 0.0
    n = 1000000
    pos, neg = 1, 2
    cp, cq = p, q
    for i in range(1, n+1):
        if cp > 0:
            sum += 1 / pos
            pos += 2
            cp -= 1
        elif cq > 0:
            sum -= 1 / neg
            neg += 2
            cq -= 1
        else:
            cp, cq = p, q
        
    return sum

def main() -> None:
    n_values = [10, 50, 100, 1000, 10000, 20000, 50000, 100000, 1000000]

    for n in n_values:
        print(f"n = {n} -> sum = {calculate_sum(n)} (~ln2)")

    p = int(input("Number of positive terms on the sum: "))
    q = int(input("Number of negative terms on the sum: "))

    print(f"p = {p}, q = {q} -> sum = {calculate_sum_p_q(p, q)}")


main()