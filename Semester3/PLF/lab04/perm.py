

# Generate all permutation of N (N - given) respecting the property: for every 2<=i<=n exists an 1<=j<=i,
# so (v[i]-v[j])=1.

def permute(n):
    def cond(x):
        if len(x) < 1:
            return True
        for j in range(0, len(x) - 1):
            if abs(x[-1] - x[j]) == 1:
                return True
        return False

    def permute_rec(k, x):
        if len(x) == n:
            print(x[:])
            return

        for i in range(n):
            if i not in x:
                x.append(i)
                if cond(x):
                    permute_rec(k - 1, x)
                x.pop()

    permute_rec(n, [])
            
permute(3)