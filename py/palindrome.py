def IsPalindrome(n):
    if len(n) <= 1:
        return True
    else:
        if n[-1] == n[0]:
            return IsPalindrome(n[1:-1])
        else:
            return False

print IsPalindrome("1234321")
print IsPalindrome("123321")
print IsPalindrome("12332")
