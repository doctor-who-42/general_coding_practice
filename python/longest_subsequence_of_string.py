"""
Take from Google's Tech Dev Guide:
Find longest word in dictionary that is a subsequence of a given string 
"""

#basic approach
def longest_substr_in_dict_00(s, d):
    best_match = ""
    max_len = 0

    for k, w in d.items():
        if w in s: #x is a subtring of s
            lw = len(w)
            if lw > max_len: #it's a bigger substring than what we had
                best_match = w
                max_len = lw
    
    return best_match, max_len



#another approach
def longest_substr_in_dict_01(s, d):
    best_match = ""
    max_len = 0

    for k in sorted(d, key=lambda k: len(d[k])):
        w = d[k]
        if w in s: #is a substring
            lw = len(w)
            if lw > max_len:
                max_len = lw
                best_match = w
    
    return best_match, max_len


    



###############################################################################
################################## DRIVER #####################################
###############################################################################
s = "abcdefghi"
d = {}
d["a"] = "ab"
d["smth"] = "defg"
d["5"] = "jkhb"

print(longest_substr_in_dict_00(s,d))
print(longest_substr_in_dict_01(s,d))