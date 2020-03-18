"""
Taken from CodeSignals exercise "Are following patterns":
Given an array strings, determine whether it follows the sequence given in the
 patterns array. In other words, there should be no i and j for which 
 strings[i] = strings[j] and patterns[i] ≠ patterns[j] or for which 
 strings[i] ≠ strings[j] and patterns[i] = patterns[j].
"""


def areFollowingPatterns(strings, patterns):
    p_are_same = None
    s_are_same = None
    try:
        for i in range(1, len(strings)):
            prev_char = strings[i-1]
            curr_char = strings[i]
            prev_pattern = patterns[i-1]
            curr_pattern = patterns[i]

            if prev_pattern == curr_pattern:
                p_are_same = True
            else:
                p_are_same = False
            
            if prev_char == curr_char:
                s_are_same = True
            else:
                s_are_same = False

            if not (p_are_same == s_are_same):
                return False
        return True
    except:
        return False


 #################################### Driver ##################################

s = ["cat",  "dog", "dog"]
p = ["a", "b", "b"]
print(areFollowingPatterns(s,p))
