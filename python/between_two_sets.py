"""
Hackerrank's "between two-sets" problem:
You will be given two arrays of integers and asked to determine all integers 
 that satisfy the following two conditions:
- The elements of the first array are all factors of the integer being 
    considered
- The integer being considered is a factor of all elements of the second
     array
"""


def getTotalX(a, b):
    la = len(a)
    lb = len(b)
    count  = 0

    #generating a list containing all the integers in the range that
    #  might satisfay both of these constraints
    candidates = list(range(a[len(a)-1],b[0]+1))

    for c in candidates:
        #generating a lists which only contain elements of the list which 
        # satisfy the factor condition
        tmp_a = list(filter(lambda x : (c % x == 0), a))
        tmp_b = list(filter(lambda x : (x % c == 0), b))

        if len(tmp_a) == la and len(tmp_b) == lb: #satisfies both conditions
            count += 1
    
    return count



#Driver 
"""
a = [2,6]
b =[24,36]
print(getTotalX(a,b))

x = [2,4]
y = [16,32,96]
print(getTotalX(x,y))
"""