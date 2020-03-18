"""
Taken from CodeSignals "Grouping dishes" exercise:

You are given a list dishes, where each element consists of a list of strings 
beginning with the name of the dish, followed by all the ingredients used in
preparing it. You want to group the dishes by ingredients, so that for each 
ingredient you'll be able to find all the dishes that contain it (if there 
are at least 2 such dishes).

Return an array where each element is a list beginning with the ingredient 
name, followed by the names of all the dishes that contain this ingredient. 
The dishes inside each list should be sorted lexicographically, and the 
result array should be sorted lexicographically by the names of the 
ingredients.
"""


def groupingDishes(dishes):
    list_of_ingredients = []
    added_dish_to_ingredients_list = False

    for i in range(len(dishes)): #list of dishes
        for j in range(1,len(dishes[i])): #current dish
            curr_dish = dishes[i][0]
            curr_ingredient = dishes[i][j]

            for k in range(len(list_of_ingredients)):
                if list_of_ingredients[k][0] == curr_ingredient: #it's already there
                    list_of_ingredients[k].append(curr_dish)
                    added_dish_to_ingredients_list = True
            if not added_dish_to_ingredients_list: #there is no matching ingredient for now
                new_ingredient = [curr_ingredient, curr_dish] #create what shouldbe added
                list_of_ingredients.append(new_ingredient) #and add it

            added_dish_to_ingredients_list = False #preparing next iteration
    
    sorted_res = lex_matrix_sort(list_of_ingredients)
    filtered_res = filter(lambda x: len(x) > 2, sorted_res)
    return list(filtered_res)


def lex_matrix_sort(mat):
    for i in range(len(mat)): #sorting dishes for each ingredient
        curr_ingredient = mat[i][0]
        curr_dishes = mat[i][1:]
        curr_dishes.sort(key=lambda x: x) #sorting dishes in ingredients
        mat[i] = curr_dishes
        mat[i].insert(0, curr_ingredient)
    mat.sort(key=lambda x: x[0]) #sorting ingredients 
    return mat



 #################################### Driver ##################################
dishes = [["Salad", "Tomato", "Cucumber", "Salad", "Sauce"],
            ["Pizza", "Tomato", "Sausage", "Sauce", "Dough"],
            ["Quesadilla", "Chicken", "Cheese", "Sauce"],
            ["Sandwich", "Salad", "Bread", "Tomato", "Cheese"]]
print(groupingDishes(dishes))
