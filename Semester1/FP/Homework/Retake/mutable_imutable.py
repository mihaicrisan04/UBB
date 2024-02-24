# MUTABLE AND IMMUTABLE OBJECTS IN PYTHON

# Mutable objects in python
# 1. List !
# 2. Dictionary !
# 3. Set
# 4. Bytearray
# 5. User-defined classes (unless specifically made immutable) !


# Immutable objects in python
# 1. int !
# 2. float !
# 3. decimal
# 4. complex
# 5. bool !
# 6. string !
# 7. tuple
# 8. range
# 9. bytes
# 10. frozenset
# 11. bytes
# 12. None


# Mutable objects can be changed after they are created, and immutable objects can't be changed after they are created.


# Example: Mutable objects

# list

def modify_list(some_list):
    some_list.append(4)

# Create a list
my_list = [1, 2, 3]
print(my_list) # [1, 2, 3]
modify_list(my_list)
print(my_list) # [1, 2, 3, 4]


# dictionary

def modify_dict(some_dict):
    some_dict["key"] = "value"

# Create a dictionary
my_dict = {"a": 1, "b": 2}
print(my_dict) # {'a': 1, 'b': 2}
modify_dict(my_dict)
print(my_dict) # {'a': 1, 'b': 2, 'key': 'value'}


# Classes

# Create a class
class MyClass:
    attribute = "value"

def modify_class(some_class):
    some_class.attribute = "new value"


my_class = MyClass()
print(my_class.attribute) # value
modify_class(my_class)
print(my_class.attribute) # new value


# ... 


# Example: Immutable objects

# int

def modify_int(some_int):
    some_int += 10  # new int inside the function = 15

# Create an integer
my_int = 5
print(my_int) # 5
modify_int(my_int)
print(my_int) # 5


# string

def modify_string(some_string):
    some_string += "def"  # new string inside the function = "abcdef"

# Create a string
my_string = "abc"
print(my_string) # abc
modify_string(my_string)
print(my_string) # abc
