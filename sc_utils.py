#!/bin/env python3

"""
This is a library of helper functions for preparing the dictionary for use by the
spelling corrector.
This is a little over-engineered but it is designed to be expanded in the future if needed.
"""

"""
This function takes a filename as input and opens it to be read. It then creates a
new file called dictionary.rkt and writes each line from the input file into a
plait define to create a global variable called dictionary which is a list of Strings.
The main function in main.rkt will use this dictionary to load the spelling corrector.
"""
def create_dictionary(dictionary):
    with open(dictionary) as file:
        string = "#lang plait\n\n(define dictionary (list "
        for word in file:
            string += "\"" word.strip() + "\" "
        string += "))"

        with open("dictionary.rkt", "w") as output:
            output.write(string)

