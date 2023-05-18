#!/bin/env python3




def create_dictionary(dictionary):
    with open(dictionary) as file:
        string = "(define dictionary (list "
        for word in file:
            string += word.strip() + " "
        string += "))"

        with open("dictionary.rkt", "w") as output:
            output.write(string)

