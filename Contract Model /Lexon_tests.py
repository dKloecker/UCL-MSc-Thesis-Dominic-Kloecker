from LexonTranslator import LexonTranslator
import os
# Set current directory path 
current_dir = os.getcwd()

def lexon_escrow():
    translator = LexonTranslator(filename= current_dir + '/lexon_contracts/lexon_json_00.txt')
    translator._simulate_contract()
    
def lexon_bet():
    translator = LexonTranslator(filename= current_dir + '/lexon_contracts/lexon_json_01.txt')
    translator._simulate_contract()
    
def lexon_ucc():
    translator = LexonTranslator(filename= current_dir + '/lexon_contracts/lexon_json_02.txt')
    translator._simulate_contract()
    
lexon_escrow()