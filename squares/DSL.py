# find if there is one integer constant in the list of constants
def find_consts(consts):
    if consts == []:
        return False
    try:
        if int(consts[0][1:-1]):
            return True
    except:
        return find_consts(consts[1:])
