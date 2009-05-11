mixChars = [' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', '^', #'t' means triangle
            'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', '#', '!', #'#' means sigma, '!' means pi
            'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
            '.', ',', '(', ')', '+', '-', '*', '/', '=', '$', '<', '>', '@', ';', ':', "'"]

mixChars = mixChars + ['?'] * (256 - len(mixChars))
assert (len(mixChars) == 256)

def ascii2mix_1(c):
    return chr(mixChars.index(c.upper())) #abcd should be treated the same as ABCD

def mix2ascii_1(c):
    return mixChars[ord(c)]

def ascii2mix_n(s):
    return ''.join(map(ascii2mix_1, [x for x in s]))

def mix2ascii_n(s):
    return ''.join(map(mix2ascii_1, [x for x in s]))

if __name__ == '__main__':
    import sys
    if len(sys.argv) == 1 or sys.argv[1:] == ['-a']:
        while True :
            line = sys.stdin.readline().strip('\r\n')
            if not line:
                break
            sys.stdout.write(ascii2mix_n(line))
    elif sys.argv[1:] == ['-m']:
        import os
        import msvcrt
        msvcrt.setmode(0, os.O_BINARY)
        line = os.read(0, 100)
        if line:
            print `mix2ascii_n(line)`


