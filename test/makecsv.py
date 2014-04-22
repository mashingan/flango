import sys

def makefile(entries = 30, filename = 'test.csv'):
    with open(filename, 'w') as f:
        for i in range(entries):
            f.write(';'.join(['dummy'+str(i), '1', '2', '3', 
                str(i), str(i), '\n']))

            
if __name__ == '__main__':
    if len(sys.argv) == 3:
        makefile(sys.argv[1], sys.argv[2])
    else:
        makefile()
