with open('testcsv.csv', 'w') as f:
    for i in range(15):
        f.write(';'.join(['dummy'+str(i), '1', '2', '3', 
            str(i), str(i), '\n']))
