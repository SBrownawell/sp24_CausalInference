



variable_columns = {"menopaus": [0, 1],
                    "agegrp": [2, 4], 
                    "density": [5, 6],
                    "race": [7, 8],
                    "Hispanic": [9, 10],
                    "bmi": [11, 12],
                    "agefirst": [13, 14],
                    "nrelbc": [15, 16],
                    "brstproc": [17, 18],
                    "lastmamm": [19, 20],
                    "surgmeno": [21, 22],
                    "hrt": [23, 24],
                    "cancer": [27, 28],
                    "count": [31, 37]}


def load_data(path="../data/risk.txt"):
    data = [[key for key in variable_columns if key != "count"]]
    with open(path, 'r') as r:
        max_num = 0
        for line in r:
            row = []

            for key in variable_columns:
                
                i1, i2 = variable_columns[key]

                data_val = int(line[i1: i2])

                if key == 'count':
                    count = data_val
                else:
                    row.append(data_val)

            # adding duplicates
            for i in range(count):
                data.append(row)

    return data

def write_data_to_csv(data, path="data.csv"):
    import csv
    assert all(isinstance(i, str) for i in data[0])

    with open(path, 'w', newline="") as f:
        writer = csv.writer(f)
        writer.writerows(data)



def main():
    data = load_data()
    write_data_to_csv(data)

    '''
    with open("data.csv", 'r') as f:
        for l in f:
            print(l)
    '''
        
    


if __name__ == "__main__":
    main()
