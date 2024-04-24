




def main():
    global data
    data = []
    with open("clean_data.csv", "r") as f:
        data.append(f.readlines())
        for l in f:
            data.append([int(val) for val in l.split(",")])



if __name__ == "__main__":
    global data
    main()
