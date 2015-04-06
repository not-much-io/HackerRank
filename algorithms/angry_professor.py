
def get_input():
    nr_of_tcs = int(input())
    tcs = []
    for i in range(nr_of_tcs):
        students, min_present = input().split(" ")  # students ignored
        arrival_times = input().split(" ")
        tcs.append({"min_present": int(min_present),
                    "arrival_times": list(map(int, arrival_times))})
    return tcs


def present_students(tc):
    return len(list(filter(lambda x: x <= 0, tc["arrival_times"])))


def will_class_continue(tc):
    if present_students(tc) < tc["min_present"]:
        return "YES"
    else:
        return "NO"

tcs = get_input()

for tc in tcs:
    print(will_class_continue(tc))
