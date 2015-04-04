
nr_of_tcs = int(input())
tcs = []

for i in range(nr_of_tcs):
    input()  # ignore
    tcs.append(list(map(int, input().split(" "))))


def max_subarray(arr):
    # Initial values
    current_sum = 0  # Sum of current subarray under inspection
    current_index = -1  # Index under consideration for best index
    best_sum = 0  # Highest sum of subarray
    best_start_index = -1  # Start index of subarray that has the highest sum
    best_end_index = -1  # End index of subarray that has the highest sum

    for i in range(len(arr)):
        val = current_sum + arr[i]  # New subarray sum to check
        if val > 0:  # Is atleast positive
            if current_sum == 0:  # Is the initial value
                current_index = i  # Nothing set yet, so this is where to start
            current_sum = val  # the current sum will be under inspection
        else:  # Is not even positive
            current_sum = 0  # Do not consider for highest sum

    if current_sum > best_sum:  # If a new best sum
        best_sum = current_sum
        best_start_index = current_index
        best_end_index = i  # Will be increased on next iterations if appliable

    res = arr[best_start_index:best_end_index + 1]
    if len(res) == 0:
        return [max(arr)]
    return res


def max_non_sequential_subarray(arr):
    is_pos = lambda x: True if x > 0 else False
    res = list(filter(is_pos, arr))
    if len(res) == 0:
        return [max(arr)]
    return res


def solution(tc):
    print(sum(max_subarray(tc)), sum(max_non_sequential_subarray(tc)))

list(map(solution, tcs))
