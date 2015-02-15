"""
A linear-complexity algorithm to find the sub-array of an array with the
largest sum relative to its other sub-arrays. Essentially Kadane's algorithm.
"""

import random
import sys

def max_sub_array_sum_slow(arr):
	"""
	Find the sub-array of `arr` with the largest sum in O(n^2) time. Return its
	sum and the sub-array itself in a tuple.
	"""

	sums = []
	max_sum = 0
	for start_ind in range(len(arr)):
		for end_ind in range(start_ind, len(arr)):
			sub_arr = arr[start_ind:end_ind + 1]
			sums.append((sum(sub_arr), sub_arr))

	sums.sort(key=lambda item: item[0])
	return sums[-1]

def max_sub_array_sum(arr):
	"""
	Like `max_sub_array_sum_slow()`, but of O(n) complexity. Returns only the
	sum of the sub-array.
	"""

	prev_sum = arr[0]
	max_sum = prev_sum

	for item in arr[1:]:
		if item < 0 and prev_sum > max_sum:
			max_sum = prev_sum

		if prev_sum < 0:
			prev_sum = item
		else:
			prev_sum += item

	if prev_sum > max_sum:
		max_sum = prev_sum

	return max_sum

def max_sum_sub_array(arr):
	"""
	Like `max_sub_array_sum()`, but returns both the sum and the sub-array.
	"""

	start_ind = 0
	max_start_ind = start_ind
	prev_sum = arr[0]
	max_sum = prev_sum
	end_ind = start_ind

	for ind in range(1, len(arr)):
		if arr[ind] < 0 and prev_sum > max_sum:
			max_sum = prev_sum
			max_start_ind = start_ind
			end_ind = ind

		if prev_sum < 0:
			start_ind = ind
			prev_sum = arr[ind]
		else:
			prev_sum += arr[ind]

	if prev_sum > max_sum:
		max_sum = prev_sum
		max_start_ind = start_ind
		end_ind = ind + 1

	return max_sum, arr[max_start_ind:end_ind]

def test():
	"""
	Test `max_sub_array_sum()` and `max_sum_sub_array()` against
	`max_sub_array_sum_slow()` for a large number of randomly generated arrays.
	"""

	for _ in range(100):
		sys.stdout.write("\rtest #" + str(_))
		arr = [random.randint(-20, 20) for _ in range(200)]
		expected = max_sub_array_sum_slow(arr)
		actual_sum = max_sub_array_sum(arr)
		actual_sub_arr = max_sum_sub_array(arr)

		if actual_sum != expected[0]:
			err_msg = (
				"`max_sub_array_sum()` failed test.\n"
				"Array: {0}\n"
				"Expected sum: {1}\n"
				"Actual sum: {2}"
			).format(arr, expected[0], actual_sum)
			print(err_msg, file=sys.stderr)
			return

		elif actual_sub_arr[0] != expected[0]:
			err_msg = (
				"`max_sum_sub_array()` failed test.\n"
				"Array: {0}\n"
				"Expected sum: {1}\n"
				"Actual sum: {2}"
			).format(arr, expected[0], actual_sub_arr[0])
			print(err_msg, file=sys.stderr)
			return

		elif sum(actual_sub_arr[1]) != expected[0]:
			err_msg = (
				"`max_sum_sub_array()` failed test.\n"
				"Array: {0}\n"
				"Expected sub-array: {1}\n"
				"Actual sub-array: {2}"
			).format(arr, expected[1], actual_sub_arr[1])
			print(err_msg, file=sys.stderr)
			return

if __name__ == "__main__":
	test()
