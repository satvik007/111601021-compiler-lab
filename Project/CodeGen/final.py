from __future__ import print_function
def LetH0():
	a_string = "satvik choudhary"
	another_string = "thi si a string "
	N = 8
	row = [0] * (N)
	col = [0] * (N)
	diag1 = [0] * (((N + N) - 1))
	diag2 = [0] * (((N + N) - 1))

	def printboard():
		for i in range (0, (N - 1)):
			for j in range (0, (N - 1)):
				if (col[i] == j):
					print(" O", end="")
				else:
					print(" .", end="")

			print("\n", end="")

		print("\n", end="")


	def tryf(c):
		if (c == N):
			printboard()
		else:
			for r in range (0, (N - 1)):
				if (((row[r] == 0) and (diag1[(r + c)] == 0)) and (diag2[((r + 7) - c)] == 0)):
					row[r] = 1
					diag1[(r + c)] = 1
					diag2[((r + 7) - c)] = 1
					col[c] = r
					tryf((c + 1))
					row[r] = 0
					diag1[(r + c)] = 0

					diag2[((r + 7) - c)] = 0

	tryf(0)

	printboard()

LetH0()

