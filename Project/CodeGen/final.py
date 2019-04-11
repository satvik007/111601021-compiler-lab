from __future__ import print_function


def __print(s):
	print(s, end="")


def LetH0():
	N = 8
	row = [0] * (N)
	col = [0] * (N)
	diag1 = [0] * (((N + N) - 1))
	diag2 = [0] * (((N + N) - 1))

	def __printboard():
		for i in range (0, (N - 1) + 1):
			for j in range (0, (N - 1) + 1):
				if (col[i] == j):
					__print(" O")
				else:
					__print(" .")

			__print("\n")

		__print("\n")


	def __atry(c):
		if (c == N):
			__printboard()
		else:
			for r in range (0, (N - 1) + 1):
				if (((row[r] == 0) and (diag1[(r + c)] == 0)) and (diag2[((r + 7) - c)] == 0)):
					row[r] = 1
					diag1[(r + c)] = 1
					diag2[((r + 7) - c)] = 1
					col[c] = r
					__atry((c + 1))
					row[r] = 0
					diag1[(r + c)] = 0

					diag2[((r + 7) - c)] = 0


	__atry(0)

LetH0()

