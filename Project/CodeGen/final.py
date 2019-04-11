def LetH0:
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
				if (col[i] = j):
					print(" O")
				else:
					print(" .")

			print("\n")

		print("\n")


	def try(c):
		if (c = N):
			printboard()
		else:
			for r in range (0, (N - 1)):
				if (((row[r] = 0) && (diag1[(r + c)] = 0)) && (diag2[((r + 7) - c)] = 0)):
					nonlocal row[r]
					row[r] = 1
					nonlocal diag1[(r + c)]
					diag1[(r + c)] = 1
					nonlocal diag2[((r + 7) - c)]
					diag2[((r + 7) - c)] = 1
					nonlocal col[c]
					col[c] = r
					try((c + 1))
					nonlocal row[r]
					row[r] = 0
					nonlocal diag1[(r + c)]
					diag1[(r + c)] = 0

					nonlocal diag2[((r + 7) - c)]
					diag2[((r + 7) - c)] = 0


	try(0)

LetH0()

