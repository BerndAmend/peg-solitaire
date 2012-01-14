package com.googlecode.pegsolitaire

import java.io.DataOutputStream
import java.util.zip.GZIPOutputStream
import java.io.BufferedOutputStream
import java.io.FileOutputStream

object IO {

	/**
	 * load a solution from file
	 *
	 * Description of the used file
	 * <version: Int>
	 * <type: Int | only in version 1>
	 * <Possible Moves count: Int><Possible Moves: repeated Int> # only available in user mode
	 * <boardDescription length><boardDescription: repeated Char> # only available in user mode
	 * <number of bits set:Int><number of values: Int><values: repeated Long>: repeated until eof
	 *
	 * Version changes
	 *   1: Initial version
	 *   2: All fields are handled as User defined fields, since equal fields are now detected for all games.
	 *      Due to this change it is not possible to correctly load new results with an old program version.
	 */
	def load(filename: String, observer: StatusObserver): Solver = {
		var output: Solver = null

		val in = new java.io.DataInputStream(
							new java.io.BufferedInputStream(
									new ProgressCallbackInputStream(
										new java.io.FileInputStream(filename),
											0.01 , percent => {print((percent*100).toInt + "% loaded from file \"" + filename + "\".\r"); Console.flush}
								)
						)
					)

		try {
			// read version
			val version = in.readInt

			var boardType = Boards.User
			var deleteDuplicateFields = false
			version match {
				case 1 =>
					boardType = Boards(in.readInt)
					if(boardType != Boards.English) {
						println("Old file version. To save memory it is recommended to load and save the database.")
						deleteDuplicateFields = true
					}
				case 2 =>
				case _ =>
			    throw new Exception("unsupported version")
			}

			// read and create game
			boardType match {
				case Boards.English => output = new Solver(Boards.EnglishBoard, observer)
				case Boards.European => output = new Solver(Boards.EuropeanBoard, observer)
				case Boards.Holes15 => output = new Solver(Boards.Holes15Board, observer)
				case Boards.User =>
					val possibleMoves = new Array[MoveDirections.Value](in.readInt)

					for(i <- 0 until possibleMoves.size)
						possibleMoves(i) = MoveDirections(in.readInt)

					// read board Description
					val boardDescription = new Array[Char](in.readInt)
					for(i <- 0 until boardDescription.size)
						boardDescription(i) = in.readChar

					output = new Solver(new Board(new String(boardDescription), possibleMoves), observer)

				case _ => throw new Exception("unsupported boardType")
			}

			var run = true
			while (run) {
				var i = 0
				try {
					i = in.readInt
				} catch {
					case _ => run = false
				}

				if (run) {
					if (i < 0)
						throw new Exception("file corruption detected, number of pegs is lower than 0")
					if (i >= output.solution.length)
						throw new Exception("file corruption detected, number of pegs is highter than the field size")

					val size = in.readInt

					val newLongHashSet = new LongHashSet(size)

					output.solution(i) = newLongHashSet

					val requiredNumberOfBitsSet = output.game.length-i

					for( pos <- 0 until size) {
						val n = in.readLong
						if(java.lang.Long.bitCount(n) == requiredNumberOfBitsSet) {
								if(deleteDuplicateFields) // no performance penalty by doing this inside the loop
									newLongHashSet += output.game.boardHelper.getNormalform(n)
								else
									newLongHashSet += n
						} else {
							throw new Exception("file corruption detected, invalid field")
						}
					}

				}
			}
		} finally {
			in.close
		}

		output
	}

	/**
	 * save solution to a file
	 */
	def save(filename: String, sol: Solver) {
		Time("save time") {
			val out = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(filename)))

			out.writeInt(2) // version

			// move directions
			out.writeInt(sol.game.moveDirections.length)
			for(i <- sol.game.moveDirections)
				out.writeInt(i.id)
			// board description
			out.writeInt(sol.game.boardDescription.length)
			for(i <- sol.game.boardDescription)
				out.writeChar(i)

			try {
				for(i <- 0 until sol.game.length) {
					out.writeInt(i)
					save(out, sol.solution(i))
				}
			} finally {
				out.close
			}
		}
	}

	def save(out: DataOutputStream, hashset: LongHashSet) {
		if(hashset != null && hashset.size > 0) {
			out.writeInt(hashset.size)
			hashset foreach { out.writeLong(_) }
		} else {
			out.writeInt(0)
		}
	}

}