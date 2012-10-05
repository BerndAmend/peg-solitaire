/**
 * Peg Solitaire
 * Copyright (C) 2010-2012 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License version 3 as
 *  published by the Free Software Foundation.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.googlecode.pegsolitaire

import Helper._

class ConsolenStatusObserver extends StatusObserver {
	def begin_forward_calculation() = println("Begin forward calculation")

	def end_forward_calculation(required_time: Long) = println("Forward calculation took " + Helper.millisecToString(required_time))

	def begin_backward_cleaning() = println("Begin backward cleaning")
	def end_backward_cleaning(required_time: Long) = println("Backward cleaning took " + Helper.millisecToString(required_time))

	def begin_forward_calculation_step(removed_pegs: Int) = printColoredText("search fields with " + removed_pegs + " removed pegs", Color.green)
	def end_forward_calculation_step(removed_pegs: Int, solution: LongHashSet) {
		printColoredText(", found " + solution.size + " fields", Color.green)
		if(Helper.enableDebug)
			printlnlnDebug(" " + solution.depth + " " + solution.bitDistributionString)
		else
			println()
	}

	def begin_backward_cleaning_step(removed_pegs: Int) = printColoredText("clean field list with " + removed_pegs + " removed pegs", Color.green)
	def end_backward_cleaning_step(removed_pegs: Int, deadends: Long) = {
		printColoredText(", found " + deadends + " dead ends\n", Color.green)
		//printDepthDebug(solution(i+next))
	}

	def dead_ends(count: Long) = printlnColoredText("There are " + count + " fields which doesn't result in a 1 peg solution", Color.blue)
}

/**
 * TODO:
 *  rewrite argument parser
 *  add functionality to allow the user to define how many destination pegs should stay on the board
 •  add functionality to allow the user to define a start field
 •  allow the user to search a solution for a given field
 */
object TUI {

	def main(args: Array[String]) {
		println("Peg Solitaire 0.4\n" +
				"  Copyright (C) 2010-2012 Bernd Amend <berndamend+pegsolitaire@googlemail.com>\n" +
		        "  This program is free software: you can redistribute it and/or modify\n" +
		        "  it under the terms of the GNU General Public License version 3 as published by\n" +
		        "  the Free Software Foundation. This program comes with ABSOLUTELY NO WARRANTY\n")

		println("usage [user|english|15holes|euro] [-full] [-count] [additional options]\n" +
				"  Available Boards:\n" +
				"    user: create your own board!\n" +
				"    english: standard english (default)\n" +
				"    15holes: simple test board\n" +
				"    euro: standard european\n\n" +
				"  -full                calculate all solutions for all possible start fields,\n" +
				"                        by default you have to select the startfield\n" +
				"  -count               count the number of ways to a solution (this may take a while)\n" +
				"  -color               enable colored text output\n" +
				"  -thread-count        number of threads that should be used (default 0 = auto)\n" +
				"  -debug               enable debug output\n\n" +
				"  To reduce memory usage try \"-thread-count 1\"")

		val observer = new ConsolenStatusObserver

		/*
		 * parse command line arguments
		 */
		var arg_full = false
		var arg_count = false

		var selectedGame = Boards.English
		var thread_count = 0

		var i=0
		while(i<args.length) {
			/// exit program if argument count is insufficient
			def checkForArguments(name: String, num: Int=1): Int = {
				if(i+num == args.length) {
					printlnError("error: " + name + " requires an additional parameter")
					sys.exit(-1)
				}
				num
			}

			args(i) match {
				case "-full" => arg_full = true
				case "-count" => arg_count = true
				case "-board" =>
				case "-color" => Helper.enableColor = true
				case "-debug" => Helper.enableDebug = true
				case "-thread-count" =>
					i += checkForArguments("-thread-count")
					try {
						thread_count = args(i).toInt
						if(thread_count < 0) {
							printlnError("error: negative arguments for -thread-count are not allowed, exit")
							return
						}
					} catch {
						case _ => printlnError("error: invalid argument for -thread-count, exit")
											return
					}
				case s =>
				  		try {
							selectedGame = Boards.withName(args(i))
						} catch {
							case _ => printlnError("error: unknown parameter " + s + " exit")
							return
						}
			}
			i += 1
		}
		thread_count = if(thread_count==0) Runtime.getRuntime.availableProcessors else thread_count
		
		println("Use " + thread_count + " threads")

		var solitaire: Solver = null
	
		val solitaireType = selectedGame match {
			case Boards.English => Boards.EnglishBoard
			case Boards.European => Boards.EuropeanBoard
			case Boards.Holes15 => Boards.Holes15Board
			case Boards.User =>
				println("Examples:")
				println("15 holes board:\n\no . . . .\no o . . .\no o o . .\no o o o .\no o o o o\n")
				println("Simple board:\n\n. o o o o .\no o o o o o\no o . . o o\no o . . o o\no o o o o o\n. o o o o .\n")
				println("Check board (all move directions have to be allowed):\n\n. . . . . . . o o\n. . . . . . o o o\no o . . . o o o .\no o o . o o o . .\n. o o o o o . . .\n. . o o o . . . .\n. . . o . . . . .\n")
				println("Please create a board or copy one from above (max 63 holes).\nPress enter 2x to process your input (o = hole, . = blocked):\n")
				val sb = new StringBuilder
				var current = ""
				var done = false
				do {
					current = Console.readLine
					done = current.isEmpty
					if(!done) {
						sb append current
						sb append '\n'
					}
				} while(!done)
				var moveDirection = List[MoveDirections.Value]()
				MoveDirections.values.foreach {
					m =>
					if(readYesOrNo("Are " + m + " moves allowed? (y/n)"))
					moveDirection ::= m
				}

				if(moveDirection.isEmpty) {
					printlnError("error: no move directions selected, exit")
					return
				}

				var sol: Board = null
				try {
					sol = new Board(sb.toString, moveDirection.toArray[MoveDirections.Value])
				} catch {
					case _ => printlnError("error: the entered field is invalid, exit")
					return
				}
			println("Press enter to start solving. This may take a while.")
			readLine
			sol
		}
		
		if(Helper.enableDebug) {
			println(solitaireType.debug_output)
		}
		
		val selection: Iterable[Long] = if(arg_full) {
			solitaireType.possibleStartFields.toList
		} else {
			println("Select one or more start fields:")
			selectFields(solitaireType, solitaireType.getCompleteList(solitaireType.possibleStartFields).toList)
		}
		Time("Solve")(solitaire = new Solver(solitaireType, selection, observer, thread_count))

		try {
			solitaire.getStart
		} catch {
			case _ => printlnError("error: there is no solution, sorry")
			return
		}

		println("\nPossible fields:")
		var count = 0
		for (i <- 0 until solitaire.game.length) {
			if(solitaire.solution(i) != null) {
				val num = solitaire.solution(i).size
				println("  - removed pegs = " + i + "  possible fields = " + num)
				count += num
				Unit
			}
		}
		printlnColoredText("There are " + count + " possible fields.", Color.blue)

		if(arg_count) {
			println("\nCount how many ways are available to solve the board (this may take a while)")
			printlnColoredText("There are " + solitaire.countPossibleGames + " ways to a solution.", Color.blue)
		}

		println("Possible solutions:")
		printFields(solitaire.game, solitaire.getEnd.toList)

		solutionBrowser(solitaire)

		println("Bye, bye")
	}

	def printFields(game: Board, choices: List[Long])  {
		val sb = new StringBuilder
		var tmp = ""
		for (i <- 0 until choices.length) {
			tmp = Helper.mixStrings(tmp, game.toString(choices(i)), seperator="     ", str2label=i.toString)

			if((i+1)%4 == 0) {
				sb append tmp + "\n"
				tmp = ""
			}
		}

		if(!tmp.isEmpty)
			sb append tmp + "\n"

		println(sb.toString)
	}

	/**
	 * Simple console based game-field selection
	 *
	 * @return selected game-field
	 */
	def selectField(game: Board, choices: List[Long]): Long = {
		printFields(game, choices)

		var selection = -1
		while (selection < 0 || selection >= choices.length) {
			print("(x to abort) > ")
			Console.flush
			val input = readLine()
			if(input.toLowerCase=="x") {
				println("Bye, bye")
				sys.exit(0)
			}
			try {
				selection = input.toInt
				if (selection < 0 || selection >= choices.length)
					printlnError("error: invalid selection, please try again")
			} catch {
				case _ =>
					printlnError("error: invalid input, please try again")
					selection = -1
			}
		}

		choices(selection)
	}

	/**
	 * Simple console based game-field selection
	 *
	 * @return selected game-fields
	 */
	def selectFields(game: Board, choices: List[Long]): List[Long] = {
		printFields(game, choices)

		var selected = List[Long]()

		do {
			print("select multiple fields seperated by spaces (x to abort) > ")
			val input = readLine()

			val splitted = input.split(' ')

			for(e <- splitted) {
				if(e.toLowerCase == "x") {
					println("Bye, bye")
					sys.exit(0)
				}
				try {
					val num = e.toInt
					if (num < 0 || num >= choices.length)
						printlnError("ignore invalid selection: " + num)
					else
						selected ::= choices(num)
					Unit
				} catch {
					case _ => printlnError("ignore invalid selection: " + input)
				}
			}

			if(selected.isEmpty)
				println("Nothing selected, try again")
		} while(selected.isEmpty)
		selected
	}

	def solutionBrowser(solitaire: Solver) {
		while (true) {
			println("\nSolution Browser: (x = peg, . = empty)")
			var s = solitaire.getStart.toList
			var f = s(0)
			while (s.length != 0) {
				println("Please choose a move: ")
				f = selectField(solitaire.game, s)
				println()
				println("Current field " + (solitaire.game.length - java.lang.Long.bitCount(f)) + "")
				println(solitaire.game.toString(f))
				println()
				s = solitaire.getCompleteList(solitaire.getFollower(f)).toList
			}
			println("Game is finished, press enter to restart or 'x' to exit")
			readLine match {
				case "x" => return
				case _ =>
			}
		}
	}
}

