/**
 * Peg Solitaire
 * Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>
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

object TUI {
	def main(args: Array[String]) {
		println("Peg Solitaire 0.2dev\n" +
				"  Copyright (C) 2010 Bernd Amend <berndamend+pegsolitaire@googlemail.com>\n" +
		        "  This program is free software: you can redistribute it and/or modify\n" +
		        "  it under the terms of the GNU General Public License version 3 as published by\n" +
		        "  the Free Software Foundation. This program comes with ABSOLUTELY NO WARRANTY\n")

		if(args.length == 0) {
			println("usage [[-board user|english|15holes|euro]|[-load <filename>] [-single|-full] [-save <filename>] [-browser] [-count] [additional options]\n" +
					"  -board <selection>   select a solitaire board\n" +
					"                         user: create your own board!\n" +
					"                         english: standard english\n" +
					"                         15holes: simple test board\n" +
					"                         euro: standard european (not memory optimized)" +
					"  -load <filename>     load a saved field from a file (gz compressed)\n" +
					"  -full                calculate all solutions for all possible start fields\n" +
					"  -single              limit startfield to 1 (use only if -full takes too long)\n" +
					"  -save <filename>     since calculating all solutions for complicated fields\n" +
					"                       takes a while, the results can be saved (gz compressed)\n" +
					"  -browser             interactive text based interface to explore all possible solutions\n" +
					"  -count               count the number of ways to a solution (this may take a while)\n" +
					"  -color               enable colored text output")
			return
		}

		/*
		 * parse command line arguments
		 */
		var arg_single = false
		var arg_full = false
		var arg_load = ""
		var arg_count = false

		var arg_save = ""
		var arg_browse = false

		var arg_board = false
		var selectedGame = GameType.English

		var i=0
		while(i<args.length) {
			args(i) match {
				case "-single" => arg_single = true
				case "-full" => arg_full = true
				case "-load" =>
					i += 1
					if(i == args.length) {
						printError("error: -load requires an additional parameter")
						return
					} else
						arg_load = args(i)
				case "-count" => arg_count = true
				case "-save" =>
					 i += 1
					if(i == args.length) {
						printError("error: -save requires an additional parameter")
						return
					} else
						arg_save = args(i)
				case "-browser" => arg_browse = true
				case "-board" =>
					i += 1
					if(i == args.length) {
						printError("error: -board requires an additional parameter")
						return
					}
					try {
						selectedGame = GameType.withName(args(i))
					} catch {
						case _ => printError("error: unknown game type")
					}
					arg_board = true
				case "-color" => Helper.enableColor = true
				case s => printError("error: unknown parameter " + s + " exit")
						return
			}
			i += 1
		}

		if(!arg_board && arg_load.isEmpty) {
			printError("error: either -load or -board has to be specified")
			return
		}

		if(arg_board && !arg_load.isEmpty) {
			printError("error: -load and -board are mutually exclusive")
			return
		}

		if(arg_full && arg_single) {
			printError("error: -single and -full are mutually exclusive")
			return
		}

		if(arg_load.isEmpty && !arg_full && !arg_single) {
			printError("error: either -single or -full has to be selected")
			return
		}

		if(arg_save.isEmpty && !arg_browse) {
			printError("error: either -save or -browser has to be selected")
			return
		}

		var solitaire: Solver = null

		if(arg_board) {
			var solitaireType = selectedGame match {
				case GameType.English => EnglishBoard
				case GameType.European => EuropeanBoard
				case GameType.Holes15 => Board15Holes
				case GameType.User =>
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
						printError("error: no move directions selected, exit")
						return
					}

					var sol: Board = null
					try {
						sol = new Board(sb.toString, moveDirection.toArray[MoveDirections.Value])
					} catch {
						case _ => printError("error: the entered field is invalid, exit")
						return
					}
				println("Press enter to start solving. This may take a while.")
				readLine
				sol
			}
			if(arg_single) {
				println("Select a start field:")
				val selection = selectField(solitaireType, solitaireType.getCompleteList(solitaireType.possibleStartFields))
				Time("Solve")(solitaire = new Solver(solitaireType, selection))
			} else if(arg_full) {
				Time("Solve")(solitaire = new Solver(solitaireType, solitaireType.possibleStartFields))
			}
		} else if(!arg_load.isEmpty) {
			Time("Load")(solitaire = Solver.fromFile(arg_load))
		} else {
			printError("error: neither -board nor -load is specified, abort")
			return
		}

		try {
			solitaire.getStart
		} catch {
			case _ => printError("error: There are no solutions, exit")
			return
		}

		println("\nPossible fields:")
		var count = 0
		for (i <- 0 until solitaire.game.length) {
			if(solitaire.solution(i) != null) {
				val num = solitaire.solution(i).size
				println("  - removed pegs = " + i + "  possible fields = " + num + " (maybe without equivalent fields)")
				count += num
			}
		}
		printlnColoredText("There are " + count + " possible fields", Color.blue)

		if(arg_count)
			printlnColoredText("There are " + solitaire.countPossibleGames + " ways to a solution", Color.blue)

		if(!arg_save.isEmpty)
			solitaire.save(arg_save)

		if(arg_browse)
			solutionBrowser(solitaire)

		println("Bye, bye")
	}

	/**
	 * Simple console based game-field selection
	 *
	 * @return selected game-field
	 */
	def selectField(game: Board, choices: List[Long]): Long = {
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

		var input = -1
		while (input < 0 || input >= choices.length) {
			print("> ")
			Console.flush
			try {
				input = readInt
			} catch {
				case _ => input = -1
			}
			if (input < 0 || input >= choices.length) {
				printError("error: invalid input, please try again")
			}
		}

		choices(input)
	}

	def solutionBrowser(solitaire: Solver) {
		while (true) {
			println("\nSolution Browser: (x = peg, . = empty)")
			var s = solitaire.getStart
			var f = s(0)
			while (s.length != 0) {
				println("Please choose a move: ")
				f = selectField(solitaire.game, s)
				println()
				println("Current field " + (solitaire.game.length - java.lang.Long.bitCount(f)) + "")
				println(solitaire.game.toString(f))
				println()
				s = solitaire.getFollower(f)
			}
			println("Game is finished, press enter to restart or 'x' to exit")
			readLine match {
				case "x" => return
				case _ =>
			}
		}
	}
}

