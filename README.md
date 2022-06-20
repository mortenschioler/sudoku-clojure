# sudoku

Tiny spare-time project trying to make an idiomatic sudoku solver in Clojure.

## Usage

(time (print-sudoku (solve-sudoku [0 9 0   1 4 0   0 0 0
                                   0 0 5   0 0 0   0 2 0
                                   0 3 0   0 0 0   0 6 0

                                   0 4 6   0 0 0   0 0 0
                                   1 2 0   9 3 0   0 4 5
                                   0 0 3   0 0 4   0 0 6

                                   4 0 0   0 0 1   2 0 0
                                   0 8 0   4 0 0   0 0 3
                                   3 5 0   7 0 0   9 0 0])))

## License

Copyright © 2022 Morten Schiøler

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
