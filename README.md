# A solution of the Date Puzzle

This repository deal with the puzzle as follow:

Here is a sepecial kind of Jigsaw Puzzle. In the puzzle, you have several kinds of piece to fill the map. The pieces get three blocks less than the map size, therefore it will be three blank blocks when you fill up all the pieces. Every single block is written a word in the map except the one at top-left. And the words contain Januery-December, 1st-31st, and Sunday-Saturday. So three blocks can match the date when you try to solve the puzzle. It's like this:

![image](pic.jpg)

The algorithm in this repository to solve this problem is DSF(Depth First Search). The program allow user select one of these mode:

1. Compute one solution for a user specified situation.
2. Compute all combination for date and weekday, try to find one situation that has none solution. (In another word, checking out if all situations get the solution).

== Version 2 ==

```txt
Usage: DatePuzzle [options]

Options:
-h, --help: Print help messages.
-m, --map: Specify a map-pieces file, instead of default map&pieces
-v, --verify: Configure if the program should find solutions of all combos or only one solution by user console input.
```

-v/--verify option is false by default. 

- When specified false, the program enter a loop, reading user input and solve the situation user specified.
- When specified true, the program will calculate all combos for search area, to verify this map-pieces combo is valid.

-m/--map option allow user supply custom map-pieces.

It is a map and pieces like above picture by default. But if you config -m option, the map-pieces will be read from the file specified.

File format:

```txt
xSize,ySize
map
invalid
x1,y1-x2,y2
x3,y3
x4,y4
end invalid
month 1
x1,y1
x2,y2-x3,y3
end month
day 1
x1,y1
x2,y2-x3,y3
end day
end map
pieces
piece
x1,y1
x2,y2-x3,y3
end piece
end pieces
```

NOTICE:

- First line is map size.
- There are two main section: map and pieces
- In sub-section of map/pieces, you config coordinates. One area can have many coordinates. coordinates follow two format:
    - `x,y` : means a single coordinate
    - `x1,y1-x2,y2` : means a rect that starts from left-top by x1,x1, ends to right-bottom by x2,y2.
        - **DO NOT** make x1 > x2 or y1 > y2, that will make the rect **EMPTY**.
- In pieces section, there will be several sections named piece. Each one have many coordinates, to describe the shape of the piece.
- In map section, there will be several sections:
    - invalid section, which means the coordinates can not be placed with piece.
    - search area section, which config a search area. The name is not limited that You can specify any word. You should specify another integer, which means this section index start with it. For example, you can specify month section, that start with 1. The name will be shown on the Selection Info in single mode. Section name is not case-sensitive, and every custom area name will be transform into lower-case form.
        - The search coordinates describe how to search the area. In a area, the coordinates will search one by one, to make a blank on the map. Several areas are searching in the same time. That means, 'month' and 'day' will take two blanks , one is from month area, another is from day area, to complete the solution.
