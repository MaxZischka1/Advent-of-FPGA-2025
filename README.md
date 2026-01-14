# Advent-of-FPGA-2025
 
# Day 1

## System approach: 
I utilized a CPU for parsing and an FPGA for the actual solution logic
* **CPU:** Handles string parsing and arithmetic like division/modulo operations.
* **FPGA (intended target) Intel MAX 10:** Implements the core logic and state machine.
* **Communication:** Data is streamed via UART (8-bit) using a protocol with `valid`, `data`, `clk`, and `clear` signals.

## Design Overview

For Day 1, my approach was based on the idea that each rotation command consists of three parts: a character for direction, a decimal value to add or subtract from the total, and a newline character that marks the end of the command. To build the parser, I took the raw input string which is VAL\nVAL\nVAL... vand used the split_lines operator to break it into a list of individual strings. Once I had that list, I iterated through it to separate each string into its two core pieces being the character value and the numeric value.

Parser Implementation [Day1Parser.ml](Day1Pt1/src/Day1Parser.ml)

I chose to have the input data separated on the CPU and then sent to the FPGA in 8-bit streams because I targeted a MAX10 that only has UART communication, and I thought tasks like parsing a massive string would be much easier for a CPU to do than an FPGA. The communication protocol relies on `clk`, `clear`, `data`, and `valid` pins to ensure that state changes only occur when the valid signal is high, which is important for the UART communication. For part 1 structure, I began with the state machine structure. We needed an idle state where nothing happened, and then two states for the different pieces of data being received, these being the character and number. From here, in the “Eval” state, you are toggling the direction to either be positive for R and negative for L. This is also in the Idle state because the first data signal is an L or R input. For this, I simply made a register `dir` that toggles 0 or 1 to decide if, in the AddSum state, you should add or subtract the number input. After running through an iteration where in Eval or Idle State, the dir register is assigned a 0 or 1, then the total sum is added to by the newest number input before checking if this value is equal to 0. If the total sum is equal to zero, then increment the total count counter. The final value is the total count. For handling overflows, the approach I took was just to check if the total sum is greater than 100 first, then to check its direction. This is because the total sum is an 8-bit unsigned number, so numbers decremented below zero and incremented above 100 will both be values greater than 100. Then you just have to check the direction used in the previous operation to decide if you must add or subtract 100 from the current total sum.

State Machine Implementation [Day1Adder.ml](Day1Pt1/src/Day1Adder.ml)

For Part 2, the counting shifted from when it simply landed on zero to tracking every time the dial rotated over it. There are now three scenarios where the counter needs to increment: when the sum exceeds 100, indicating a rollover or rollunder, when the dial lands exactly on zero, and when a single command spans more than 100 rotations. So the first statement I added was if the total sum is greater than 100, add one. This means if it rotates over or under 100, then increment by one because it has passed zero. Doing this keeps things simple, but creates an edge case when the dial is at exactly zero, and the next operation is a rotate ‘L’ operation, because we have already incremented the value for going over 0, so when it overflows, it will trigger another increment, which is incorrect. The way I handled this edge case was simply by adding the statement that when the direction is ‘L’ and the total sum is exactly zero, decrement the value by 1 to cancel the increment it will get for overflowing. Lastly, the way I decided to handle the case where the number in an instruction is greater than 100 was by letting the CPU handle this task. I did this because I thought the division operation is much easier to do in a CPU than an FPGA, and I can parse the mod value and the integer division with 100, so I can keep almost the exact same state machine as part one and just add a state where the count is incremented by whatever numVal/100 was.

Final Parser [Day1Parser.ml](Day1/src/Day1Parser.ml)
Final Logic [Day1Adder.ml](Day1/src/Day1Adder.ml)

# Day 3

## System approach: 
I utilized a CPU for parsing and an FPGA for the actual solution logic
* **CPU:** Handling String parsing, creating a tuple of number and position inputs.
* **FPGA (intended target) Intel MAX 10:** Implements the feedback register and accumulator

## Design Overview

For the Day 3 implementation, I designed a system based on two feedback registers that continuously compare the current output against the incoming data stream. To support this logic, I first developed a parser to transform the raw input into a format suitable for the hardware. The parser takes a large string input, breaks it down by line, and then further splits each line into individual characters. As the program iterates through these characters, it converts them into a constant 4-bit input representing values between 1 and 9.

Parser Implementation: [Day3Parser.ml](Day3Pt1/src/DayThreeParser.ml)

The logic behind the tens digit or “val1” feedback register is to simply change to the input data if the input data is greater. This is pretty simple because the largest two-digit numbers will have the largest number in the tens digit. Then, for the ones digit or “val2”, there are three cases to consider: 1. The input is greater than the current val1, 2. The input isn’t greater than val1, but greater than the current val1 and 3. The input isn’t greater than either. So if the input isn’t greater than either, we just keep the input the same. If the input is greater than val2, we treat it the same way we did with val1 and just replace the currentVal with the input. If, however, it is greater than val1, then we set val2 to zero. The reason for this is we want the greatest val1 possible, but because the order of numbers is important, the ones digit must be after the tens digit, so by setting val2 to zero, this is saying we want the next possible val2 input because it is after the greatest val1 possible. This is shown with the mux used with `sel` where I created a `sel1` and `sel2`. If sel1 is high, then sel is 2 or 3 so both those positions in the mux equate to zero. And then positions 0 and 1 of the mux are based on sel2, so if the input is only greater than val2, then sel2 is on, and sel2 equals the input. If both are zero, keep the current value. Now an issue occurs when the greatest value is the last digit of input because output with be `greatestVal @: 0`, which is not write. What we know is that the val1 one clock cycle previous to the greatestVal replacement was holding the second largest. So if we hold the val1 of one clock cycle previous and concatenate it with the greatestVal, positioning it in the ones digit, we get the greatest possible value. To detect this our finalVal has mux checking if val2 is zero and if so use `val3 @: val1` where val3 is the previous val1. The one last issue I had was actually accumulating these values. For this, I had to create a register that only updated after an entire line of the input was run through. For this, I thought of the fact that after the last input from a line was run through, I could turn the valid input low, and the falling edge of the valid signal could update the registers. This works because it is a unique signal that is only turned on at the end of the last line. In actual implementation with UART, I think this would have to be changed to some new signal or possibly a data input like 0. With the finished signal, it only updates the register at the end of an entire line and holds the value. It also resets the feedback register, which is important because if not, it would just hold the greatest values from the entire string input, not per line. Finally, we just convert the BCD output to decimal and continue adding after every line by using the same falling valid edge as the enable.

Feedback Register and Accumulator:[DayThreeComparatorpt1.ml](Day3Pt1/src/DayThreeComparatorpt1.ml)

The change from pt1 to pt2 required a significant shift in structure to handle 12-digit numbers efficiently. Instead of relying on wonky concatenation logic for the zero edge cases like above, I used the character position as a second input by updating the parser to use `mapi`. Now the changes I had to make to the comparator included the accumulator logic, feedback logic and number of feedback registers. So obviously, we need 12 registers instead of 2 and the logic works differently, but not too different, based on how I built it. So for this I was actually able to keep the mux logic the exact same and instead just changed the sel1 logic along with including the position. For this, the same questions are asked with each register, but now the position is also included; is the data input greater than the currentVal AND would it not lead to trailing zeros, meaning the position is less than (the total number of inputs - the digits position ). So this makes the logic actually very simple, asking the same question as before but now always considering position. The sel1 is simply checking if a digit above has picked the number. If a digit above has this means there are definitely enough numbers still to be inputted to complete the number, so it should be set to zero. The `sel2` is checking if this digit should pick up the number, asking just two questions: is it greater than the current, and it the position less than (number of characters - digit pos). Then the same mux logic from above applies. The most computationally expensive task I thought would be the accumulation. For this, I implemented a hardware-efficient version of a `List.fold_left` tail recursion. If you keep an accumulator going you essentially shift and add the next decimal value in base 10. So you are just doing listInput+(10*(acc)) recursively, creating what I think is the most efficient approach in hardcaml. Finally with a binary number you can simply add it to the current accumulator output.

Final Comparator: [DayThreeComparator.ml](Day3/src/DayThreeComparator.ml)
Final Parser: [DayThreeParser.ml](Day3/src/DayThreeParser.ml)

# Implementation

In terms of synthesis onto an actual FPGA I was unsuccesful in getting an FPGA from schools workshop since coming back to school. So the only implementation I have is in testcases. I used Cyclesim and GTKWave to test my outputs. To test my output first clone the project, then put in `dune build` before doing `dune runtest Day3/test` to test the output of Day3 with the test vals. To alter the test inputs, go to inputs.txt in Day3/test and change to whatever is desired, then in [DayThreeComparator.ml](Day3/src/DayThreeComparator.ml) alter the `totalChars` to however long each string input is. It will output every single line of the accumulator before outputting FinalVal. For Day1 I implement a UART “wrapper”. I don’t have any experience making wrappers like this, but I wanted to implement this for when I am able to test it on an FPGA. This output can be viewed in GTKWaveform. First, you can run dune build then `dune exec bin/main.exe`. Then run `gtkwave waveform.vcd`. To install GTKWaveform I believe just run `brew install --cask gtkwave`.

UART Implementation: (For GTKWave fake 12.5 Million Baud Rate):[UART RX and TX](./lib)

Wrapper Implementation: [Day1Wrapper.ml](/Wrapper/src/Day1Wrapper.ml)

Main file to test Day1 on GTKWave: [main.ml](/bin/main.ml)

GTKWave Solution found at the end of the simulation in count Day1: 
<img width="1330" height="592" alt="Screenshot 2026-01-14 at 12 13 53 PM" src="https://github.com/user-attachments/assets/67edea71-98b0-45ce-a6c4-aeb2c57c8849" />

Cyclesim Terminal Solution Day3:

<img width="571" height="135" alt="Screenshot 2026-01-14 at 1 09 44 PM" src="https://github.com/user-attachments/assets/f3afe1ad-567b-4e8d-97c9-ea312aa20d70" />




# Resources
Here are some resources I used to learn Hardcaml

[Helpful manual](https://hardcaml-mini-course-at-stevens.github.io/hardcaml-docs/introduction/why)

[Hardcaml Doc](https://ocaml.org/p/hardcaml/v0.17.1/doc/hardcaml/Hardcaml/Signal/index.html)

[VERY helpful playlist](https://www.youtube.com/watch?v=MUcka_SvhLw&list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU)

[Great problem set to try in HardCaml](https://hdlbits.01xz.net/wiki/Problem_sets)

# Final comments
Thank you so much to the people at Jane Street for creating the Advent of FPGA challenge it was a really fun challenge to learn Hardcaml and then tackle the problems over December and January. I am very disappointed I was unable to synthesize and tinker getting it to work on an FPGA but if the challenge is ever run again, which I hope it is, I want to have a much better output. If anybody reading this has any resources on how someone can improve in the FPGA space, I would love any feedback from how I write my code to approaching problems and literally anything else please send me a message @ max.zischka1@gmail.com.


