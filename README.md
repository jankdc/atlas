# ATLAS Programming Language

## Usage:

java -jar atlas_jar_file [option] src_file

Legend:
atlas_jar_file = The path to the ATLAS compiler
src_file = The path to the ATLAS source code

## Build:

### Dependencies

- Simple Build Tool 0.13 (sbt)
- LLVM 3.5 Compiler Tools (llc)
- For OS X Yosemite: Object linker (ld)
- For Ubuntu Linux: Modern C++11 Compiler (c++)
- For Windows: Not supported (yet)

All of the programs above must be in the PATH of the terminal or console you
are running.

### How-to

In order to build the project, please cd to the root of the project, and
run the 'sbt one-jar' command. This will build the compiler and
repackaged into an all-in-one .jar file. The .jar file is located in the
project_root/target/scala-2.11 directory. The .jar file is suffixed with a
'one-jar'.
