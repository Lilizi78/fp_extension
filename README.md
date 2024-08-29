SGen Tool Enhancement

Project Overview

This project is based on the SGen tool developed by ETHZ, with the goal of extending its original functionality. The SGen tool is written in Scala and uses sbt for compilation. The main objective of this task is to add a radix 2^k structure to support more complex computations and reduce the recursive depth during FFT calculations.

First Upload: Enhancements
In the first round of modifications, we focused on the following key areas to support the radix 2^k structure:

Support for radix 2^k structure:

Added support for the radix 2^k structure to enable the tool to handle more complex FFT computations, thereby reducing the recursive depth.
Configuration of Butterfly.scala and DFT.scala:

Modified the Butterfly.scala and DFT.scala files to accommodate the newly added radix 2^k structure.
Addition of DFT4.scala and DFT8.scala:

Introduced DFT4.scala and DFT8.scala to handle specific DFT computation scenarios.
Inclusion of Debugging Output:

Added several println statements throughout the code to facilitate better tracking and debugging of the program's execution.
Creation of F4 and F8 Fields:

Defined and implemented F4 and F8 fields to support different levels of FFT computations.
