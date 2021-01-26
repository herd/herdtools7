Litmus Tests
===

This directory contains files to test the `litmus7` tool, specifically for the Neon extension of AArch64.
Each `*.litmus` file contains a litmus test for a specific instruction or feature indicated by a comment.
The corresponding `*.expected` file contains the expected result of running the test on hardware.

`litmus7` can be use to generate C code to run a litmus test on hardware.
The following command will create `V01.tar` containing all of the relevant files:
```bash
litmus7 -o V01.tar V01.litmus
```

The archive may need to be moved to a separate piece of hardware, which can be done with the `scp` command.

The files can be extracted using the `tar` command:
```bash
tar xvf V01.tar
```

Running `comp.sh` will compile the C code to an executable.
Note that this script may need to be given execute permissions.
Running the executable will execute the litmus test on hardware.

Alternatively, `run.sh` can be used to run the test.
Again, this may need to be given execute permissions.
