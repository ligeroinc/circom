
# Ligetron Target Support in the Circom Compiler

## Building the Circom Compiler from Sources

To build Circom compiler with the Ligetron target support,
checkout the ligetron branch and use the
`cargo build --release` command

```bash
cd <circom_src_dir>
git checkout ligetron
cargo build --release
```

The compiler executable will be placed in the `<circom_src_dir>/target/release` directory.

## Compiling Circom Programs for the Ligetron Target

To compile a Circom program for the Ligetron target, pass the `--ligetron-wasm` flag to the compiler:

```bash
<circom_src_dir>/target/release/circom <program>.circom --ligetron-wasm
```

The Circom compiler will generate WASM code for the Ligetron target and place it in the `<program>_ligetron/<program>.wasm` file, where `<program>` is the name of the Circom program.

## Running a Compiled Program with Ligetron

To run a compiled program with Ligetron, execute the demo executable in the folder containing the compiled `.wasm` program:

```bash
<ligetron_build>/demo '{"program":"<program>.wasm", "packing": 8192, "private-indices":[],"args":[{"i64":1},{"i64":2}]}}'
```

You must provide arguments for all input signals of the main component of the Circom program. If an input signal is an array of size N, you should pass N arguments for that input signal.

## Building example program
1. Save the following code in the `xor3.circom` file:
    ```
    pragma circom 2.0.0;

    template Xor3 {
        signal input a;
        signal input b;
        signal input c;
        signal input d;
        signal output out;
        signal mid;

        mid <== b * c;
        out <== a * (1 -2*b  -2*c +4*mid) + b + c -2*mid;
        out === d;
    }

    component main = Xor3();
    ```
2. Compile the `xor3.circom` program:
    ```bash
    <circom_src_dir>/target/release/circom xor3.circom --ligetron-wasm
    ```

3. Run compiled program:
    ```
    <ligetron_build>/demo '{"program":"xor3.wasm","packing":8192,"private-indices":[],"args":[{"i64":1},{"i64":2},{"i64":3},{"i64":8}]}'
    ```
