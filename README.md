# Rhino

## Installation

Run the following in Rhino's root directory:

```
cabal new-install
```



## Usage

Interpreter for Rhino programs

```
usage: rhino [-i|--include INCLUDE_DIR] [-t|--target VARIABLE]
             [-l|--list-inputs] [-r|--reachable] FILE

Available options:
  -i,--include INCLUDE_DIR Directory to include when searching for modules
  -t,--target VARIABLE     Target variable (default: `main`)
  -l,--list-inputs         List inputs
  -r,--reachable           List only reachable inputs
  FILE                     Program file
  -h,--help                Show this help text
```



## Examples

The `examples` folder contains a small toy program divided into a few different modules. In order to run it, we need to know what its inputs are. We can ask Rhino about that:

```
> rhino -i examples/ examples/salary.rh --list-inputs --reachable | jq
{
  "net_salary_for_alice": {
    "label": "Net salary for Alice"
  },
  "net_salary_for_bob": {
    "label": "Net salary for Bob"
  },
  "tax_rate": {}
}

```

We see that there are three input variables. Each input has a record of metadata associated with it. In this case, the first two inputs have a "label" attribute set. This attribute can, for example, be used by an external program to generate an input form for the program.


Now that we know what the inputs are, we can run the program by providing an input environment as a JSON object on `stdin`:

```
> rhino -i examples/ examples/salary.rh <<< '{"tax_rate": 0.3, "net_salary_for_alice": 20000, "net_salary_for_bob": 20000}'
28000
```

The above is the evaluation of the `main` definition in the given environment. Use the `--target` option to evaluate another definition.

### Input reachability

Note that the above `--list-inputs` query returned the inputs reachable from the `main` definition. If we specify a different target, the set of reachable inputs will be different:

```
> rhino -i examples/ examples/salary.rh --list-inputs --reachable --target salary_bob | jq
{
  "net_salary_for_bob": {
    "label": "Net salary for Bob"
  },
  "tax_rate": {}
}

```

As mentioned earlier, the listed inputs and their attributes can be used to generate external input form for the program. The reachability analysis ensures that derived views only show inputs that actually contribute to the desired target definition.
