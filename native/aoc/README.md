# NIF for Elixir.AdventOfCode

## To build the NIF module:

- Your NIF will now build along with your project.

## To load the NIF:

```elixir
defmodule AdventOfCode do
  use Rustler, otp_app: :advent_of_code, crate: "aoc"

  # When your NIF is loaded, it will override this function.
  def add(_a, _b), do: :erlang.nif_error(:nif_not_loaded)
end
```

## Examples

[This](https://github.com/rusterlium/NifIo) is a complete example of a NIF written in Rust.
