defmodule AdventOfCode.RustNif do
  @moduledoc """
  Module to list all NIFs used from Rust
  """
  use Rustler,
    otp_app: :advent_of_code,
    crate: :aoc

  def solve(_year, _day), do: :erlang.nif_error(:nif_not_loaded)
end
