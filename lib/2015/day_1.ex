defmodule AdventOfCode.Y2015.Day1 do
  use AdventOfCode.Data.InputReader, year: 2015, day: 1

  def floor(""), do: 0
  def floor("(" <> rest), do: 1 + floor(rest)
  def floor(")" <> rest), do: -1 + floor(rest)

  def run do
    input!() |> floor()
  end
end
