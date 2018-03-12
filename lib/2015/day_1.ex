defmodule AdventOfCode.Y2015.Day1 do
  use AdventOfCode.Data.InputReader, year: 2015, day: 1

  def floor("", current_floor), do: current_floor
  def floor("(" <> rst, current_floor), do: floor(rst, current_floor + 1)
  def floor(")" <> rst, current_floor), do: floor(rst, current_floor - 1)

  def run do
    input!() |> floor(0)
  end
end
