defmodule AdventOfCode.Y2015.Day1 do
  def input!(), do: AdventOfCode.Data.InputReader.read!(2015, 1)
  def floor(""), do: 0
  def floor("(" <> rest), do: 1 + floor(rest)
  def floor(")" <> rest), do: -1 + floor(rest)

  def run do
    input!() |> floor()
  end
end
