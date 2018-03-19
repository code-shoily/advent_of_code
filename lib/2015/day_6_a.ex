defmodule AdventOfCode.Y2015.Day6a do
  use AdventOfCode.Data.InputReader, year: 2015, day: 6

  def start(size), do: Agent.start(fn -> 0..(size * size - 1) |> Enum.to_list() end)

  def run do
    input!()
  end
end
