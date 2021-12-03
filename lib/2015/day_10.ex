defmodule AdventOfCode.Y2015.Day10 do
  @moduledoc """
  --- Day 10: Elves Look, Elves Say ---
  Problem Link: https://adventofcode.com/2015/day/10
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 10

  def run_1, do: input!() |> look_and_say(40) |> String.length()
  def run_2, do: input!() |> look_and_say(50) |> String.length()

  def look_and_say(input) do
    input
    |> String.graphemes()
    |> Enum.chunk_by(&Function.identity/1)
    |> Enum.map_join("", fn [n | _] = lst ->
      "#{length(lst)}#{n}"
    end)
  end

  defp look_and_say(input, times) do
    1..times
    |> Enum.reduce(input, fn _, acc ->
      look_and_say(acc)
    end)
  end
end
