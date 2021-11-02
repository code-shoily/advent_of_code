defmodule AdventOfCode.Y2019.Day05 do
  @moduledoc """
  --- Day 5: Sunny with a Chance of Asteroids ---
  Problem Link: https://adventofcode.com/2019/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2019, day: 5

  alias AdventOfCode.Y2019.IntCode

  def parse(data), do: data |> String.split(",") |> Enum.map(&String.to_integer/1)

  def run_1 do
    {:ok, pid} = input!() |> parse() |> IntCode.start_link()
    %{output: output} = IntCode.run(pid)

    output
    |> Enum.reject(&(&1 == 0))
    |> case do
      [data] -> data
      [] -> -1
    end
  end

  def run_2 do
    {:not_implemented, 2}
  end
end
