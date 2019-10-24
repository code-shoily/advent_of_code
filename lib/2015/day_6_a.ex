defmodule AdventOfCode.Y2015.Day6a do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 6

  @size 1000

  def start, do: Agent.start(fn -> 0..(@size * @size - 1) |> Enum.to_list() end)

  def find_range(s), do: s |> String.trim()

  def parse_instruction("turn on" <> rest), do: {true, rest |> find_range()}
  def parse_instruction("turn off" <> rest), do: {false, rest |> find_range()}
  def parse_instruction("toggle" <> rest), do: {:toggle, rest |> find_range()}

  def parse_line(line) do
    line
    |> String.replace("through", "")
  end

  def run do
    input!()
    |> parse_line()
    |> String.split("\n")
    |> Enum.map(&parse_instruction/1)
  end
end
