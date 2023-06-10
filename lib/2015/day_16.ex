defmodule AdventOfCode.Y2015.Day16 do
  @moduledoc """
  --- Day 16: Aunt Sue ---
  Problem Link: https://adventofcode.com/2015/day/16
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 16)

  def run(input \\ input()) do
    input = parse(input)

    {runner(input, &guess_1/1), runner(input, &guess_2/1)}
  end

  def runner(parsed_input, guesstimator), do: guess(parsed_input, guesstimator)

  def parse(data) do
    data
    |> Transformers.lines()
    |> Map.new(fn fact ->
      [aunt | possessions] = String.split(fact, ": ")
      {get_sue_number(aunt), parse_possessions(possessions |> Enum.join(": "))}
    end)
  end

  def parse_possessions(possessions) do
    possessions
    |> String.split(", ")
    |> Map.new(fn possession ->
      [item, quantity] = String.split(possession, ": ")
      {item, String.to_integer(quantity)}
    end)
  end

  defp get_sue_number(aunt) do
    [_, number] = String.split(aunt, " ")

    String.to_integer(number)
  end

  defp guess(aunts, predicate) do
    aunts
    |> Enum.map(fn {aunt, possessions} ->
      points = predicate.(possessions)
      {aunt, points}
    end)
    |> Enum.max_by(fn {_, points} -> points end)
    |> elem(0)
  end

  @ticker_outputs %{
    "children" => 3,
    "cats" => 7,
    "samoyeds" => 2,
    "pomeranians" => 3,
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => 5,
    "trees" => 3,
    "cars" => 2,
    "perfumes" => 1
  }
  defp guess_1(possessions) do
    Enum.count(possessions, fn {item, count} -> @ticker_outputs[item] == count end)
  end

  @ticker_outputs %{
    "children" => 3,
    "cats" => {&Kernel.>/2, 7},
    "samoyeds" => 2,
    "pomeranians" => {&Kernel.</2, 3},
    "akitas" => 0,
    "vizslas" => 0,
    "goldfish" => {&Kernel.</2, 5},
    "trees" => {&Kernel.>/2, 3},
    "cars" => 2,
    "perfumes" => 1
  }
  defp guess_2(possessions) do
    Enum.count(possessions, fn {item, count} ->
      case @ticker_outputs[item] do
        {fun, val} -> fun.(count, val)
        val -> val == count
      end
    end)
  end
end
