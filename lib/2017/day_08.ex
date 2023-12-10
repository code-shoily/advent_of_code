defmodule AdventOfCode.Y2017.Day08 do
  @moduledoc """
  --- Day 8: I Heard You Like Registers ---
  Problem Link: https://adventofcode.com/2017/day/8
  Difficulty: s
  Tags: op-code
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 8)

  def run(input \\ input()) do
    input
    |> parse()
    |> Enum.reduce(
      {%{}, 0},
      fn {reg, dep, updater, predicate}, {acc, highest} ->
        case predicate.(Map.get(acc, dep, 0)) do
          true ->
            value = updater.(Map.get(acc, reg, 0))
            {Map.put(acc, reg, value), max(value, highest)}

          _ ->
            {acc, highest}
        end
      end
    )
    |> then(fn {registers, highest} ->
      {Enum.max(Map.values(registers)), highest}
    end)
  end

  @upd %{"inc" => :+, "dec" => :-}
  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [reg, upd, by, "if", dep, comp, val] = Transformers.words(line)

      {
        reg,
        dep,
        &apply(Kernel, @upd[upd], [&1, String.to_integer(by)]),
        &apply(Kernel, String.to_existing_atom(comp), [&1, String.to_integer(val)])
      }
    end)
  end
end
