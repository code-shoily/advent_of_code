defmodule AdventOfCode.Y2019.Day02 do
  @moduledoc """
  --- Day 2: 1202 Program Alarm ---
  Problem Link: https://adventofcode.com/2019/day/2
  Difficulty: xs
  Tags: op-code not-fast-enough int-code
  """
  alias AdventOfCode.Helpers.InputReader
  alias AdventOfCode.Y2019.IntCode

  def input, do: InputReader.read_from_file(2019, 2)

  @range 99..0//-1

  def run(input \\ input()), do: {run_1(input), run_2(input)}

  def run_1(input) do
    {:ok, pid} =
      input
      |> parse()
      |> fix1202()
      |> IntCode.start_link()

    IntCode.run(pid)
    IntCode.get_output(pid)
  end

  def run_2(input) do
    memory = input |> parse()

    pairs = for i <- @range, j <- @range, do: {i, j}

    {:ok, pid} = IntCode.start_link(memory)

    Enum.reduce_while(pairs, nil, fn {noun, verb}, _ ->
      new_memory = fix1202(memory, noun, verb)
      IntCode.reset(pid, new_memory)
      IntCode.run(pid)

      case IntCode.get_output(pid) do
        19_690_720 -> {:halt, 100 * noun + verb}
        _ -> {:cont, nil}
      end
    end)
  end

  def parse(data) do
    data
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp fix1202(memory, one \\ 12, two \\ 2) do
    memory
    |> List.replace_at(1, one)
    |> List.replace_at(2, two)
  end
end
