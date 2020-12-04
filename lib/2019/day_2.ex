defmodule AdventOfCode.Y2019.Day2 do
  @moduledoc """
  Problem description: https://adventofcode.com/2019/day/2
  """
  use AdventOfCode.Helpers.InputReader, year: 2019, day: 2

  alias AdventOfCode.Y2019.IntCode

  @range 99..0

  @spec run_1 :: integer()
  def run_1 do
    {:ok, pid} = IntCode.start_link(fix1202(process()))
    IntCode.run(pid)
    IntCode.get_output(pid)
  end

  def process() do
    input!()
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp fix1202(memory, one \\ 12, two \\ 2) do
    memory
    |> List.replace_at(1, one)
    |> List.replace_at(2, two)
  end

  @spec run_2 :: integer | nil
  def run_2 do
    memory = process()

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

  def run, do: {run_1(), run_2()}
end
