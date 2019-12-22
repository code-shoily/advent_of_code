defmodule AdventOfCode.Y2019.Day5 do
  @moduledoc """
  Problem description: Problem description: https://adventofcode.com/2019/day/5
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 5

  alias AdventOfCode.Y2019.IntCode

  def process, do: input!() |> String.split(",") |> Enum.map(&String.to_integer/1)

  def run_1 do
    {:ok, pid} = IntCode.start_link(process())
    %{output: output} = IntCode.run(pid)

    output
    |> Enum.reject(&(&1 == 0))
    |> case do
      [data] -> data
      [] -> -1
    end
  end

  def run_2 do
    :not_implemented
  end

  def run, do: %{problem_1: run_1(), problem_2: run_2()}
end
