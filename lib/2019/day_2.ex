defmodule AdventOfCode.Y2019.Day2 do
  @moduledoc """
  Problem description: https://adventofcode.com/2019/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 2

  @spec run_1 :: integer
  def run_1 do
    state = process()
    process_command(state, state)
  end

  def process(noun \\ 12, verb \\ 2) do
    input!()
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
    |> last_state(noun, verb)
  end

  defp last_state(state, noun, verb) do
    state |> List.replace_at(1, noun) |> List.replace_at(2, verb)
  end

  defp process_command([99 | _], state), do: hd(state)

  defp process_command([op, a, b, out | rest], state) do
    process_command(rest, next({op, a, b, out}, state))
  end

  defp next({op, a, b, out}, state) do
    List.replace_at(
      state,
      out,
      apply(
        case op do
          1 -> &Kernel.+/2
          2 -> &Kernel.*/2
        end,
        [Enum.at(state, a), Enum.at(state, b)]
      )
    )
  end

  @spec run_2 :: integer
  def run_2, do: test_run(input_pairs(), 0)

  defp test_run(_, {noun, verb, 19_690_720}), do: noun * 100 + verb

  defp test_run([{noun, verb} | rest], _) do
    state = process(noun, verb)
    test_run(rest, {noun, verb, process_command(state, state)})
  end

  defp input_pairs do
    for noun <- 0..99,
        verb <- 0..99 do
      {noun, verb}
    end
  end

  @spec run :: %{problem_1: integer, problem_2: integer}
  def run do
    %{
      problem_1: run_1(),
      problem_2: run_2()
    }
  end
end
