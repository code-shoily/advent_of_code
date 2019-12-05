defmodule AdventOfCode.Y2019.Day5 do
  @moduledoc """
  Problem description: Problem description: https://adventofcode.com/2019/day/5
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 5

  @bins [3, 4, 103, 104]
  @save "00003"

  def run_1 do
    with data <- process(),
         cmds <- chunkify(data) do
      cmds
      |> tl()
      |> run_program(
        cmds
        |> hd()
        |> init(data, 1)
        |> IO.inspect(label: :oops)
      )
    end
  end

  def process, do: input!() |> String.split(",") |> Enum.map(&String.to_integer/1)
  def init([@save, ax], data, a), do: List.replace_at(data, ax, a)

  def run_program(cmds, state) do
    step_through(cmds, state, [])
  end

  def step_through(["00099" | _], _state, output) do
    output
  end

  def step_through([cmd | rest], state, output) do
    {new_state, new_output} = next(cmd, state, output)
    step_through(rest, new_state, new_output)
  end

  def next(["00001", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) + Enum.at(state, b)
       ), output}

  def next(["00101", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) + b
       ), output}

  def next(["01001", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a + Enum.at(state, b)
       ), output}

  def next(["01101", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a + b
       ), output}

  def next(["00002", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) * Enum.at(state, b)
       ), output}

  def next(["00102", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         Enum.at(state, a) * b
       ), output}

  def next(["01002", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a * Enum.at(state, b)
       ), output}

  def next(["01102", a, b, x], state, output),
    do:
      {List.replace_at(
         state,
         x,
         a * b
       ), output}

  def next(["00004", x], state, output),
    do: {state, [Enum.at(state, x) | output]}

  def next(["00104", x], state, output), do: {state, [x | output]}

  defp chunkify(data) do
    pad_zeroes = fn x -> x |> to_string() |> String.pad_leading(5, "0") end
    format_op = fn [x | rest] -> [pad_zeroes.(x) | rest] end

    data
    |> Enum.chunk_while(
      [],
      fn
        el, [] -> {:cont, [el]}
        el, [99 | _] = chunk -> {:cont, chunk ++ [el]}
        el, [x] when x in @bins -> {:cont, [x, el]}
        el, [x, _] = chunk when x in @bins -> {:cont, format_op.(chunk), [el]}
        el, [x] -> {:cont, [x, el]}
        el, [x, a] -> {:cont, [x, a, el]}
        el, [x, a, b] -> {:cont, [x, a, b, el]}
        el, [_, _, _, _] = chunk -> {:cont, format_op.(chunk), [el]}
      end,
      fn chunk -> {:cont, format_op.(chunk), []} end
    )
  end

  def run_2 do
    2
  end

  def run, do: %{problem_1: run_1(), problem_2: run_2()}
end
