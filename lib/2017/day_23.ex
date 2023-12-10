defmodule AdventOfCode.Y2017.Day23 do
  @moduledoc """
  --- Day 23: Coprocessor Conflagration ---
  Problem Link: https://adventofcode.com/2017/day/23
  Difficulty: m
  Tags: op-code
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @state %{
    registers: %{
      "a" => 0,
      "b" => 0,
      "c" => 0,
      "d" => 0,
      "e" => 0,
      "f" => 0,
      "g" => 0,
      "h" => 0
    },
    index: 0,
    muls: 0
  }

  def input, do: InputReader.read_from_file(2017, 23)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    process(@state, input)
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split(" ")
      |> parse_line()
    end)
    |> Enum.with_index()
    |> Map.new(fn {v, k} -> {k, v} end)
  end

  defp parse_line([cmd, op1, op2]) do
    {cmd, parse_arg(op1), parse_arg(op2)}
  end

  defp parse_arg(arg) do
    case Integer.parse(arg) do
      {num, ""} -> num
      _ -> arg
    end
  end

  defp process(%{index: index, muls: muls}, commands)
       when index > map_size(commands) - 1 or index < 0,
       do: muls

  defp process(%{index: index} = state, commands) do
    case commands[index] do
      {"set", x, y} -> process(:set, state, commands, x, y)
      {"sub", x, y} -> process(:sub, state, commands, x, y)
      {"mul", x, y} -> process(:mul, state, commands, x, y)
      {"jnz", 0, _} -> process(:jnz, state, commands)
      {"jnz", x, y} -> process(:jnz, state, commands, x, y)
    end
  end

  defp process(:set, %{index: index, registers: registers} = state, commands, x, y) do
    y_value = (is_integer(y) && y) || registers[y]

    state
    |> Map.merge(%{registers: %{registers | x => y_value}, index: index + 1})
    |> process(commands)
  end

  defp process(:sub, %{index: index, registers: registers} = state, commands, x, y) do
    y_value = (is_integer(y) && y) || registers[y]

    state
    |> Map.merge(%{
      registers: %{registers | x => registers[x] - y_value},
      index: index + 1
    })
    |> process(commands)
  end

  defp process(:mul, %{index: index, registers: registers, muls: muls} = state, commands, x, y) do
    y_value = (is_integer(y) && y) || registers[y]

    state
    |> Map.merge(%{
      registers: %{registers | x => registers[x] * y_value},
      index: index + 1,
      muls: muls + 1
    })
    |> process(commands)
  end

  defp process(:jnz, %{index: index, registers: registers} = state, commands, x, y) do
    y_value = (is_integer(y) && y) || registers[y]
    jump_value = (registers[x] == 0 && 1) || y_value

    state
    |> Map.put(:index, index + jump_value)
    |> process(commands)
  end

  defp process(:jnz, %{index: index} = state, commands) do
    state
    |> Map.put(:index, index + 1)
    |> process(commands)
  end
end
