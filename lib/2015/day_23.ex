defmodule AdventOfCode.Y2015.Day23 do
  @moduledoc """
  --- Day 23: Opening the Turing Lock ---
  Problem Link: https://adventofcode.com/2015/day/23
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 23

  require Integer

  alias AdventOfCode.Helpers.Transformers

  def run(input \\ input!()) do
    input = parse(input)
    size = Enum.count(input)

    {
      execute(input, %{a: 0, b: 0}, 1, size).b,
      execute(input, %{a: 1, b: 0}, 1, size).b
    }
  end

  defp execute(_, registers, idx, size) when idx > size, do: registers

  defp execute(instructions, registers, idx, size) do
    instruction = Map.fetch!(instructions, idx)
    {next_idx, registers} = handle_instruction({idx, instruction}, registers)
    execute(instructions, registers, next_idx, size)
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      ~r/\s+|\,/
      |> Regex.split(line, trim: true)
      |> parse_instruction()
    end)
    |> Enum.with_index(1)
    |> Map.new(fn {a, b} -> {b, a} end)
  end

  defp parse_instruction([instruction, register, value]) do
    {
      to_atom(instruction),
      to_atom(register),
      String.to_integer(value)
    }
  end

  defp parse_instruction(["jmp", value]) do
    {:jmp, String.to_integer(value)}
  end

  defp parse_instruction([instruction, register]) do
    {
      to_atom(instruction),
      to_atom(register)
    }
  end

  @tokens ~w/a b hlf tpl jmp inc jie jio/
  defp to_atom(instruction) when instruction in @tokens do
    String.to_atom(instruction)
  end

  defp handle_instruction({idx, {:inc, register}}, registers) do
    {idx + 1, Map.update(registers, register, 1, &(&1 + 1))}
  end

  defp handle_instruction({idx, {:hlf, register}}, registers) do
    {idx + 1, Map.update(registers, register, 0, &div(&1, 2))}
  end

  defp handle_instruction({idx, {:tpl, register}}, registers) do
    {idx + 1, Map.update(registers, register, 0, &(3 * &1))}
  end

  defp handle_instruction({idx, {:jmp, value}}, registers) do
    {idx + value, registers}
  end

  defp handle_instruction({idx, {:jio, register, value}}, registers) do
    register_value = Map.fetch!(registers, register)
    next_idx = (register_value == 1 && idx + value) || idx + 1

    {next_idx, registers}
  end

  defp handle_instruction({idx, {:jie, register, value}}, registers) do
    register_value = Map.fetch!(registers, register)
    next_idx = (Integer.is_even(register_value) && idx + value) || idx + 1

    {next_idx, registers}
  end
end
