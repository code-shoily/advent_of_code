defmodule AdventOfCode.Y2016.Day12 do
  @moduledoc """
  --- Day 12: Leonardo's Monorail ---
  Problem Link: https://adventofcode.com/2016/day/12
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @input InputReader.read_from_file(2016, 12)

  @tokens ~w/a b c d cpy inc dec jnz/
  @regs %{"a" => 0, "b" => 0, "c" => 0, "d" => 0}

  def run(input \\ @input) do
    instructions = parse(input)

    solution_1 = Task.async(fn -> run_1(instructions) end)
    solution_2 = Task.async(fn -> run_2(instructions) end)

    {
      Task.await(solution_1, :infinity),
      Task.await(solution_2, :infinity)
    }
  end

  def run_1(instructions) do
    Map.get(exec(instructions, @regs, 0, Enum.count(instructions)), "a")
  end

  def run_2(instructions) do
    Map.get(exec(instructions, %{@regs | "c" => 1}, 0, Enum.count(instructions)), "a")
  end

  defp exec(_, regs, s, s), do: regs

  defp exec(cmds, regs, idx, s) do
    case cmds[idx] do
      ["cpy", val, reg] when is_integer(val) ->
        exec(cmds, %{regs | reg => val}, idx + 1, s)

      ["cpy", reg_val, reg] ->
        exec(cmds, %{regs | reg => regs[reg_val]}, idx + 1, s)

      ["inc", reg] ->
        exec(cmds, %{regs | reg => regs[reg] + 1}, idx + 1, s)

      ["dec", reg] ->
        exec(cmds, %{regs | reg => regs[reg] - 1}, idx + 1, s)

      ["jnz", val, step] when is_integer(val) ->
        offset = (val == 0 && 1) || step
        exec(cmds, regs, idx + offset, s)

      ["jnz", reg_val, step] ->
        offset = (regs[reg_val] == 0 && 1) || step
        exec(cmds, regs, idx + offset, s)
    end
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn instruction ->
      instruction
      |> String.split(" ")
      |> Enum.map(&sanitize/1)
    end)
    |> Enum.with_index()
    |> Map.new(fn {cmd, idx} -> {idx, cmd} end)
  end

  defp sanitize(reg) when reg in @tokens, do: reg
  defp sanitize(reg), do: String.to_integer(reg)
end
