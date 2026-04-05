defmodule AdventOfCode.Y2016.Day12 do
  @moduledoc """
  --- Day 12: Leonardo's Monorail ---
  Problem Link: https://adventofcode.com/2016/day/12
  Difficulty: s
  Tags: assembunny optimization
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 12)

  def run(input \\ input()) do
    instructions = parse(input)
    instr_tuple = List.to_tuple(instructions)
    size = tuple_size(instr_tuple)

    task_1 = Task.async(fn -> solve(instr_tuple, {0, 0, 0, 0}, 0, size) end)
    task_2 = Task.async(fn -> solve(instr_tuple, {0, 0, 1, 0}, 0, size) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      String.split(line, " ") |> Enum.map(&sanitize/1)
    end)
  end

  # Use atoms for registers to distinguish from constant integers
  defp sanitize(val) do
    case val do
      "a" ->
        :a

      "b" ->
        :b

      "c" ->
        :c

      "d" ->
        :d

      other ->
        case Integer.parse(other) do
          {n, ""} -> n
          _ -> other
        end
    end
  end

  defp solve(_, {a, _, _, _}, pc, size) when pc >= size, do: a

  defp solve(cmds, regs, pc, size) do
    case elem(cmds, pc) do
      ["inc", r1] ->
        # Loop Optimization: Add R2 to R1 and zero out R2
        if pc + 2 < size do
          case elem(cmds, pc + 1) do
            ["dec", r2] ->
              case elem(cmds, pc + 2) do
                ["jnz", ^r2, -2] ->
                  val = get_reg(regs, r1) + get_reg(regs, r2)
                  solve(cmds, put_reg(regs, r1, val) |> put_reg(r2, 0), pc + 3, size)

                _ ->
                  val = get_reg(regs, r1) + 1
                  solve(cmds, put_reg(regs, r1, val), pc + 1, size)
              end

            _ ->
              val = get_reg(regs, r1) + 1
              solve(cmds, put_reg(regs, r1, val), pc + 1, size)
          end
        else
          val = get_reg(regs, r1) + 1
          solve(cmds, put_reg(regs, r1, val), pc + 1, size)
        end

      ["dec", r1] ->
        # Loop Optimization: dec r1; inc r2; jnz r1 -2
        if pc + 2 < size do
          case elem(cmds, pc + 1) do
            ["inc", r2] ->
              case elem(cmds, pc + 2) do
                ["jnz", ^r1, -2] ->
                  val = get_reg(regs, r2) + get_reg(regs, r1)
                  solve(cmds, put_reg(regs, r2, val) |> put_reg(r1, 0), pc + 3, size)

                _ ->
                  val = get_reg(regs, r1) - 1
                  solve(cmds, put_reg(regs, r1, val), pc + 1, size)
              end

            _ ->
              val = get_reg(regs, r1) - 1
              solve(cmds, put_reg(regs, r1, val), pc + 1, size)
          end
        else
          val = get_reg(regs, r1) - 1
          solve(cmds, put_reg(regs, r1, val), pc + 1, size)
        end

      ["cpy", src, target] ->
        val = get_reg(regs, src)
        solve(cmds, put_reg(regs, target, val), pc + 1, size)

      ["jnz", src, step] ->
        val = get_reg(regs, src)
        new_pc = if val != 0, do: pc + step, else: pc + 1
        solve(cmds, regs, new_pc, size)
    end
  end

  # Helper functions for O(1) register access using pattern matching
  @compile {:inline, get_reg: 2, put_reg: 3}
  defp get_reg({a, _, _, _}, :a), do: a
  defp get_reg({_, b, _, _}, :b), do: b
  defp get_reg({_, _, c, _}, :c), do: c
  defp get_reg({_, _, _, d}, :d), do: d
  # It's a constant integer
  defp get_reg(_, val), do: val

  defp put_reg({_, b, c, d}, :a, v), do: {v, b, c, d}
  defp put_reg({a, _, c, d}, :b, v), do: {a, v, c, d}
  defp put_reg({a, b, _, d}, :c, v), do: {a, b, v, d}
  defp put_reg({a, b, c, _}, :d, v), do: {a, b, c, v}
end
