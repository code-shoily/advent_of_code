defmodule AdventOfCode.Y2020.Day08 do
  @moduledoc """
  --- Day 8: Handheld Halting ---
  Problem Link: https://adventofcode.com/2020/day/8
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 8)

  def run(input \\ input()) do
    input = input |> Transformers.lines() |> Enum.map(&parse_command/1)
    {elem(exec(input), 1), fix(input)}
  end

  defp exec(prog), do: exec(prog, 0, 0, %{})
  defp exec(_, cur, acc, hist) when is_map_key(hist, cur), do: {:cont, acc}
  defp exec(prog, cur, acc, _) when cur >= length(prog), do: {:halt, acc}

  defp exec(prog, cur, acc, hist) do
    case Enum.at(prog, cur) do
      {:acc, val} -> exec(prog, cur + 1, acc + val, Map.put(hist, cur, 0))
      {:nop, _} -> exec(prog, cur + 1, acc, Map.put(hist, cur, 0))
      {:jmp, val} -> exec(prog, cur + val, acc, Map.put(hist, cur, 0))
    end
  end

  defp parse_command(cmd) do
    ~r/(?<cmd>.+) (?<val>[+-]\d+)/
    |> Regex.named_captures(cmd)
    |> then(&{String.to_existing_atom(&1["cmd"]), String.to_integer(&1["val"])})
  end

  defp swaps(prog) do
    %{jmp: jmps, nop: nops} = jmp_nop_cur(prog)

    Kernel.++(
      Enum.map(jmps, fn i -> List.update_at(prog, i, &{:nop, elem(&1, 1)}) end),
      Enum.map(nops, fn i -> List.update_at(prog, i, &{:jmp, elem(&1, 1)}) end)
    )
  end

  defp jmp_nop_cur(prog) do
    prog
    |> Enum.with_index()
    |> Enum.reduce(%{jmp: [], nop: []}, fn
      {{:jmp, _}, cur}, acc -> %{acc | jmp: [cur | acc.jmp]}
      {{:nop, _}, cur}, acc -> %{acc | nop: [cur | acc.nop]}
      _, acc -> acc
    end)
  end

  defp fix(prog), do: Enum.reduce_while(swaps(prog), 0, fn p, _ -> exec(p) end)
end
