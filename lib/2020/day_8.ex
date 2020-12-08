defmodule AdventOfCode.Y2020.Day8 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/8
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 8

  def run_1, do: input!() |> process() |> exec() |> elem(1)
  def run_2, do: input!() |> process() |> fix()
  def run, do: {run_1(), run_2()}

  def process(input), do: Enum.map(String.split(input, "\n"), &parse/1)

  defp exec(prog), do: exec(prog, 0, 0, %{})
  defp exec(_, cur, acc, hist) when is_map_key(hist, cur), do: {:cont, acc}
  defp exec(prog, cur, acc, _) when cur >= length(prog), do: {:halt, acc}

  defp exec(prog, cur, acc, hist) do
    case Enum.at(prog, cur) do
      {:acc, val} -> exec(prog, cur + 1, acc + val, Map.put(hist, cur, true))
      {:nop, _} -> exec(prog, cur + 1, acc, Map.put(hist, cur, true))
      {:jmp, val} -> exec(prog, cur + val, acc, Map.put(hist, cur, true))
    end
  end

  defp parse(cmd) do
    [[_, cmd, val]] = Regex.scan(~r/(.+) ([+-]\d+)/, cmd)
    {String.to_existing_atom(cmd), String.to_integer(val)}
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
