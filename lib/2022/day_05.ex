defmodule AdventOfCode.Y2022.Day05 do
  @moduledoc """
  --- Day 5: Supply Stacks ---
  Problem Link: https://adventofcode.com/2022/day/5
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 5, false)

  def run(input \\ input()) do
    with input <- parse(input), do: {transfer(input), transfer(input, false)}
  end

  def parse(data \\ input()) do
    [stacks, moves] = String.split(data, "\n\n")
    {parse_stacks(stacks), parse_moves(moves)}
  end

  defp transfer({stack, moves}, single? \\ true) do
    topmost(
      Enum.reduce(moves, stack, fn {amount, source, target}, acc ->
        {m, u} = Enum.split(acc[source], amount)
        %{acc | source => u, target => ((single? && Enum.reverse(m)) || m) ++ acc[target]}
      end)
    )
  end

  defp parse_stacks(stacks) do
    stacks
    |> String.replace(~r"\[|\]", " ")
    |> Transformers.lines()
    |> Enum.map(fn row ->
      row
      |> String.split("   ")
      |> Enum.map(&String.trim/1)
      |> Enum.with_index(1)
    end)
    |> remove_header()
    |> Enum.group_by(&elem(&1, 1), &elem(&1, 0))
    |> Map.new(fn {k, v} -> {k, Enum.drop_while(v, &(&1 == ""))} end)
  end

  defp parse_moves(moves) do
    for move <- Transformers.lines(moves) do
      [_, q, x, y] = Regex.run(~r/move (\d+) from (\d+) to (\d+)/, move)
      {String.to_integer(q), String.to_integer(x), String.to_integer(y)}
    end
  end

  defp remove_header(xs),
    do: xs |> Enum.reverse() |> tl() |> Enum.reverse() |> List.flatten()

  defp topmost(xs), do: Enum.map_join(xs, fn {_, [x | _]} -> x end)
end
